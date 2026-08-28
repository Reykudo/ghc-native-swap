{-# LANGUAGE NumericUnderscores #-}

module HotSwap.Polling
  ( Candidate (..)
  , PollSettings (..)
  , Poller
  , defaultPollSettings
  , startPolling
  , stopPolling
  , waitPolling
  ) where

import Control.Concurrent
  ( ThreadId
  , forkFinally
  , killThread
  , threadDelay
  )
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , readMVar
  , tryPutMVar
  )
import Control.Exception
  ( AsyncException
  , SomeException
  , catchJust
  , fromException
  , throwIO
  )
import Control.Monad (void)
import HotSwap (HotSwap, swapHotSwap, waitRetirement)
import System.IO (hPutStrLn, stderr)

data Candidate = Candidate
  { candidateRevision :: !String
  , candidatePath :: !FilePath
  }
  deriving (Eq, Show)

data PollSettings = PollSettings
  { pollIntervalMicroseconds :: !Int
  , pollErrorHandler :: SomeException -> IO ()
  }

data Poller = Poller
  { pollerThread :: !ThreadId
  , pollerDone :: !(MVar (Either SomeException ()))
  }

defaultPollSettings :: PollSettings
defaultPollSettings =
  PollSettings
    { pollIntervalMicroseconds = 60_000_000
    , pollErrorHandler = hPutStrLn stderr . ("hot-swap poll failed: " <>) . show
    }

startPolling
  :: PollSettings
  -> HotSwap input output
  -> String
  -> (String -> IO (Maybe Candidate))
  -> IO Poller
startPolling settings hotSwap initialRevision fetch = do
  done <- newEmptyMVar
  thread <-
    forkFinally
      (loop initialRevision)
      (void . tryPutMVar done)
  pure (Poller thread done)
 where
  loop revision = do
    next <- catchSynchronous (fetch revision) reportNothing
    nextRevision <- case next of
      Nothing -> pure revision
      Just candidate -> do
        swapped <-
          catchSynchronous
            (do
                retirement <- swapHotSwap hotSwap (candidatePath candidate)
                waitRetirement retirement
                pure True
            )
            reportFalse
        pure
          ( if swapped
              then candidateRevision candidate
              else revision
          )
    threadDelay (pollIntervalMicroseconds settings)
    loop nextRevision

  reportNothing exception =
    pollErrorHandler settings exception >> pure Nothing

  reportFalse exception =
    pollErrorHandler settings exception >> pure False

stopPolling :: Poller -> IO ()
stopPolling poller = do
  killThread (pollerThread poller)
  _ <- readMVar (pollerDone poller)
  pure ()

waitPolling :: Poller -> IO ()
waitPolling poller =
  readMVar (pollerDone poller) >>= either throwIO pure

catchSynchronous :: IO value -> (SomeException -> IO value) -> IO value
catchSynchronous action handler =
  catchJust synchronousException action handler

synchronousException :: SomeException -> Maybe SomeException
synchronousException exception =
  case fromException exception :: Maybe AsyncException of
    Nothing -> Just exception
    Just _ -> Nothing
