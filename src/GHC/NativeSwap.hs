{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.NativeSwap
  ( Dynamic
  , DynamicFunction
  , HotSwap
  , HotSwapError (..)
  , Retirement
  , UnloadSafe (..)
  , closeDynamic
  , closeHotSwap
  , invoke
  , newDynamic
  , newHotSwap
  , snapshotFunction
  , swapDynamic
  , swapHotSwap
  , forceUnloadSafe
  , waitRetirement
  , withDynamic
  , withHotSwap
  ) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , newMVar
  , readMVar
  , tryPutMVar
  , withMVar
  )
import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , check
  , modifyTVar'
  , newTVarIO
  , readTVar
  , registerDelay
  , throwSTM
  , writeTVar
  )
import Control.Exception
  ( SomeException
  , bracket
  , evaluate
  , finally
  , mask
  , onException
  , throwIO
  )
import Control.Monad (unless, void)
import Data.Foldable (traverse_)
import Data.Typeable (Typeable)
import Foreign.Concurrent qualified as Concurrent
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (nullPtr)
import GHC.NativeSwap.Error (HotSwapError (..))
import GHC.NativeSwap.Internal.ABI (AbiDescriptor, descriptorFor)
import GHC.NativeSwap.Internal.Native
  ( Native
  , acquireNativeCall
  , invokeNative
  , loadNative
  , nativeArtifactPath
  , readNative
  , releaseNativeCall
  , runPluginAction
  , unloadNative
  )
import GHC.NativeSwap.Plugin (Entry)
import GHC.NativeSwap.UnloadSafe
  ( UnloadSafe (..)
  , forceUnloadSafe
  )
import System.Mem (performMajorGC)

data Dynamic value = Dynamic
  { state :: !(TVar (RuntimeState value))
  , swapLock :: !(MVar ())
  , expectedDescriptor :: !AbiDescriptor
  }

type HotSwap input output = Dynamic (Entry input output)

data RuntimeState value
  = Open !(Generation value)
  | Closed

data Generation value = Generation
  { generationNative :: !(Native value)
  , generationActive :: !(TVar Int)
  }

newtype Retirement = Retirement (MVar (Either SomeException ()))

newDynamic
  :: forall value
   . (Typeable value)
  => FilePath
  -> IO (Dynamic value)
newDynamic artifactPath = do
  let descriptor = descriptorFor @value
  native <- loadNative descriptor artifactPath
  active <- newTVarIO 0
  currentState <- newTVarIO (Open (Generation native active))
  lock <- newMVar ()
  pure
    Dynamic
      { state = currentState
      , swapLock = lock
      , expectedDescriptor = descriptor
      }

newHotSwap
  :: forall input output
   . (Typeable input, Typeable output)
  => FilePath
  -> IO (HotSwap input output)
newHotSwap = newDynamic @(Entry input output)

withDynamic
  :: (Typeable value)
  => FilePath
  -> (Dynamic value -> IO result)
  -> IO result
withDynamic artifactPath = bracket (newDynamic artifactPath) closeDynamic

withHotSwap
  :: (Typeable input, Typeable output)
  => FilePath
  -> (HotSwap input output -> IO result)
  -> IO result
withHotSwap = withDynamic

invoke
  :: (UnloadSafe output)
  => HotSwap input output
  -> input
  -> IO output
invoke = invokeWith invokeNative

class DynamicFunction function where
  wrapDynamicFunction
    :: FilePath
    -> ForeignPtr ()
    -> function
    -> function

instance (UnloadSafe output) => DynamicFunction (IO output) where
  wrapDynamicFunction path token action =
    withForeignPtr token $ \_ ->
      runPluginAction path (action >>= evaluate . forceUnloadSafe)

instance (DynamicFunction output) => DynamicFunction (input -> output) where
  wrapDynamicFunction path token function input =
    wrapDynamicFunction path token (function input)

snapshotFunction
  :: (DynamicFunction function)
  => Dynamic function
  -> IO function
snapshotFunction dynamic = mask $ \_ -> do
  generation <- atomically (acquireGenerationPin dynamic)
  let active = generationActive generation
  value <-
    readNative (generationNative generation)
      `onException` atomically (releaseGenerationPin active)
  token <-
    Concurrent.newForeignPtr
      nullPtr
      (atomically (releaseGenerationPin active))
      `onException` atomically (releaseGenerationPin active)
  pure
    ( wrapDynamicFunction
        (nativeArtifactPath (generationNative generation))
        token
        value
    )

invokeWith
  :: (Native value -> input -> IO output)
  -> Dynamic value
  -> input
  -> IO output
invokeWith run dynamic input = mask $ \restore -> do
  generation <- atomically (acquireGeneration dynamic)
  restore (run (generationNative generation) input)
    `finally` atomically (releaseGeneration generation)

swapDynamic :: Dynamic value -> FilePath -> IO Retirement
swapDynamic dynamic artifactPath =
  withMVar (swapLock dynamic) $ \() -> mask $ \_ -> do
    atomically (assertOpen dynamic)
    native <- loadNative (expectedDescriptor dynamic) artifactPath
    active <- newTVarIO 0
    oldGeneration <- atomically $ do
      current <- readTVar (state dynamic)
      case current of
        Closed -> throwSTMHotSwapClosed
        Open old -> do
          writeTVar (state dynamic) (Open (Generation native active))
          pure old
    startRetirement oldGeneration

swapHotSwap :: HotSwap input output -> FilePath -> IO Retirement
swapHotSwap = swapDynamic

closeDynamic :: Dynamic value -> IO ()
closeDynamic dynamic = do
  retirement <- withMVar (swapLock dynamic) $ \() -> mask $ \_ -> do
    previous <- atomically $ do
      current <- readTVar (state dynamic)
      case current of
        Closed -> pure Nothing
        Open generation -> do
          writeTVar (state dynamic) Closed
          pure (Just generation)
    traverse startRetirement previous
  traverse_ waitRetirement retirement

closeHotSwap :: HotSwap input output -> IO ()
closeHotSwap = closeDynamic

waitRetirement :: Retirement -> IO ()
waitRetirement (Retirement done) =
  readMVar done >>= either throwIO pure

acquireGeneration :: Dynamic value -> STM (Generation value)
acquireGeneration dynamic = do
  current <- readTVar (state dynamic)
  case current of
    Closed -> throwSTMHotSwapClosed
    Open generation -> do
      acquireNativeCall
      modifyTVar' (generationActive generation) (+ 1)
      pure generation

releaseGeneration :: Generation value -> STM ()
releaseGeneration generation =
  modifyTVar' (generationActive generation) (subtract 1)
    >> releaseNativeCall

acquireGenerationPin :: Dynamic value -> STM (Generation value)
acquireGenerationPin dynamic = do
  current <- readTVar (state dynamic)
  case current of
    Closed -> throwSTMHotSwapClosed
    Open generation -> do
      modifyTVar' (generationActive generation) (+ 1)
      pure generation

releaseGenerationPin :: TVar Int -> STM ()
releaseGenerationPin active = do
  modifyTVar' active (subtract 1)

startRetirement :: Generation value -> IO Retirement
startRetirement generation = do
  done <- newEmptyMVar
  void $ forkFinally (retireGeneration generation) (putResult done)
  pure (Retirement done)

retireGeneration :: Generation value -> IO ()
retireGeneration generation = do
  waitUntilInactive generation
  unloadNative (generationNative generation)

waitUntilInactive :: Generation value -> IO ()
waitUntilInactive generation = do
  previous <- atomically (readTVar (generationActive generation))
  unless (previous == 0) $ do
    performMajorGC
    wakeup <- registerDelay 1_000_000
    atomically $ do
      active <- readTVar (generationActive generation)
      expired <- readTVar wakeup
      check (active < previous || expired)
    waitUntilInactive generation

assertOpen :: Dynamic value -> STM ()
assertOpen dynamic = do
  current <- readTVar (state dynamic)
  case current of
    Closed -> throwSTMHotSwapClosed
    Open _ -> pure ()

throwSTMHotSwapClosed :: STM value
throwSTMHotSwapClosed = throwSTM HotSwapClosed

putResult
  :: MVar (Either SomeException ())
  -> Either SomeException ()
  -> IO ()
putResult done result = void (tryPutMVar done result)
