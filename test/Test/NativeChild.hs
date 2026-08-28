{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.NativeChild
  ( runNativeChild
  ) where

import Control.Concurrent
  ( forkFinally
  , killThread
  , threadDelay
  , yield
  )
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  , tryPutMVar
  , tryReadMVar
  )
import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Exception
  ( SomeException
  , try
  )
import Control.Monad
  ( forM_
  , replicateM
  , unless
  , void
  , when
  )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Foldable (traverse_)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import HotSwap
  ( Dynamic
  , HotSwap
  , HotSwapError (..)
  , Retirement
  , closeDynamic
  , closeHotSwap
  , invoke
  , newDynamic
  , newHotSwap
  , snapshotFunction
  , swapHotSwap
  , waitRetirement
  )
import HotSwap.Polling
  ( Candidate (..)
  , PollSettings (..)
  , startPolling
  , stopPolling
  )
import System.Directory (canonicalizePath)
import System.Mem (performMajorGC)
import System.Timeout (timeout)
import Numeric (readHex)

runNativeChild :: [String] -> IO ()
runNativeChild arguments =
  case arguments of
    ["basic", versionOne, versionTwo] -> basicScenario versionOne versionTwo
    ["sequential", versionOne, versionTwo] ->
      sequentialScenario versionOne versionTwo
    ["in-flight", versionOne, versionTwo] ->
      inFlightScenario versionOne versionTwo
    ["cancel", blocking] -> cancellationScenario blocking
    ["close-in-flight", blocking] -> closeInFlightScenario blocking
    ["invalid-magic", valid, wrongMagic] ->
      invalidMagicScenario valid wrongMagic
    ["invalid-type", valid, wrongType] ->
      invalidTypeScenario valid wrongType
    ["invalid-symbol", valid, missingEntry] ->
      invalidSymbolScenario valid missingEntry
    ["strict-output", lazyOutput] -> strictOutputScenario lazyOutput
    ["snapshot", versionOne, versionTwo] ->
      snapshotScenario versionOne versionTwo
    ["snapshot-in-flight", blocking] ->
      snapshotInFlightScenario blocking
    ["managed-zero", zeroArgument] -> managedZeroScenario zeroArgument
    ["managed-multiple", multipleArguments] ->
      managedMultipleScenario multipleArguments
    ["managed-partial", multipleArguments] ->
      managedPartialScenario multipleArguments
    ["managed-strict-output", lazyOutput] ->
      managedStrictOutputScenario lazyOutput
    ["managed-combined", zeroArgument, multipleArguments] ->
      managedZeroScenario zeroArgument >> managedMultipleScenario multipleArguments
    ["managed-zero-entry", zeroArgument, entry] ->
      managedZeroScenario zeroArgument >> singleEntryScenario entry
    ["polling", versionOne, versionTwo] -> pollingScenario versionOne versionTwo
    ["concurrent", artifact] -> concurrentScenario artifact
    "stress" : artifacts -> stressScenario artifacts
    "managed-stress" : artifacts -> managedStressScenario artifacts
    "managed-sequential" : artifacts -> managedSequentialScenario artifacts
    _ -> fail ("invalid native child arguments: " <> show arguments)

basicScenario :: FilePath -> FilePath -> IO ()
basicScenario versionOne versionTwo = do
  runtime <- newHotSwap versionOne :: IO (HotSwap Int Int)
  invoke runtime 1 >>= expectEqual "v1 result" 101
  pluginFailure <- try (invoke runtime 13) :: IO (Either HotSwapError Int)
  case pluginFailure of
    Left (PluginInvocationFailed _ message)
      | "plugin exception" `isInfixOf` message -> pure ()
    other -> fail ("expected transported plugin exception, got " <> show other)
  invoke runtime 2 >>= expectEqual "lease released after exception" 102
  retirement <- swapHotSwap runtime versionTwo
  invoke runtime 1 >>= expectEqual "v2 result" 201
  waitRetirement retirement
  assertUnmapped versionOne
  invoke runtime 2 >>= expectEqual "v2 survives v1 unload" 202
  closeHotSwap runtime
  assertUnmapped versionTwo
  closed <- try (invoke runtime 1) :: IO (Either HotSwapError Int)
  case closed of
    Left HotSwapClosed -> pure ()
    _ -> fail ("expected HotSwapClosed, got " <> show closed)

sequentialScenario :: FilePath -> FilePath -> IO ()
sequentialScenario versionOne versionTwo = do
  first <- newHotSwap versionOne :: IO (HotSwap Int Int)
  invoke first 1 >>= expectEqual "first lifecycle" 101
  closeHotSwap first
  assertUnmapped versionOne
  second <- newHotSwap versionTwo :: IO (HotSwap Int Int)
  invoke second 1 >>= expectEqual "second lifecycle" 201
  closeHotSwap second
  assertUnmapped versionTwo

singleEntryScenario :: FilePath -> IO ()
singleEntryScenario artifact = do
  runtime <- newHotSwap artifact :: IO (HotSwap Int Int)
  invoke runtime 1 >>= expectEqual "entry after managed zero" 101
  closeHotSwap runtime
  assertUnmapped artifact

cancellationScenario :: FilePath -> IO ()
cancellationScenario artifact = do
  runtime <-
    newHotSwap artifact
      :: IO (HotSwap (MVar (), MVar (), Int) Int)
  started <- newEmptyMVar
  release <- newEmptyMVar
  done <- newEmptyMVar
  caller <-
    forkFinally
      (invoke runtime (started, release, 1))
      (putMVar done)
  takeMVar started
  killThread caller
  cancelled <- timeout 5_000_000 (takeMVar done)
  case cancelled of
    Just (Left _) -> pure ()
    other -> fail ("expected cancelled invocation, got " <> show other)
  nextStarted <- newEmptyMVar
  nextRelease <- newEmptyMVar
  putMVar nextRelease ()
  invoke runtime (nextStarted, nextRelease, 1)
    >>= expectEqual "generation survives cancellation" 101
  takeMVar nextStarted
  closeHotSwap runtime
  assertUnmapped artifact

closeInFlightScenario :: FilePath -> IO ()
closeInFlightScenario artifact = do
  runtime <-
    newHotSwap artifact
      :: IO (HotSwap (MVar (), MVar (), Int) Int)
  started <- newEmptyMVar
  release <- newEmptyMVar
  callDone <- newEmptyMVar
  void $
    forkFinally
      (invoke runtime (started, release, 1))
      (putMVar callDone)
  takeMVar started
  closeDone <- newEmptyMVar
  void $ forkFinally (closeHotSwap runtime) (putMVar closeDone)
  early <- timeout 20_000 (takeMVar closeDone)
  case early of
    Nothing -> pure ()
    Just result -> fail ("close finished while call was active: " <> show result)
  putMVar release ()
  takeMVar callDone
    >>= either (fail . show) (expectEqual "in-flight close result" 101)
  takeMVar closeDone >>= either (fail . show) pure
  assertUnmapped artifact

invalidMagicScenario :: FilePath -> FilePath -> IO ()
invalidMagicScenario valid wrongMagic = do
  runtime <- newHotSwap valid :: IO (HotSwap Int Int)
  wrong <- try (swapHotSwap runtime wrongMagic) :: IO (Either HotSwapError Retirement)
  case wrong of
    Left (NativeSymbolMissing _ _) -> pure ()
    other -> fail ("expected typed symbol rejection, got " <> showHotSwapResult other)
  invoke runtime 1 >>= expectEqual "old generation retained" 101
  assertUnmapped wrongMagic
  closeHotSwap runtime

inFlightScenario :: FilePath -> FilePath -> IO ()
inFlightScenario versionOne versionTwo = do
  runtime <-
    newHotSwap versionOne
      :: IO (HotSwap (MVar (), MVar (), Int) Int)
  started <- newEmptyMVar
  release <- newEmptyMVar
  oldDone <- newEmptyMVar
  void $
    forkFinally
      (invoke runtime (started, release, 1))
      (putMVar oldDone)
  takeMVar started
  retirement <- swapHotSwap runtime versionTwo
  early <- timeout 20_000 (waitRetirement retirement)
  expectEqual "old generation still leased" Nothing early
  newStarted <- newEmptyMVar
  newRelease <- newEmptyMVar
  putMVar newRelease ()
  invoke runtime (newStarted, newRelease, 1)
    >>= expectEqual "new generation is already current" 201
  takeMVar newStarted
  putMVar release ()
  oldResult <- takeMVar oldDone >>= either (fail . show) pure
  expectEqual "old request completed on v1" 101 oldResult
  waitRetirement retirement
  assertUnmapped versionOne
  closeHotSwap runtime

invalidTypeScenario :: FilePath -> FilePath -> IO ()
invalidTypeScenario valid wrongType = do
  runtime <- newHotSwap valid :: IO (HotSwap Int Int)
  wrong <- try (swapHotSwap runtime wrongType) :: IO (Either HotSwapError Retirement)
  case wrong of
    Left (NativeSymbolMissing _ _) -> pure ()
    other -> fail ("expected typed symbol rejection, got " <> showHotSwapResult other)
  invoke runtime 1 >>= expectEqual "old generation retained" 101
  assertUnmapped wrongType
  closeHotSwap runtime

invalidSymbolScenario :: FilePath -> FilePath -> IO ()
invalidSymbolScenario valid missingEntry = do
  runtime <- newHotSwap valid :: IO (HotSwap Int Int)
  missing <- try (swapHotSwap runtime missingEntry) :: IO (Either HotSwapError Retirement)
  case missing of
    Left (NativeSymbolMissing _ _) -> pure ()
    other -> fail ("expected missing symbol, got " <> showHotSwapResult other)
  invoke runtime 1 >>= expectEqual "old generation retained" 101
  assertUnmapped missingEntry
  closeHotSwap runtime

strictOutputScenario :: FilePath -> IO ()
strictOutputScenario lazyOutput = do
  runtime <- newHotSwap lazyOutput :: IO (HotSwap Int [Int])
  result <- try (invoke runtime 1) :: IO (Either HotSwapError [Int])
  case result of
    Left (PluginInvocationFailed _ message)
      | "latent plugin thunk" `isInfixOf` message -> pure ()
    other -> fail ("expected forced plugin exception, got " <> show other)
  closeHotSwap runtime
  assertUnmapped lazyOutput

snapshotScenario :: FilePath -> FilePath -> IO ()
snapshotScenario versionOne versionTwo = do
  runtime <- newHotSwap versionOne :: IO (HotSwap Int Int)
  entryRef <- newIORef Nothing
  snapshotFunction runtime >>= writeIORef entryRef . Just
  callSnapshot entryRef 1 >>= expectEqual "v1 snapshot" 101
  retirement <- swapHotSwap runtime versionTwo
  earlyRetirement <- timeout 20_000 (waitRetirement retirement)
  expectEqual "v1 retained by snapshot" Nothing earlyRetirement
  callSnapshot entryRef 2 >>= expectEqual "v1 snapshot after swap" 102
  snapshotFunction runtime >>= writeIORef entryRef . Just
  performMajorGC
  waitRetirement retirement
  assertUnmapped versionOne
  callSnapshot entryRef 1 >>= expectEqual "v2 snapshot" 201
  closeDone <- newEmptyMVar
  void $ forkFinally (closeHotSwap runtime) (putMVar closeDone)
  earlyClose <- timeout 20_000 (takeMVar closeDone)
  case earlyClose of
    Nothing -> pure ()
    Just result -> fail ("close ignored a reachable snapshot: " <> show result)
  callSnapshot entryRef 2 >>= expectEqual "v2 snapshot during close" 202
  writeIORef entryRef Nothing
  performMajorGC
  takeMVar closeDone >>= either (fail . show) pure
  assertUnmapped versionTwo

snapshotInFlightScenario :: FilePath -> IO ()
snapshotInFlightScenario artifact = do
  runtime <-
    newHotSwap artifact
      :: IO (HotSwap (MVar (), MVar (), Int) Int)
  entryRef <- newIORef Nothing
  snapshotFunction runtime >>= writeIORef entryRef . Just
  started <- newEmptyMVar
  release <- newEmptyMVar
  callDone <- newEmptyMVar
  void $
    forkFinally
      (callSnapshot entryRef (started, release, 1))
      (putMVar callDone)
  takeMVar started
  closeDone <- newEmptyMVar
  void $ forkFinally (closeHotSwap runtime) (putMVar closeDone)
  writeIORef entryRef Nothing
  performMajorGC
  earlyClose <- timeout 20_000 (takeMVar closeDone)
  case earlyClose of
    Nothing -> pure ()
    Just result -> fail ("close finished while snapshot call was active: " <> show result)
  putMVar release ()
  takeMVar callDone
    >>= either (fail . show) (expectEqual "snapshot call result" 101)
  performMajorGC
  takeMVar closeDone >>= either (fail . show) pure
  assertUnmapped artifact

callSnapshot
  :: IORef (Maybe (input -> IO output))
  -> input
  -> IO output
callSnapshot entryRef input = do
  current <- readIORef entryRef
  function <- maybe (fail "snapshot entry is absent") pure current
  function input

managedZeroScenario :: FilePath -> IO ()
managedZeroScenario zeroArgument = do
  zeroRuntime <- newDynamic zeroArgument :: IO (Dynamic (IO Int))
  ranges <- mappedRanges zeroArgument
  useZeroArgument zeroRuntime >>= expectEqual "zero-argument result" 77
  performMajorGC
  closeDynamic zeroRuntime
  assertUnmapped zeroArgument
  assertGuarded ranges

managedMultipleScenario :: FilePath -> IO ()
managedMultipleScenario multipleArguments = do
  multipleRuntime <-
    newDynamic multipleArguments
      :: IO (Dynamic (Int -> Int -> IO Int))
  ranges <- mappedRanges multipleArguments
  useMultipleArguments multipleRuntime
    >>= expectEqual "multiple-argument result" 430
  performMajorGC
  closeDynamic multipleRuntime
  assertUnmapped multipleArguments
  assertGuarded ranges

managedPartialScenario :: FilePath -> IO ()
managedPartialScenario multipleArguments = do
  runtime <-
    newDynamic multipleArguments
      :: IO (Dynamic (Int -> Int -> IO Int))
  partialRef <- newIORef Nothing
  snapshotPartial runtime 10 >>= writeIORef partialRef . Just
  performMajorGC
  closeDone <- newEmptyMVar
  void $ forkFinally (closeDynamic runtime) (putMVar closeDone)
  earlyClose <- timeout 20_000 (takeMVar closeDone)
  case earlyClose of
    Nothing -> pure ()
    Just result -> fail ("close ignored a reachable partial application: " <> show result)
  callSnapshot partialRef 20
    >>= expectEqual "partial application result" 430
  writeIORef partialRef Nothing
  performMajorGC
  takeMVar closeDone >>= either (fail . show) pure
  assertUnmapped multipleArguments

managedStrictOutputScenario :: FilePath -> IO ()
managedStrictOutputScenario lazyOutput = do
  runtime <- newDynamic lazyOutput :: IO (Dynamic (Int -> IO [Int]))
  result <- try (callManagedList runtime 1) :: IO (Either HotSwapError [Int])
  case result of
    Left (PluginInvocationFailed _ message)
      | "latent plugin thunk" `isInfixOf` message -> pure ()
    other -> fail ("expected forced managed exception, got " <> show other)
  performMajorGC
  closeDynamic runtime
  assertUnmapped lazyOutput

useZeroArgument :: Dynamic (IO Int) -> IO Int
useZeroArgument runtime = snapshotFunction runtime >>= id

useMultipleArguments :: Dynamic (Int -> Int -> IO Int) -> IO Int
useMultipleArguments runtime = do
  function <- snapshotFunction runtime
  function 10 20

snapshotPartial
  :: Dynamic (Int -> Int -> IO Int)
  -> Int
  -> IO (Int -> IO Int)
snapshotPartial runtime first = ($ first) <$> snapshotFunction runtime

callManagedList :: Dynamic (Int -> IO [Int]) -> Int -> IO [Int]
callManagedList runtime input = snapshotFunction runtime >>= ($ input)

pollingScenario :: FilePath -> FilePath -> IO ()
pollingScenario versionOne versionTwo = do
  runtime <- newHotSwap versionOne :: IO (HotSwap Int Int)
  supplied <- newIORef False
  poller <-
    startPolling
      PollSettings
        { pollIntervalMicroseconds = 1_000
        , pollErrorHandler = fail . show
        }
      runtime
      "v1"
      (\revision -> do
          alreadySupplied <- readIORef supplied
          if revision == "v1" && not alreadySupplied
            then do
              writeIORef supplied True
              pure (Just (Candidate "v2" versionTwo))
            else pure Nothing
      )
  installed <- timeout 5_000_000 (waitForResult runtime 201)
  expectEqual "poller installed v2" (Just ()) installed
  stopPolling poller
  closeHotSwap runtime
  assertUnmapped versionOne
  assertUnmapped versionTwo

concurrentScenario :: FilePath -> IO ()
concurrentScenario artifact = do
  runtime <- newHotSwap artifact :: IO (HotSwap Int Int)
  failures <- newEmptyMVar
  doneSignals <- replicateM 8 newEmptyMVar
  forM_ doneSignals $ \done ->
    void $
      forkFinally
        (forM_ [1 .. 20_000 :: Int] $ \input -> do
            let value = if input == 13 then 14 else input
            output <- invoke runtime value
            unless (output == value + 100) $
              recordFailure failures ("invalid concurrent output: " <> show output)
        )
        (\result -> do
            case result of
              Left exception -> recordFailure failures (show exception)
              Right () -> pure ()
            putMVar done ()
        )
  traverse_ takeMVar doneSignals
  pendingFailure <- tryReadMVar failures
  case pendingFailure of
    Nothing -> pure ()
    Just message -> fail message
  closeHotSwap runtime
  assertUnmapped artifact

stressScenario :: [FilePath] -> IO ()
stressScenario [] = fail "stress requires artifacts"
stressScenario artifacts@(first : rest) = do
  runtime <- newHotSwap first :: IO (HotSwap Int Int)
  stopping <- newTVarIO False
  failures <- newEmptyMVar
  doneSignals <- replicateM 8 newEmptyMVar
  forM_ doneSignals $ \done ->
    void $
      forkFinally
        (callerLoop runtime stopping failures (length artifacts))
        (\result -> do
            case result of
              Left exception -> recordFailure failures (show exception)
              Right () -> pure ()
            putMVar done ()
        )
  forM_ (zip [2 ..] rest) $ \(generation, artifact) -> do
    retirement <- swapHotSwap runtime artifact
    invoke runtime 1
      >>= expectEqual
        ("generation " <> show generation)
        (generation * 1_000 + 1)
    waitRetirement retirement
  atomically (writeTVar stopping True)
  traverse_ takeMVar doneSignals
  pendingFailure <- tryReadMVar failures
  case pendingFailure of
    Nothing -> pure ()
    Just message -> fail message
  closeHotSwap runtime
  assertAllUnmapped artifacts

managedStressScenario :: [FilePath] -> IO ()
managedStressScenario [] = fail "managed stress requires artifacts"
managedStressScenario artifacts@(first : rest) = do
  runtime <- newHotSwap first :: IO (HotSwap Int Int)
  entryRef <- newIORef Nothing
  snapshotFunction runtime >>= writeIORef entryRef . Just
  stopping <- newTVarIO False
  failures <- newEmptyMVar
  doneSignals <- replicateM 8 newEmptyMVar
  forM_ doneSignals $ \done ->
    void $
      forkFinally
        (managedCallerLoop entryRef stopping failures (length artifacts))
        (\result -> do
            case result of
              Left exception -> recordFailure failures (show exception)
              Right () -> pure ()
            putMVar done ()
        )
  forM_ (zip [2 ..] rest) $ \(generation, artifact) -> do
    retirement <- swapHotSwap runtime artifact
    snapshotFunction runtime >>= writeIORef entryRef . Just
    waitWithGC (waitRetirement retirement)
    callSnapshot entryRef 1
      >>= expectEqual
        ("managed generation " <> show generation)
        (generation * 1_000 + 1)
  atomically (writeTVar stopping True)
  traverse_ takeMVar doneSignals
  pendingFailure <- tryReadMVar failures
  case pendingFailure of
    Nothing -> pure ()
    Just message -> fail message
  writeIORef entryRef Nothing
  waitWithGC (closeHotSwap runtime)
  assertAllUnmapped artifacts

managedSequentialScenario :: [FilePath] -> IO ()
managedSequentialScenario [] = fail "managed sequential stress requires artifacts"
managedSequentialScenario artifacts =
  forM_ (zip [1 ..] artifacts) $ \(generation, artifact) -> do
    runtime <- newHotSwap artifact :: IO (HotSwap Int Int)
    callManaged runtime 1
      >>= expectEqual
        ("managed sequential generation " <> show generation)
        (generation * 1_000 + 1)
    performMajorGC
    closeHotSwap runtime
    assertUnmapped artifact

callManaged :: HotSwap Int Int -> Int -> IO Int
callManaged runtime input = snapshotFunction runtime >>= ($ input)

managedCallerLoop
  :: IORef (Maybe (Int -> IO Int))
  -> TVar Bool
  -> MVar String
  -> Int
  -> IO ()
managedCallerLoop entryRef stopping failures generationCount = loop 0
 where
  loop input = do
    shouldStop <- atomically (readTVar stopping)
    unless shouldStop $ do
      result <- try (callSnapshot entryRef input) :: IO (Either SomeException Int)
      case result of
        Left exception -> recordFailure failures (show exception)
        Right output -> do
          let offset = output - input
          unless (offset >= 1_000 && offset <= generationCount * 1_000) $
            recordFailure failures ("invalid managed stress output: " <> show output)
      let nextInput = if input + 1 == 13 then 14 else input + 1
      yield
      loop nextInput

waitWithGC :: IO () -> IO ()
waitWithGC action = do
  done <- newEmptyMVar
  void $ forkFinally action (putMVar done)
  loop done
 where
  loop done = do
    performMajorGC
    outcome <- timeout 10_000 (takeMVar done)
    case outcome of
      Nothing -> loop done
      Just result -> either (fail . show) pure result

callerLoop
  :: HotSwap Int Int
  -> TVar Bool
  -> MVar String
  -> Int
  -> IO ()
callerLoop runtime stopping failures generationCount = loop 0
 where
  loop input = do
    shouldStop <- atomically (readTVar stopping)
    unless shouldStop $ do
      result <- try (invoke runtime input) :: IO (Either SomeException Int)
      case result of
        Left exception -> recordFailure failures (show exception)
        Right output -> do
          let offset = output - input
          unless (offset >= 1_000 && offset <= generationCount * 1_000) $
            recordFailure failures ("invalid stress output: " <> show output)
      let nextInput = if input + 1 == 13 then 14 else input + 1
      loop nextInput

waitForResult :: HotSwap Int Int -> Int -> IO ()
waitForResult runtime expected = do
  actual <- invoke runtime 1
  if actual == expected
    then pure ()
    else threadDelay 1_000 >> waitForResult runtime expected

assertUnmapped :: FilePath -> IO ()
assertUnmapped path = assertAllUnmapped [path]

assertAllUnmapped :: [FilePath] -> IO ()
assertAllUnmapped paths = do
  absolutePaths <- traverse canonicalizePath paths
  performMajorGC
  threadDelay 1_000
  mappings <- readFile "/proc/self/maps"
  forM_ absolutePaths $ \absolutePath ->
    when (absolutePath `isInfixOf` mappings) $
      fail ("retired artifact remains mapped: " <> absolutePath)

data Mapping = Mapping !Integer !Integer !String !String

mappedRanges :: FilePath -> IO [Mapping]
mappedRanges path = do
  absolutePath <- canonicalizePath path
  mappings <- mapMaybe parseMapping . lines <$> readFile "/proc/self/maps"
  let artifactRanges =
        filter (\(Mapping _ _ _ source) -> absolutePath `isInfixOf` source) mappings
  if null artifactRanges
    then fail ("artifact has no mappings: " <> absolutePath)
    else pure artifactRanges

assertGuarded :: [Mapping] -> IO ()
assertGuarded retired = do
  current <- mapMaybe parseMapping . lines <$> readFile "/proc/self/maps"
  forM_ retired $ \(Mapping start end _ _) ->
    unless
      ( any
          (\(Mapping currentStart currentEnd permissions _) ->
              permissions == "---p"
                && currentStart <= start
                && currentEnd >= end
          )
          current
      )
      (fail ("retired range is not guarded: " <> showHexRange start end))

parseMapping :: String -> Maybe Mapping
parseMapping mapping =
  case words mapping of
    address : permissions : rest -> do
      (startText, '-' : endText) <- Just (break (== '-') address)
      start <- parseHexInteger startText
      end <- parseHexInteger endText
      pure (Mapping start end permissions (unwords rest))
    _ -> Nothing

parseHexInteger :: String -> Maybe Integer
parseHexInteger value =
  case readHex value of
    [(parsed, "")] -> Just parsed
    _ -> Nothing

showHexRange :: Integer -> Integer -> String
showHexRange start end = show start <> "-" <> show end

expectEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
expectEqual label expected actual =
  unless (actual == expected) $
    fail
      ( label
          <> ": expected "
          <> show expected
          <> ", got "
          <> show actual
      )

showHotSwapResult :: Either HotSwapError retirement -> String
showHotSwapResult (Left exception) = show exception
showHotSwapResult (Right _) = "successful swap"

recordFailure :: MVar String -> String -> IO ()
recordFailure failures message = void (tryPutMVar failures message)
