{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Runtime
  ( runtimeTests
  ) where

import Control.Monad (forM)
import qualified Data.Text as Text
import HotSwap.Compiler (CompilerConfig)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.Process
  ( CreateProcess (env)
  , proc
  , readCreateProcessWithExitCode
  )
import System.Timeout (timeout)
import Test.Support
  ( RuntimeFixtures (..)
  , compileFixture
  , makeTestCompilerConfig
  , pluginSource
  , withTemporaryDirectory
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Read (readMaybe)

runtimeTests :: IO RuntimeFixtures -> TestTree
runtimeTests getFixtures =
  testGroup
    "runtime subprocesses"
    [ testCase "loads, swaps, closes, and unmaps" $ do
        fixtures <- getFixtures
        runChild 30 ["basic", fixtureV1 fixtures, fixtureV2 fixtures]
    , testCase "supports sequential loader lifecycles" $ do
        fixtures <- getFixtures
        runChild 30 ["sequential", fixtureV1 fixtures, fixtureV2 fixtures]
    , testCase "old in-flight call owns its generation" $ do
        fixtures <- getFixtures
        runChild
          30
          [ "in-flight"
          , fixtureBlockingV1 fixtures
          , fixtureBlockingV2 fixtures
          ]
    , testCase "caller cancellation releases its generation" $ do
        fixtures <- getFixtures
        runChild 30 ["cancel", fixtureBlockingV1 fixtures]
    , testCase "close waits for an in-flight call" $ do
        fixtures <- getFixtures
        runChild 30 ["close-in-flight", fixtureBlockingV1 fixtures]
    , testCase "wrong ABI magic preserves the current generation" $ do
        fixtures <- getFixtures
        runChild
          30
          [ "invalid-magic"
          , fixtureV1 fixtures
          , fixtureWrongMagic fixtures
          ]
    , testCase "wrong type preserves the current generation" $ do
        fixtures <- getFixtures
        runChild
          30
          [ "invalid-type"
          , fixtureV1 fixtures
          , fixtureWrongType fixtures
          ]
    , testCase "missing symbol preserves the current generation" $ do
        fixtures <- getFixtures
        runChild
          30
          [ "invalid-symbol"
          , fixtureV1 fixtures
          , fixtureMissingEntry fixtures
          ]
    , testCase "forces plugin output before releasing lease" $ do
        fixtures <- getFixtures
        runChild 30 ["strict-output", fixtureLazyOutput fixtures]
    , testCase "snapshot pins its generation until GC" $ do
        fixtures <- getFixtures
        runChild
          30
          [ "snapshot"
          , fixtureV1 fixtures
          , fixtureV2 fixtures
          ]
    , testCase "running snapshot survives finalizer race" $ do
        fixtures <- getFixtures
        runChild 30 ["snapshot-in-flight", fixtureBlockingV1 fixtures]
    , testCase "managed values support zero arguments" $ do
        fixtures <- getFixtures
        runChild 30 ["managed-zero", fixtureZeroArgument fixtures]
    , testCase "managed values support multiple arguments" $ do
        fixtures <- getFixtures
        runChild 30 ["managed-multiple", fixtureMultipleArguments fixtures]
    , testCase "managed partial application pins its generation" $ do
        fixtures <- getFixtures
        runChild 30 ["managed-partial", fixtureMultipleArguments fixtures]
    , testCase "managed results reach normal form before release" $ do
        fixtures <- getFixtures
        runChild 30 ["managed-strict-output", fixtureLazyOutput fixtures]
    , testCase "managed values support sequential shapes" $ do
        fixtures <- getFixtures
        runChild
          30
          [ "managed-combined"
          , fixtureZeroArgument fixtures
          , fixtureMultipleArguments fixtures
          ]
    , testCase "managed zero can precede a strict entry" $ do
        fixtures <- getFixtures
        runChild
          30
          [ "managed-zero-entry"
          , fixtureZeroArgument fixtures
          , fixtureV1 fixtures
          ]
    , testCase "poller installs a new generation" $ do
        fixtures <- getFixtures
        runChild 30 ["polling", fixtureV1 fixtures, fixtureV2 fixtures]
    , testCase "concurrent calls stay within one generation" $ do
        fixtures <- getFixtures
        runChild 60 ["concurrent", fixtureV1 fixtures]
    , testCase "concurrent stress reload does not crash" testStress
    ]

testStress :: IO ()
testStress =
  withTemporaryDirectory "hot-swap-stress" $ \root -> do
    generationCount <- stressGenerationCount
    compiler <- makeTestCompilerConfig root
    artifacts <-
      forM [1 .. generationCount] $ \generation ->
        compileStressGeneration compiler generation
    runChild 180 ("stress" : artifacts <> ["+RTS", "-N8", "-RTS"])
    runChild
      180
      ( "managed-sequential"
          : take 64 artifacts
            <> ["+RTS", "-N8", "-RTS"]
      )
    runChild 180 ("managed-stress" : artifacts <> ["+RTS", "-N8", "-RTS"])

compileStressGeneration :: CompilerConfig -> Int -> IO FilePath
compileStressGeneration compiler generation =
  compileFixture
    compiler
    (Text.pack ("stress-" <> show generation))
    (pluginSource (generation * 1_000))

stressGenerationCount :: IO Int
stressGenerationCount = do
  configured <- lookupEnv "HOT_SWAP_STRESS_GENERATIONS"
  pure (max 2 (maybe 100 id (configured >>= readMaybe)))

runChild :: Int -> [String] -> IO ()
runChild timeoutSeconds arguments = do
  executable <- getExecutablePath
  environment <- getEnvironment
  let process =
        (proc executable ("--native-child" : arguments))
          { env = Just environment
          }
  outcome <-
    timeout
      (timeoutSeconds * 1_000_000)
      (readCreateProcessWithExitCode process "")
  case outcome of
    Nothing -> assertFailure ("native child timed out: " <> unwords arguments)
    Just (ExitSuccess, _, _) -> pure ()
    Just (ExitFailure code, standardOutput, standardError) ->
      assertFailure
        ( "native child failed with exit code "
            <> show code
            <> "\nstdout:\n"
            <> standardOutput
            <> "\nstderr:\n"
            <> standardError
        )
