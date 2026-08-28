module Main (main) where

import System.Environment (getArgs)
import Test.Compiler (compilerTests)
import Test.NativeChild (runNativeChild)
import Test.Runtime (runtimeTests)
import Test.Support
  ( buildRuntimeFixtures
  , destroyRuntimeFixtures
  )
import Test.Tasty (defaultMain, localOption, testGroup, withResource)
import Test.Tasty.Runners (NumThreads (..))

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    "--native-child" : childArguments -> runNativeChild childArguments
    _ ->
      defaultMain
        ( localOption (NumThreads 1) $
            testGroup
              "ghc-native-swap"
              [ compilerTests
              , withResource
                  buildRuntimeFixtures
                  destroyRuntimeFixtures
                  runtimeTests
              ]
        )
