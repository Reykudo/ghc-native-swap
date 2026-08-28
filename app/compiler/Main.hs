{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text qualified as Text
import GHC.NativeSwap.Compiler (CompilerConfig (..), makeCompilerConfig)
import GHC.NativeSwap.Compiler.Server
  ( ServerConfig (..)
  , compilerApplication
  )
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setHost
  , setPort
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  bind <- environment "GHC_NATIVE_SWAP_BIND" "127.0.0.1"
  port <- environmentRead "GHC_NATIVE_SWAP_PORT" 8080
  artifactRoot <- environment "GHC_NATIVE_SWAP_ARTIFACT_DIR" "artifacts"
  compiler <- environment "GHC_NATIVE_SWAP_GHC" "ghc"
  packageDatabases <- splitPackages <$> environment "GHC_NATIVE_SWAP_PACKAGE_DBS" ""
  packages <- splitPackages <$> environment "GHC_NATIVE_SWAP_PACKAGES" "base,ghc-native-swap"
  timeoutSeconds <- environmentRead "GHC_NATIVE_SWAP_COMPILE_TIMEOUT_SECONDS" 60
  sourceLimit <- environmentRead "GHC_NATIVE_SWAP_MAX_SOURCE_BYTES" 1_048_576
  concurrency <- environmentRead "GHC_NATIVE_SWAP_MAX_CONCURRENT_COMPILATIONS" 1
  compilerConfig <-
    makeCompilerConfig
      compiler
      artifactRoot
      packageDatabases
      packages
      (timeoutSeconds * 1_000_000)
  application <-
    compilerApplication
      ServerConfig
        { compilerConfig = compilerConfig
        , maxRequestBytes = sourceLimit
        , maxConcurrentCompilations = concurrency
        }
  putStrLn
    ( "ghc-native-swap-compiler listening on "
        <> bind
        <> ":"
        <> show port
        <> ", GHC "
        <> Text.unpack (compilerGhcVersion compilerConfig)
        <> ", packages="
        <> intercalate "," packages
    )
  runSettings
    (setHost (fromString bind) (setPort port defaultSettings))
    application

environment :: String -> String -> IO String
environment name fallback = fromMaybe fallback <$> lookupEnv name

environmentRead :: (Read value) => String -> value -> IO value
environmentRead name fallback = do
  raw <- lookupEnv name
  pure (fromMaybe fallback (raw >>= readMaybe))

splitPackages :: String -> [String]
splitPackages = filter (not . null) . map trim . splitOnComma

splitOnComma :: String -> [String]
splitOnComma [] = [""]
splitOnComma input =
  let (before, after) = break (== ',') input
  in  before : case after of
        [] -> []
        _ : rest -> splitOnComma rest

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
