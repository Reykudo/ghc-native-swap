{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler
  ( compilerTests
  ) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (isInfixOf)
import Data.Text qualified as Text
import GHC.NativeSwap
  ( HotSwap
  , closeHotSwap
  , invoke
  , newHotSwap
  )
import GHC.NativeSwap.Compiler
  ( CompiledArtifact (compiledPath)
  , CompilerConfig (..)
  , compileModule
  , validModuleName
  , validSlot
  )
import GHC.NativeSwap.Compiler.Client
  ( newCompilerClient
  , pollCompilerArtifact
  )
import GHC.NativeSwap.Compiler.Protocol
  ( ArtifactManifest (..)
  , CompileRequest (..)
  )
import GHC.NativeSwap.Compiler.Server
  ( ServerConfig (..)
  , compilerApplication
  )
import GHC.NativeSwap.Polling (Candidate (..))
import Network.HTTP.Client
  ( Manager
  , Request (method, requestBody, requestHeaders)
  , RequestBody (RequestBodyLBS)
  , Response
  , defaultManagerSettings
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types
  ( hContentType
  , methodPut
  , status200
  , status201
  , status413
  , status422
  )
import Network.Wai.Handler.Warp (Port, testWithApplication)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Support
  ( invalidSource
  , makeTestCompilerConfig
  , pluginSource
  , withTemporaryDirectory
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  , testCase
  )

compilerTests :: TestTree
compilerTests =
  testGroup
    "compiler"
    [ testCase "validates public names" testNames
    , testCase "rejects function results at the unload boundary" testFunctionResult
    , testCase "publishes, downloads, and preserves last good revision" testHttpFlow
    , testCase "resolves a qualified invoke closure" testQualifiedResolver
    , testCase "rejects oversized request bodies" testRequestLimit
    ]

testNames :: IO ()
testNames = do
  assertBool "simple slot" (validSlot "rules")
  assertBool "hyphenated slot" (validSlot "rules-prod_2")
  assertBool "path traversal slot" (not (validSlot "../rules"))
  assertBool "qualified module" (validModuleName "Company.Rules.Plugin")
  assertBool "generated module is reserved" (not (validModuleName "GHCNativeSwapGenerated"))
  assertBool "lowercase module" (not (validModuleName "plugin"))
  assertBool "empty segment" (not (validModuleName "Plugin..Rules"))

testFunctionResult :: IO ()
testFunctionResult =
  withTemporaryDirectory "ghc-native-swap-function-result" $ \root -> do
    compiler <- makeTestCompilerConfig root
    let sourcePath = root <> "/FunctionResult.hs"
        arguments =
          [ "-v0"
          , "-fno-code"
          , "-hide-all-packages"
          ]
            <> concatMap (\database -> ["-package-db", database]) (packageDatabases compiler)
            <> concatMap (\packageId -> ["-package-id", packageId]) (exposedPackages compiler)
            <> [sourcePath]
        source =
          unlines
            [ "module FunctionResult where"
            , "import GHC.NativeSwap (forceUnloadSafe)"
            , "bad :: Int -> Int"
            , "bad = forceUnloadSafe (+ 1)"
            ]
    writeFile sourcePath source
    (exitCode, standardOutput, standardError) <-
      readCreateProcessWithExitCode
        (proc (ghcExecutable compiler) arguments)
        ""
    case exitCode of
      ExitFailure _ ->
        assertBool
          (standardOutput <> standardError)
          ("A function result is not UnloadSafe" `isInfixOf` (standardOutput <> standardError))
      ExitSuccess -> assertFailure "a function result unexpectedly satisfied UnloadSafe"

testHttpFlow :: IO ()
testHttpFlow =
  withTemporaryDirectory "ghc-native-swap-http-test" $ \root -> do
    compiler <- makeTestCompilerConfig (root <> "/artifacts")
    let serverConfig =
          ServerConfig
            { compilerConfig = compiler
            , maxRequestBytes = 1_048_576
            , maxConcurrentCompilations = 1
            }
    testWithApplication (compilerApplication serverConfig) $ \port -> do
      manager <- newManager defaultManagerSettings
      health <- get manager port "/healthz"
      assertEqual "health" status200 (responseStatus health)
      published <- putCompile manager port "rules" (pluginSource 700)
      assertEqual "publish status" status201 (responseStatus published)
      manifest <- decodeManifest published
      client <-
        newCompilerClient
          manager
          (baseUrl port)
          (root <> "/cache")
          "rules"
      candidate <- pollCompilerArtifact client ""
      selected <- maybe (assertFailure "client returned no artifact") pure candidate
      assertEqual "revision" (showText (manifestArtifactId manifest)) (candidateRevision selected)
      downloaded <- doesFileExist (candidatePath selected)
      assertBool "artifact downloaded" downloaded
      runtime <- newHotSwap (candidatePath selected) :: IO (HotSwap Int Int)
      invoke runtime 1 >>= assertEqual "downloaded artifact executes" 701
      closeHotSwap runtime
      unchanged <- pollCompilerArtifact client (candidateRevision selected)
      assertEqual "unchanged revision" Nothing unchanged
      failed <- putCompile manager port "rules" invalidSource
      assertEqual "compile error" status422 (responseStatus failed)
      latest <- get manager port "/v1/modules/rules"
      latestManifest <- decodeManifest latest
      assertEqual
        "failed publish did not replace manifest"
        (manifestArtifactId manifest)
        (manifestArtifactId latestManifest)

testQualifiedResolver :: IO ()
testQualifiedResolver =
  withTemporaryDirectory "ghc-native-swap-qualified-test" $ \root -> do
    compiler <- makeTestCompilerConfig (root <> "/artifacts")
    let moduleName = "Company.Zed_Plugin"
        source = Text.replace "module Plugin where" ("module " <> moduleName <> " where") (pluginSource 900)
    compiled <-
      compileModule
        compiler
        "qualified"
        CompileRequest
          { requestedModuleName = moduleName
          , requestedSource = source
          }
        >>= either (assertFailure . show) pure
    runtime <- newHotSwap (compiledPath compiled) :: IO (HotSwap Int Int)
    invoke runtime 2 >>= assertEqual "qualified result" 902
    closeHotSwap runtime

testRequestLimit :: IO ()
testRequestLimit =
  withTemporaryDirectory "ghc-native-swap-http-limit" $ \root -> do
    compiler <- makeTestCompilerConfig (root <> "/artifacts")
    let serverConfig =
          ServerConfig
            { compilerConfig = compiler
            , maxRequestBytes = 16
            , maxConcurrentCompilations = 1
            }
    testWithApplication (compilerApplication serverConfig) $ \port -> do
      manager <- newManager defaultManagerSettings
      response <- putCompile manager port "rules" (pluginSource 1)
      assertEqual "body limit" status413 (responseStatus response)

putCompile
  :: Manager
  -> Port
  -> String
  -> Text.Text
  -> IO (Response LazyByteString.ByteString)
putCompile manager port requestedSlot fixtureSource = do
  initial <- parseRequest (baseUrl port <> "/v1/modules/" <> requestedSlot)
  let request =
        initial
          { method = methodPut
          , requestBody =
              RequestBodyLBS
                ( encode
                    CompileRequest
                      { requestedModuleName = "Plugin"
                      , requestedSource = fixtureSource
                      }
                )
          , requestHeaders = [(hContentType, "application/json")]
          }
  httpLbs request manager

get
  :: Manager
  -> Port
  -> String
  -> IO (Response LazyByteString.ByteString)
get manager port path = parseRequest (baseUrl port <> path) >>= (`httpLbs` manager)

decodeManifest :: Response LazyByteString.ByteString -> IO ArtifactManifest
decodeManifest response =
  case eitherDecode (responseBody response) of
    Left message -> assertFailure message
    Right manifest -> pure manifest

baseUrl :: Port -> String
baseUrl port = "http://127.0.0.1:" <> show port

showText :: Text.Text -> String
showText = Text.unpack
