{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler
  ( compilerTests
  ) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import HotSwap
  ( HotSwap
  , closeHotSwap
  , invoke
  , newHotSwap
  )
import HotSwap.Compiler
  ( CompiledArtifact (compiledPath)
  , compileModule
  , validModuleName
  , validSlot
  )
import HotSwap.Compiler.Client
  ( newCompilerClient
  , pollCompilerArtifact
  )
import HotSwap.Compiler.Protocol
  ( ArtifactManifest (..)
  , CompileRequest (..)
  )
import HotSwap.Compiler.Server
  ( ServerConfig (..)
  , compilerApplication
  )
import HotSwap.Polling (Candidate (..))
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
  assertBool "generated module is reserved" (not (validModuleName "HotSwapGenerated"))
  assertBool "lowercase module" (not (validModuleName "plugin"))
  assertBool "empty segment" (not (validModuleName "Plugin..Rules"))

testHttpFlow :: IO ()
testHttpFlow =
  withTemporaryDirectory "hot-swap-http-test" $ \root -> do
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
  withTemporaryDirectory "hot-swap-qualified-test" $ \root -> do
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
  withTemporaryDirectory "hot-swap-http-limit" $ \root -> do
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
