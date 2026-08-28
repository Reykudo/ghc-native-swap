{-# LANGUAGE OverloadedStrings #-}

module GHC.NativeSwap.Compiler.Server
  ( ServerConfig (..)
  , compilerApplication
  ) where

import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Exception (bracket_)
import Data.Aeson (ToJSON, eitherDecodeStrict', encode)
import Data.ByteString qualified as ByteString
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.NativeSwap.Compiler
  ( CompileFailure (..)
  , CompiledArtifact (..)
  , CompilerConfig (..)
  , compileModule
  )
import GHC.NativeSwap.Compiler.Protocol
  ( ArtifactManifest (..)
  , ErrorResponse (..)
  , HealthResponse (..)
  )
import GHC.NativeSwap.Plugin (abiMagic)
import Network.HTTP.Types
  ( Status
  , hContentType
  , methodGet
  , methodPut
  , status200
  , status201
  , status400
  , status404
  , status405
  , status413
  , status422
  , status500
  , status504
  )
import Network.Wai
  ( Application
  , Request
  , Response
  , getRequestBodyChunk
  , pathInfo
  , requestMethod
  , responseFile
  , responseLBS
  )

data ServerConfig = ServerConfig
  { compilerConfig :: !CompilerConfig
  , maxRequestBytes :: !Int
  , maxConcurrentCompilations :: !Int
  }
  deriving (Eq, Show)

data Registry = Registry
  { latestBySlot :: !(Map Text ArtifactManifest)
  , pathsById :: !(Map Text FilePath)
  }

compilerApplication :: ServerConfig -> IO Application
compilerApplication config = do
  registry <- newTVarIO (Registry Map.empty Map.empty)
  semaphore <- newQSem (max 1 (maxConcurrentCompilations config))
  pure (route semaphore registry)
 where
  route semaphore registry request respond =
    case (requestMethod request, pathInfo request) of
      (method, ["healthz"])
        | method == methodGet ->
            respond
              ( jsonResponse
                  status200
                  HealthResponse
                    { healthStatus = "ok"
                    , healthGhcVersion = compilerGhcVersion (compilerConfig config)
                    , healthTargetPlatform = compilerTargetPlatform (compilerConfig config)
                    , healthAbiVersion = abiMagic
                    }
              )
      (method, ["v1", "modules", requestedSlot])
        | method == methodGet -> do
            current <- latestManifest registry requestedSlot
            respond $ maybe (errorResponse status404 "not_found" "slot is not published" Nothing) (jsonResponse status200) current
        | method == methodPut -> do
            body <- readLimitedBody (maxRequestBytes config) request
            case body of
              Nothing ->
                respond (errorResponse status413 "request_too_large" "request body is too large" Nothing)
              Just bytes ->
                case eitherDecodeStrict' bytes of
                  Left message ->
                    respond (errorResponse status400 "invalid_json" "invalid compile request" (Just (Text.pack message)))
                  Right compileRequest -> do
                    result <-
                      withSemaphore semaphore $
                        compileModule (compilerConfig config) requestedSlot compileRequest
                    case result of
                      Left failure -> respond (compileFailureResponse failure)
                      Right artifact -> do
                        publish registry artifact
                        respond (jsonResponse status201 (compiledManifest artifact))
        | otherwise -> respond methodNotAllowed
      (method, ["v1", "artifacts", identifier])
        | method == methodGet -> do
            artifactPath <- lookupArtifact registry identifier
            respond $
              maybe
                (errorResponse status404 "not_found" "artifact is not published" Nothing)
                (\path -> responseFile status200 [(hContentType, "application/x-sharedlib")] path Nothing)
                artifactPath
        | otherwise -> respond methodNotAllowed
      _ -> respond (errorResponse status404 "not_found" "route not found" Nothing)

publish :: TVar Registry -> CompiledArtifact -> IO ()
publish registry artifact = atomically $ do
  current <- readTVar registry
  let manifest = compiledManifest artifact
  writeTVar
    registry
    Registry
      { latestBySlot = Map.insert (manifestSlot manifest) manifest (latestBySlot current)
      , pathsById = Map.insert (manifestArtifactId manifest) (compiledPath artifact) (pathsById current)
      }

latestManifest :: TVar Registry -> Text -> IO (Maybe ArtifactManifest)
latestManifest registry requestedSlot =
  Map.lookup requestedSlot . latestBySlot <$> atomically (readTVar registry)

lookupArtifact :: TVar Registry -> Text -> IO (Maybe FilePath)
lookupArtifact registry identifier =
  Map.lookup identifier . pathsById <$> atomically (readTVar registry)

readLimitedBody :: Int -> Request -> IO (Maybe ByteString.ByteString)
readLimitedBody limit request = go 0 []
 where
  go size chunks = do
    chunk <- getRequestBodyChunk request
    if ByteString.null chunk
      then pure (Just (ByteString.concat (reverse chunks)))
      else do
        let nextSize = size + ByteString.length chunk
        if nextSize > limit
          then pure Nothing
          else go nextSize (chunk : chunks)

compileFailureResponse :: CompileFailure -> Response
compileFailureResponse failure =
  case failure of
    InvalidSlot value ->
      errorResponse status400 "invalid_slot" "invalid slot name" (Just value)
    InvalidModuleName value ->
      errorResponse status400 "invalid_module" "invalid module name" (Just value)
    EmptySource ->
      errorResponse status400 "empty_source" "source must not be empty" Nothing
    CompilationTimedOut ->
      errorResponse status504 "compile_timeout" "compiler timed out" Nothing
    CompilationFailed output ->
      errorResponse status422 "compile_failed" "compiler rejected the source" (Just output)
    ArtifactMissing ->
      errorResponse status500 "artifact_missing" "compiler produced no artifact" Nothing

jsonResponse :: (ToJSON value) => Status -> value -> Response
jsonResponse responseStatus value =
  responseLBS responseStatus [(hContentType, "application/json")] (encode value)

errorResponse :: Status -> Text -> Text -> Maybe Text -> Response
errorResponse responseStatus code message output =
  jsonResponse
    responseStatus
    ErrorResponse
      { responseErrorCode = code
      , responseErrorMessage = message
      , responseCompilerOutput = output
      }

methodNotAllowed :: Response
methodNotAllowed =
  errorResponse status405 "method_not_allowed" "method not allowed" Nothing

withSemaphore :: QSem -> IO value -> IO value
withSemaphore semaphore = bracket_ (waitQSem semaphore) (signalQSem semaphore)
