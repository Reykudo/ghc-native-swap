{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HotSwap.Compiler.Client
  ( CompilerClient
  , CompilerClientError (..)
  , newCompilerClient
  , pollCompilerArtifact
  ) where

import Control.Exception (Exception, bracketOnError, throwIO)
import Control.Monad (unless)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import HotSwap.Compiler.Protocol (ArtifactManifest (..))
import HotSwap.Plugin (abiMagic)
import HotSwap.Polling (Candidate (..))
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types (status200, status404, statusCode)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , removeFile
  , renameFile
  )
import System.FilePath ((</>), (<.>), takeDirectory)
import System.IO (hClose, openBinaryTempFile)

data CompilerClient = CompilerClient
  { clientManager :: !Manager
  , clientBaseUrl :: !String
  , clientCacheDirectory :: !FilePath
  , clientSlot :: !Text
  }

data CompilerClientError
  = UnexpectedStatus !Int
  | InvalidManifest !String
  | IncompatibleCompiler !Text !Text
  | IncompatibleAbi !Integer !Integer
  | UnsafeArtifactIdentifier !Text
  deriving (Eq, Show, Generic)

instance Exception CompilerClientError

newCompilerClient
  :: Manager
  -> String
  -> FilePath
  -> Text
  -> IO CompilerClient
newCompilerClient manager baseUrl cacheDirectory requestedSlot = do
  createDirectoryIfMissing True cacheDirectory
  pure
    CompilerClient
      { clientManager = manager
      , clientBaseUrl = reverse (dropWhile (== '/') (reverse baseUrl))
      , clientCacheDirectory = cacheDirectory
      , clientSlot = requestedSlot
      }

pollCompilerArtifact
  :: CompilerClient
  -> String
  -> IO (Maybe Candidate)
pollCompilerArtifact client currentRevision = do
  request <-
    parseRequest
      ( clientBaseUrl client
          <> "/v1/modules/"
          <> Text.unpack (clientSlot client)
      )
  response <- httpLbs request (clientManager client)
  if responseStatus response == status404
    then pure Nothing
    else do
      unless (responseStatus response == status200) $
        throwIO (UnexpectedStatus (statusCode (responseStatus response)))
      manifest <-
        either (throwIO . InvalidManifest) pure (eitherDecode (responseBody response))
      validateManifest manifest
      let revision = Text.unpack (manifestArtifactId manifest)
      if revision == currentRevision
        then pure Nothing
        else do
          path <- downloadArtifact client manifest
          pure
            ( Just
                Candidate
                  { candidateRevision = revision
                  , candidatePath = path
                  }
            )

validateManifest :: ArtifactManifest -> IO ()
validateManifest manifest = do
  let hostVersion = Text.pack __GLASGOW_HASKELL_FULL_VERSION__
  unless (manifestGhcVersion manifest == hostVersion) $
    throwIO (IncompatibleCompiler hostVersion (manifestGhcVersion manifest))
  unless (manifestAbiVersion manifest == abiMagic) $
    throwIO
      ( IncompatibleAbi
          (fromIntegral abiMagic)
          (fromIntegral (manifestAbiVersion manifest))
      )
  unless (safeIdentifier (manifestArtifactId manifest)) $
    throwIO (UnsafeArtifactIdentifier (manifestArtifactId manifest))

downloadArtifact :: CompilerClient -> ArtifactManifest -> IO FilePath
downloadArtifact client manifest = do
  let finalPath =
        clientCacheDirectory client
          </> Text.unpack (manifestArtifactId manifest)
          <.> "so"
  exists <- doesFileExist finalPath
  if exists
    then pure finalPath
    else do
      request <- parseRequest (clientBaseUrl client <> Text.unpack (manifestArtifactUrl manifest))
      response <- httpLbs request (clientManager client)
      unless (responseStatus response == status200) $
        throwIO (UnexpectedStatus (statusCode (responseStatus response)))
      writeAtomically finalPath (responseBody response)
      pure finalPath

writeAtomically :: FilePath -> LazyByteString.ByteString -> IO ()
writeAtomically finalPath bytes = do
  let directory = takeDirectory finalPath
  bracketOnError
    (openBinaryTempFile directory ".hot-swap-download")
    (\(temporaryPath, handle) -> hClose handle >> removeFile temporaryPath)
    (\(temporaryPath, handle) -> do
        LazyByteString.hPut handle bytes
        hClose handle
        renameFile temporaryPath finalPath
    )

safeIdentifier :: Text -> Bool
safeIdentifier value =
  not (Text.null value)
    && Text.all valid value
 where
  valid character =
    isAscii character
      && (isAlphaNum character || character == '_' || character == '-')
