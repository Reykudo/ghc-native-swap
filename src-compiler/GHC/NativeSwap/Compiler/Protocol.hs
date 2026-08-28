{-# LANGUAGE DeriveGeneric #-}

module GHC.NativeSwap.Compiler.Protocol
  ( ArtifactManifest (..)
  , CompileRequest (..)
  , ErrorResponse (..)
  , HealthResponse (..)
  ) where

import Data.Aeson
  ( FromJSON (parseJSON)
  , Options (fieldLabelModifier)
  , ToJSON (toEncoding, toJSON)
  , defaultOptions
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  )
import Data.Char (toLower)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)

data CompileRequest = CompileRequest
  { requestedModuleName :: !Text
  , requestedSource :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON CompileRequest where
  parseJSON = genericParseJSON (jsonOptions "requested")

instance ToJSON CompileRequest where
  toJSON = genericToJSON (jsonOptions "requested")
  toEncoding = genericToEncoding (jsonOptions "requested")

data ArtifactManifest = ArtifactManifest
  { manifestArtifactId :: !Text
  , manifestSlot :: !Text
  , manifestModuleName :: !Text
  , manifestGhcVersion :: !Text
  , manifestTargetPlatform :: !Text
  , manifestAbiVersion :: !Word64
  , manifestArtifactUrl :: !Text
  , manifestCreatedAt :: !UTCTime
  }
  deriving (Eq, Show, Generic)

instance FromJSON ArtifactManifest where
  parseJSON = genericParseJSON (jsonOptions "manifest")

instance ToJSON ArtifactManifest where
  toJSON = genericToJSON (jsonOptions "manifest")
  toEncoding = genericToEncoding (jsonOptions "manifest")

data ErrorResponse = ErrorResponse
  { responseErrorCode :: !Text
  , responseErrorMessage :: !Text
  , responseCompilerOutput :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON (jsonOptions "response")

instance ToJSON ErrorResponse where
  toJSON = genericToJSON (jsonOptions "response")
  toEncoding = genericToEncoding (jsonOptions "response")

data HealthResponse = HealthResponse
  { healthStatus :: !Text
  , healthGhcVersion :: !Text
  , healthTargetPlatform :: !Text
  , healthAbiVersion :: !Word64
  }
  deriving (Eq, Show, Generic)

instance FromJSON HealthResponse where
  parseJSON = genericParseJSON (jsonOptions "health")

instance ToJSON HealthResponse where
  toJSON = genericToJSON (jsonOptions "health")
  toEncoding = genericToEncoding (jsonOptions "health")

jsonOptions :: String -> Options
jsonOptions prefix =
  defaultOptions
    { fieldLabelModifier = lowerInitial . drop (length prefix)
    }

lowerInitial :: String -> String
lowerInitial [] = []
lowerInitial (first : rest) = toLower first : rest
