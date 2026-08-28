{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GHC.NativeSwap.Internal.ABI
  ( AbiDescriptor (..)
  , abiMagic
  , descriptorFor
  , generatedModuleName
  , generatedUnitIdFor
  , invokeBindingFor
  , invokeSymbolFor
  ) where

import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Word (Word64)
import GHC.Fingerprint.Type (Fingerprint (..))

data AbiDescriptor = AbiDescriptor !Word64 !Word64 !Word64
  deriving (Eq, Show)

abiMagic :: Word64
abiMagic = 0x4853574150000004

generatedModuleName :: String
generatedModuleName = "GHCNativeSwapGenerated"

descriptorFor :: forall value. (Typeable value) => AbiDescriptor
descriptorFor =
  case typeRepFingerprint (typeRep (Proxy @value)) of
    Fingerprint first second -> AbiDescriptor abiMagic first second

invokeBindingFor :: AbiDescriptor -> String
invokeBindingFor descriptor = "hotSwapInvoke" <> descriptorSuffix descriptor

generatedUnitIdFor :: AbiDescriptor -> String
generatedUnitIdFor descriptor = "hotswapplugin" <> descriptorSuffix descriptor

descriptorSuffix :: AbiDescriptor -> String
descriptorSuffix (AbiDescriptor magic first second) =
  "A"
    <> show magic
    <> "B"
    <> show first
    <> "C"
    <> show second

invokeSymbolFor :: AbiDescriptor -> String
invokeSymbolFor descriptor =
  generatedUnitIdFor descriptor
    <> "_"
    <> generatedModuleName
    <> "_"
    <> invokeBindingFor descriptor
    <> "_closure"
