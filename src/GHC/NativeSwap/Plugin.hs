{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GHC.NativeSwap.Plugin
  ( Entry
  , Export (..)
  , abiBindingFor
  , abiMagic
  , abiModuleName
  , abiProbeUnitId
  , abiUnitIdFor
  , descriptorWordsFor
  ) where

import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.NativeSwap.Internal.ABI
  ( AbiDescriptor (..)
  , abiMagic
  , descriptorFor
  , generatedModuleName
  , generatedUnitIdFor
  , invokeBindingFor
  )

type Entry input output = input -> IO output

data Export value = Export value

abiProbeUnitId :: String
abiProbeUnitId = "hotswapprobe"

abiModuleName :: String
abiModuleName = generatedModuleName

abiBindingFor :: (Word64, Word64, Word64) -> String
abiBindingFor (magic, first, second) =
  invokeBindingFor (AbiDescriptor magic first second)

abiUnitIdFor :: (Word64, Word64, Word64) -> String
abiUnitIdFor (magic, first, second) =
  generatedUnitIdFor (AbiDescriptor magic first second)

descriptorWordsFor :: forall value. (Typeable value) => value -> (Word64, Word64, Word64)
descriptorWordsFor _ =
  case descriptorFor @value of
    AbiDescriptor magic first second -> (magic, first, second)
