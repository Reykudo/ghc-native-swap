module GHC.NativeSwap.Error
  ( HotSwapError (..)
  ) where

import Control.Exception (Exception)

data HotSwapError
  = NativeLoadFailed FilePath String
  | NativeSymbolMissing FilePath String
  | PluginInvocationFailed FilePath String
  | NativeUnloadFailed FilePath
  | HotSwapClosed
  deriving (Eq, Show)

instance Exception HotSwapError
