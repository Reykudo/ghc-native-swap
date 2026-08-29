{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.NativeSwap.Internal.Native
  ( Native
  , acquireNativeCall
  , invokeNative
  , loadNative
  , nativeArtifactPath
  , readNative
  , releaseNativeCall
  , runPluginAction
  , unloadNative
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , check
  , modifyTVar'
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.DeepSeq (force)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , evaluate
  , finally
  , fromException
  , mask
  , mask_
  , onException
  , throwIO
  , tryJust
  )
import Control.Monad (filterM, unless)
import Data.Bits ((.|.))
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  )
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import Foreign.C.Error
  ( eEXIST
  , getErrno
  , throwErrno
  )
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr
  ( Ptr
  , WordPtr
  , castPtr
  , intPtrToPtr
  , wordPtrToPtr
  )
import Foreign.StablePtr
  ( StablePtr
  , deRefStablePtr
  , freeStablePtr
  )
import GHC.NativeSwap.Error (HotSwapError (..))
import GHC.NativeSwap.Internal.ABI
  ( AbiDescriptor
  , invokeSymbolFor
  )
import GHC.NativeSwap.Plugin (Entry, Export (..))
import GHC.NativeSwap.UnloadSafe (UnloadSafe, forceUnloadSafe)
import GHCi.Message (LoadedDLL)
import GHCi.ObjLink
  ( ShouldRetainCAFs (RetainCAFs)
  , initObjLinker
  , loadDLL
  , lookupSymbolInDLL
  )
import Numeric (readHex)
import System.Directory (canonicalizePath)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC)

data Native value = Native
  { nativePath :: !FilePath
  , nativeHandle :: !(Ptr LoadedDLL)
  , nativeValue :: !(StablePtr (Export value))
  , nativeRanges :: ![NativeRange]
  }

data NativeRange = NativeRange !(Ptr ()) !CSize

{-# NOINLINE linkerLock #-}
linkerLock :: MVar ()
linkerLock = unsafePerformIO (newMVar ())

{-# NOINLINE pendingRangeGuards #-}
pendingRangeGuards :: IORef [NativeRange]
pendingRangeGuards = unsafePerformIO (newIORef [])

data NativeCallGate = NativeCallGate
  { activeNativeCalls :: !(TVar Int)
  , nativeUnloadActive :: !(TVar Bool)
  }

{-# NOINLINE nativeCallGate #-}
nativeCallGate :: NativeCallGate
nativeCallGate = unsafePerformIO $ do
  active <- newTVarIO 0
  unloading <- newTVarIO False
  pure (NativeCallGate active unloading)

foreign import ccall unsafe "getStablePtr"
  c_getStablePtr :: Ptr () -> IO (StablePtr value)

foreign import ccall unsafe "unloadNativeObj"
  c_unloadNativeObj :: Ptr () -> IO Int

foreign import ccall unsafe "mmap"
  c_mmap
    :: Ptr ()
    -> CSize
    -> CInt
    -> CInt
    -> CInt
    -> Int64
    -> IO (Ptr ())

foreign import ccall unsafe "munmap"
  c_munmap :: Ptr () -> CSize -> IO CInt

loadNative :: AbiDescriptor -> FilePath -> IO (Native value)
loadNative expected requestedPath = do
  absolutePath <- canonicalizePath requestedPath
  withMVar linkerLock $ \() -> mask_ $ do
    reservePendingRanges
    initObjLinker RetainCAFs
    handle <-
      loadDLL absolutePath >>= either (throwIO . NativeLoadFailed absolutePath) pure
    ranges <- mappedNativeRanges absolutePath
    let cleanup = cleanupHandle handle ranges
    ( do
        closurePointer <-
          requireSymbol absolutePath handle (invokeSymbolFor expected)
        stablePointer <- c_getStablePtr closurePointer
        pure
          Native
            { nativePath = absolutePath
            , nativeHandle = handle
            , nativeValue = stablePointer
            , nativeRanges = ranges
            }
      )
      `onException` cleanup

invokeNative
  :: (UnloadSafe output)
  => Native (Entry input output)
  -> input
  -> IO output
invokeNative native input = do
  invoke <- readNative native
  runPluginAction
    (nativePath native)
    (invoke input >>= evaluate . forceUnloadSafe)

readNative :: Native value -> IO value
readNative native = do
  Export value <- deRefStablePtr (nativeValue native)
  pure value

nativeArtifactPath :: Native value -> FilePath
nativeArtifactPath = nativePath

runPluginAction :: FilePath -> IO output -> IO output
runPluginAction path action = mask $ \restore -> do
  outcome <-
    tryJust
      synchronousException
      (restore action)
  case outcome of
    Left exception -> do
      message <- renderException exception
      throwIO (PluginInvocationFailed path message)
    Right output -> pure output

unloadNative :: Native value -> IO ()
unloadNative native =
  withMVar linkerLock $ \() -> withNativeUnloadBarrier $ mask_ $ do
    freeStablePtr (nativeValue native)
    performMajorGC
    unloaded <- c_unloadNativeObj (castPtr (nativeHandle native))
    unless (unloaded /= 0) $ do
      retireRanges (nativeRanges native)
      throwIO (NativeUnloadFailed (nativePath native))
    performMajorGC
    retireRanges (nativeRanges native)

acquireNativeCall :: STM ()
acquireNativeCall = do
  unloading <- readTVar (nativeUnloadActive nativeCallGate)
  check (not unloading)
  modifyTVar' (activeNativeCalls nativeCallGate) (+ 1)

releaseNativeCall :: STM ()
releaseNativeCall =
  modifyTVar' (activeNativeCalls nativeCallGate) (subtract 1)

requireSymbol :: FilePath -> Ptr LoadedDLL -> String -> IO (Ptr value)
requireSymbol path handle symbol =
  lookupSymbolInDLL handle symbol
    >>= maybe (throwIO (NativeSymbolMissing path symbol)) pure

cleanupHandle :: Ptr LoadedDLL -> [NativeRange] -> IO ()
cleanupHandle handle ranges = do
  performMajorGC
  _ <- c_unloadNativeObj (castPtr handle)
  performMajorGC
  retireRanges ranges

mappedNativeRanges :: FilePath -> IO [NativeRange]
mappedNativeRanges path = do
  mappings <- lines <$> readFile "/proc/self/maps"
  let ranges = mapMaybe (parseMapping path) mappings
  if null ranges
    then throwIO (NativeLoadFailed path "artifact mappings are absent from /proc/self/maps")
    else pure ranges

parseMapping :: FilePath -> String -> Maybe NativeRange
parseMapping expectedPath mapping =
  case words mapping of
    address : _permissions : _offset : _device : _inode : _path
      | matchesPath -> do
          (startText, '-' : endText) <- Just (break (== '-') address)
          start <- parseHexWord startText
          end <- parseHexWord endText
          if end > start
            then Just (NativeRange (wordPtrToPtr start) (fromIntegral (end - start)))
            else Nothing
    _ -> Nothing
 where
  matchesPath =
    expectedPath `isSuffixOf` mapping
      || (expectedPath <> " (deleted)") `isSuffixOf` mapping

parseHexWord :: String -> Maybe WordPtr
parseHexWord value =
  case readHex value of
    [(parsed, "")] -> Just parsed
    _ -> Nothing

retireRanges :: [NativeRange] -> IO ()
retireRanges ranges = do
  atomicModifyIORef' pendingRangeGuards (\pending -> (ranges <> pending, ()))
  reservePendingRanges

reservePendingRanges :: IO ()
reservePendingRanges = do
  pending <- atomicModifyIORef' pendingRangeGuards (\ranges -> ([], ranges))
  unguarded <-
    filterM (fmap not . reserveRange) pending
      `onException` atomicModifyIORef'
        pendingRangeGuards
        (\ranges -> (pending <> ranges, ()))
  atomicModifyIORef' pendingRangeGuards (\ranges -> (unguarded <> ranges, ()))

reserveRange :: NativeRange -> IO Bool
reserveRange (NativeRange start size) = do
  mapped <-
    c_mmap
      start
      size
      0
      (mapPrivate .|. mapAnonymous .|. mapFixedNoReplace)
      (-1)
      0
  if mapped == start
    then pure True
    else
      if mapped == mapFailed
        then do
          errno <- getErrno
          if errno == eEXIST
            then pure False
            else throwErrno "mmap(MAP_FIXED_NOREPLACE)"
        else do
          _ <- c_munmap mapped size
          throwIO (userError "MAP_FIXED_NOREPLACE is unsupported")

mapPrivate, mapAnonymous, mapFixedNoReplace :: CInt
mapPrivate = 0x02
mapAnonymous = 0x20
mapFixedNoReplace = 0x100000

mapFailed :: Ptr ()
mapFailed = intPtrToPtr (-1)

withNativeUnloadBarrier :: IO value -> IO value
withNativeUnloadBarrier action = mask $ \restore -> do
  atomically $ do
    unloading <- readTVar (nativeUnloadActive nativeCallGate)
    check (not unloading)
    writeTVar (nativeUnloadActive nativeCallGate) True
  atomically $ do
    active <- readTVar (activeNativeCalls nativeCallGate)
    check (active == 0)
  restore action
    `finally` atomically (writeTVar (nativeUnloadActive nativeCallGate) False)

synchronousException :: SomeException -> Maybe SomeException
synchronousException exception =
  case fromException exception :: Maybe SomeAsyncException of
    Nothing -> Just exception
    Just _ -> Nothing

renderException :: SomeException -> IO String
renderException exception = do
  rendered <-
    tryJust
      synchronousException
      (evaluate (force (displayException exception)))
  pure $ either (const "plugin exception could not be rendered") id rendered
