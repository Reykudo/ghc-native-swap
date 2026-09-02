{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Support
  ( RuntimeFixtures (..)
  , blockingPluginSource
  , buildRuntimeFixtures
  , compileFixture
  , destroyRuntimeFixtures
  , invalidSource
  , lazyOutputSource
  , makeTestCompilerConfig
  , pluginSource
  , withTemporaryDirectory
  , wrongTypeSource
  ) where

import Control.Exception (bracket)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word64)
import GHC.NativeSwap.Compiler
  ( CompileFailure
  , CompiledArtifact (compiledPath)
  , CompilerConfig (..)
  , compileModule
  , makeCompilerConfig
  )
import GHC.NativeSwap.Compiler.Protocol (CompileRequest (..))
import GHC.NativeSwap.Plugin
  ( abiBindingFor
  , abiModuleName
  , abiUnitIdFor
  )
import System.Directory
  ( createDirectory
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , getTemporaryDirectory
  , removePathForcibly
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process
  ( CreateProcess (cwd)
  , proc
  , readCreateProcessWithExitCode
  )
import System.Timeout (timeout)

data RuntimeFixtures = RuntimeFixtures
  { fixtureRoot :: !FilePath
  , fixtureV1 :: !FilePath
  , fixtureV2 :: !FilePath
  , fixtureBlockingV1 :: !FilePath
  , fixtureBlockingV2 :: !FilePath
  , fixtureWrongMagic :: !FilePath
  , fixtureWrongType :: !FilePath
  , fixtureMissingEntry :: !FilePath
  , fixtureLazyOutput :: !FilePath
  , fixtureZeroArgument :: !FilePath
  , fixtureMultipleArguments :: !FilePath
  }

buildRuntimeFixtures :: IO RuntimeFixtures
buildRuntimeFixtures = do
  root <- createTemporaryDirectory "ghc-native-swap-runtime-fixtures"
  config <- makeTestCompilerConfig root
  versionOne <- compileFixture config "version-one" (pluginSource 100)
  versionTwo <- compileFixture config "version-two" (pluginSource 200)
  blockingOne <- compileFixture config "blocking-one" (blockingPluginSource 100)
  blockingTwo <- compileFixture config "blocking-two" (blockingPluginSource 200)
  wrongMagic <- compileRawFixture config "wrong-magic" (Just (0, 0, 0))
  wrongType <- compileFixture config "wrong-type" wrongTypeSource
  missingEntry <- compileRawFixture config "missing-entry" Nothing
  lazyOutput <- compileFixture config "lazy-output" lazyOutputSource
  zeroArgument <- compileFixture config "zero-argument" zeroArgumentSource
  multipleArguments <-
    compileFixture config "multiple-arguments" multipleArgumentsSource
  pure
    RuntimeFixtures
      { fixtureRoot = root
      , fixtureV1 = versionOne
      , fixtureV2 = versionTwo
      , fixtureBlockingV1 = blockingOne
      , fixtureBlockingV2 = blockingTwo
      , fixtureWrongMagic = wrongMagic
      , fixtureWrongType = wrongType
      , fixtureMissingEntry = missingEntry
      , fixtureLazyOutput = lazyOutput
      , fixtureZeroArgument = zeroArgument
      , fixtureMultipleArguments = multipleArguments
      }

destroyRuntimeFixtures :: RuntimeFixtures -> IO ()
destroyRuntimeFixtures = removePathForcibly . fixtureRoot

makeTestCompilerConfig :: FilePath -> IO CompilerConfig
makeTestCompilerConfig artifactRoot = do
  projectRoot <- findProjectRoot
  let packageDatabase =
        projectRoot
          </> "dist-newstyle"
          </> "packagedb"
          </> ("ghc-" <> __GLASGOW_HASKELL_FULL_VERSION__)
  packageDatabaseExists <- doesDirectoryExist packageDatabase
  if not packageDatabaseExists
    then fail ("local package database does not exist: " <> packageDatabase)
    else
      makeCompilerConfig
        "ghc"
        artifactRoot
        [packageDatabase]
        ["base", "ghc-native-swap"]
        30_000_000

compileFixture :: CompilerConfig -> Text -> Text -> IO FilePath
compileFixture config requestedSlot fixtureSource = do
  result <-
    compileModule
      config
      requestedSlot
      CompileRequest
        { requestedModuleName = "Plugin"
        , requestedSource = fixtureSource
        }
  case result of
    Left failure -> fail ("fixture compilation failed: " <> showFailure failure)
    Right artifact -> pure (compiledPath artifact)

compileRawFixture
  :: CompilerConfig
  -> Text
  -> Maybe (Word64, Word64, Word64)
  -> IO FilePath
compileRawFixture config label descriptor = do
  unique <- hashUnique <$> newUnique
  let buildDirectory = artifactDirectory config </> (".invalid-" <> show unique)
      sourcePath = buildDirectory </> abiModuleName <> ".hs"
      artifactPath = artifactDirectory config </> Text.unpack label <> "-" <> show unique <> ".so"
      unitIdentifier = maybe "hotswapinvalid" abiUnitIdFor descriptor
      arguments =
        [ "-v0"
        , "-O1"
        , "-dynamic"
        , "-fPIC"
        , "-fno-full-laziness"
        , "-pgma"
        , "clang"
        , "-opta-Qunused-arguments"
        , "-hide-all-packages"
        , "-this-unit-id"
        , unitIdentifier
        , "-odir"
        , buildDirectory
        , "-hidir"
        , buildDirectory
        , "-shared"
        , "-fno-link-rts"
        , "-optl-Wl,-Bsymbolic"
        , sourcePath
        , "-o"
        , artifactPath
        ]
          <> concatMap (\database -> ["-package-db", database]) (packageDatabases config)
          <> concatMap (\packageId -> ["-package-id", packageId]) (exposedPackages config)
      process = (proc (ghcExecutable config) arguments) {cwd = Just buildDirectory}
      fixtureSource =
        case descriptor of
          Just descriptorWords ->
            let binding = Text.pack (abiBindingFor descriptorWords)
            in  Text.unlines
                  [ "module " <> Text.pack abiModuleName <> " (" <> binding <> ") where"
                  , "import GHC.NativeSwap.Plugin (Entry)"
                  , "{-# NOINLINE " <> binding <> " #-}"
                  , binding <> " :: Entry Int Int"
                  , binding <> " = pure"
                  ]
          Nothing ->
            Text.unlines
              [ "module " <> Text.pack abiModuleName <> " (notTheEntry) where"
              , "notTheEntry :: Int -> Int"
              , "notTheEntry value = value"
              ]
  bracket
    (createDirectory buildDirectory >> ByteString.writeFile sourcePath (TextEncoding.encodeUtf8 fixtureSource))
    (const (removePathForcibly buildDirectory))
    ( \_ -> do
        outcome <-
          timeout
            (compileTimeoutMicroseconds config)
            (readCreateProcessWithExitCode process "")
        case outcome of
          Nothing -> fail "invalid fixture compilation timed out"
          Just (ExitFailure _, standardOutput, standardError) ->
            fail ("invalid fixture compilation failed:\n" <> standardOutput <> standardError)
          Just (ExitSuccess, _, _) -> pure artifactPath
    )

withTemporaryDirectory :: String -> (FilePath -> IO value) -> IO value
withTemporaryDirectory label =
  bracket
    (createTemporaryDirectory label)
    removePathForcibly

createTemporaryDirectory :: String -> IO FilePath
createTemporaryDirectory label = do
  temporaryRoot <- getTemporaryDirectory
  unique <- hashUnique <$> newUnique
  let path = temporaryRoot </> (label <> "-" <> show unique)
  createDirectory path
  pure path

findProjectRoot :: IO FilePath
findProjectRoot = getCurrentDirectory >>= search
 where
  search directory = do
    markerExists <- doesFileExist (directory </> "ghc-native-swap.cabal")
    if markerExists
      then pure directory
      else do
        let parent = takeDirectory directory
        if parent == directory
          then fail "could not locate ghc-native-swap.cabal"
          else search parent

pluginSource :: Int -> Text
pluginSource offset =
  Text.unlines
    [ "module Plugin where"
    , "import GHC.NativeSwap.Plugin (Entry)"
    , "invoke :: Entry Int Int"
    , "invoke = run"
    , "run :: Int -> IO Int"
    , "run value"
    , "  | value == 13 = ioError (userError \"plugin exception\")"
    , "  | otherwise = pure (value + " <> Text.pack (show offset) <> ")"
    ]

blockingPluginSource :: Int -> Text
blockingPluginSource offset =
  Text.unlines
    [ "module Plugin where"
    , "import Control.Concurrent.MVar (MVar, putMVar, takeMVar)"
    , "import GHC.NativeSwap.Plugin (Entry)"
    , "type Request = (MVar (), MVar (), Int)"
    , "invoke :: Entry Request Int"
    , "invoke = run"
    , "run :: Request -> IO Int"
    , "run (started, release, value) = do"
    , "  putMVar started ()"
    , "  takeMVar release"
    , "  pure (value + " <> Text.pack (show offset) <> ")"
    ]

wrongTypeSource :: Text
wrongTypeSource =
  Text.unlines
    [ "module Plugin where"
    , "import GHC.NativeSwap.Plugin (Entry)"
    , "invoke :: Entry Int String"
    , "invoke = pure . show"
    ]

lazyOutputSource :: Text
lazyOutputSource =
  Text.unlines
    [ "module Plugin where"
    , "import GHC.NativeSwap.Plugin (Entry)"
    , "invoke :: Entry Int [Int]"
    , "invoke _ = pure [1, error \"latent plugin thunk\"]"
    ]

zeroArgumentSource :: Text
zeroArgumentSource =
  Text.unlines
    [ "module Plugin where"
    , "invoke :: IO Int"
    , "invoke = pure 77"
    ]

multipleArgumentsSource :: Text
multipleArgumentsSource =
  Text.unlines
    [ "module Plugin where"
    , "invoke :: Int -> Int -> IO Int"
    , "invoke first second = pure (first + second + 400)"
    ]

invalidSource :: Text
invalidSource = "module Plugin where\nthis is not Haskell"

showFailure :: CompileFailure -> String
showFailure = show
