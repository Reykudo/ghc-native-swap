{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HotSwap.Compiler (
    CompileFailure (..),
    CompiledArtifact (..),
    CompilerConfig (..),
    compileModule,
    makeCompilerConfig,
    validModuleName,
    validSlot,
) where

import Control.Exception (bracket)
import Data.ByteString qualified as ByteString
import Data.Char (isAlphaNum, isAscii, isUpper)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word64)
import HotSwap.Compiler.Protocol (
    ArtifactManifest (..),
    CompileRequest (..),
 )
import HotSwap.Plugin (
    abiBindingFor,
    abiMagic,
    abiModuleName,
    abiProbeUnitId,
    abiUnitIdFor,
 )
import System.Directory (
    canonicalizePath,
    createDirectory,
    createDirectoryIfMissing,
    doesFileExist,
    removePathForcibly,
    renameFile,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.Process (
    CreateProcess (cwd),
    proc,
    readCreateProcessWithExitCode,
    readProcess,
 )
import System.Timeout (timeout)
import Text.Read (readMaybe)

data CompilerConfig = CompilerConfig
    { ghcExecutable :: !FilePath
    , artifactDirectory :: !FilePath
    , packageDatabases :: ![FilePath]
    , exposedPackages :: ![String]
    , compileTimeoutMicroseconds :: !Int
    , compilerGhcVersion :: !Text
    , compilerTargetPlatform :: !Text
    }
    deriving (Eq, Show)

data CompileFailure
    = InvalidSlot !Text
    | InvalidModuleName !Text
    | EmptySource
    | CompilationTimedOut
    | CompilationFailed !Text
    | ArtifactMissing
    deriving (Eq, Show)

data CompiledArtifact = CompiledArtifact
    { compiledManifest :: !ArtifactManifest
    , compiledPath :: !FilePath
    }
    deriving (Eq, Show)

makeCompilerConfig ::
    FilePath ->
    FilePath ->
    [FilePath] ->
    [String] ->
    Int ->
    IO CompilerConfig
makeCompilerConfig compiler artifactRoot packageDatabasePaths packages timeoutMicroseconds = do
    createDirectoryIfMissing True artifactRoot
    absoluteRoot <- canonicalizePath artifactRoot
    absolutePackageDatabases <- traverse canonicalizePath packageDatabasePaths
    version <- commandOutput compiler ["--numeric-version"]
    target <- commandOutput compiler ["--print-target-platform"]
    pure
        CompilerConfig
            { ghcExecutable = compiler
            , artifactDirectory = absoluteRoot
            , packageDatabases = absolutePackageDatabases
            , exposedPackages = packages
            , compileTimeoutMicroseconds = timeoutMicroseconds
            , compilerGhcVersion = version
            , compilerTargetPlatform = target
            }

compileModule ::
    CompilerConfig ->
    Text ->
    CompileRequest ->
    IO (Either CompileFailure CompiledArtifact)
compileModule config requestedSlot request
    | not (validSlot requestedSlot) = pure (Left (InvalidSlot requestedSlot))
    | not (validModuleName (requestedModuleName request)) =
        pure (Left (InvalidModuleName (requestedModuleName request)))
    | Text.null (Text.strip (requestedSource request)) = pure (Left EmptySource)
    | otherwise = do
        identifier <- freshIdentifier requestedSlot
        let buildDirectory = artifactDirectory config </> (".build-" <> Text.unpack identifier)
        bracket
            (createDirectory buildDirectory >> pure buildDirectory)
            removePathForcibly
            (\_ -> compileIn buildDirectory identifier)
  where
    compileIn buildDirectory identifier = do
        sourcePath <- writeSource buildDirectory request
        probePath <- writeProbe buildDirectory (requestedModuleName request)
        let stagedArtifact = buildDirectory </> "artifact.so"
            finalArtifact = artifactDirectory config </> Text.unpack identifier <.> "so"
            probeExecutable = buildDirectory </> "abi-probe"
            baseArguments =
                [ "-v0"
                , "-O1"
                , "-dynamic"
                , "-fPIC"
                , "-fno-full-laziness"
                , "-pgma"
                , "clang"
                , "-opta-Qunused-arguments"
                , "-hide-all-packages"
                , "-odir"
                , buildDirectory
                , "-hidir"
                , buildDirectory
                , "-stubdir"
                , buildDirectory
                ]
                    <> concatMap (\database -> ["-package-db", database]) (packageDatabases config)
                    <> concatMap (\packageName -> ["-package", packageName]) (exposedPackages config)
            compilerProcess arguments =
                (proc (ghcExecutable config) arguments){cwd = Just buildDirectory}
        probeBuild <-
            runLimitedProcess
                config
                ( compilerProcess
                    ( baseArguments
                        <> [ "-this-unit-id"
                           , abiProbeUnitId
                           , "-fforce-recomp"
                           , probePath
                           , "-o"
                           , probeExecutable
                           ]
                    )
                )
        case probeBuild of
            Left failure -> pure (Left failure)
            Right _ -> do
                probeRun <- runLimitedProcess config (proc probeExecutable [])
                case probeRun >>= parseDescriptor of
                    Left failure -> pure (Left failure)
                    Right descriptor -> do
                        generatedPath <-
                            writeGeneratedModule
                                buildDirectory
                                (requestedModuleName request)
                                descriptor
                        compiled <-
                            runLimitedProcess
                                config
                                ( compilerProcess
                                    ( baseArguments
                                        <> [ "-this-unit-id"
                                           , abiUnitIdFor descriptor
                                           , "-fforce-recomp"
                                           , "-shared"
                                           , "-fno-link-rts"
                                           , "-optl-Wl,-Bsymbolic"
                                           , sourcePath
                                           , generatedPath
                                           , "-o"
                                           , stagedArtifact
                                           ]
                                    )
                                )
                        case compiled of
                            Left failure -> pure (Left failure)
                            Right _ -> publishArtifact stagedArtifact finalArtifact identifier

    publishArtifact stagedArtifact finalArtifact identifier = do
        exists <- doesFileExist stagedArtifact
        if not exists
            then pure (Left ArtifactMissing)
            else do
                renameFile stagedArtifact finalArtifact
                now <- getCurrentTime
                let manifest =
                        ArtifactManifest
                            { manifestArtifactId = identifier
                            , manifestSlot = requestedSlot
                            , manifestModuleName = requestedModuleName request
                            , manifestGhcVersion = compilerGhcVersion config
                            , manifestTargetPlatform = compilerTargetPlatform config
                            , manifestAbiVersion = abiMagic
                            , manifestArtifactUrl = "/v1/artifacts/" <> identifier
                            , manifestCreatedAt = now
                            }
                pure (Right (CompiledArtifact manifest finalArtifact))

validSlot :: Text -> Bool
validSlot value =
    not (Text.null value)
        && Text.length value <= 80
        && Text.all valid value
  where
    valid character =
        isAscii character
            && (isAlphaNum character || character == '_' || character == '-')

validModuleName :: Text -> Bool
validModuleName value =
    not (Text.null value)
        && Text.length value <= 200
        && value /= Text.pack abiModuleName
        && all validSegment (Text.splitOn "." value)
  where
    validSegment segment =
        case Text.uncons segment of
            Nothing -> False
            Just (first, rest) ->
                isAscii first
                    && isUpper first
                    && Text.all validRest rest
    validRest character =
        isAscii character
            && (isAlphaNum character || character == '_' || character == '\'')

writeSource :: FilePath -> CompileRequest -> IO FilePath
writeSource buildDirectory request = do
    let segments = map Text.unpack (Text.splitOn "." (requestedModuleName request))
        sourceDirectory = foldl (</>) buildDirectory (init segments)
        sourcePath = sourceDirectory </> last segments <.> "hs"
    createDirectoryIfMissing True sourceDirectory
    ByteString.writeFile sourcePath (Text.encodeUtf8 (requestedSource request))
    pure sourcePath

writeProbe :: FilePath -> Text -> IO FilePath
writeProbe buildDirectory moduleName = do
    let path = buildDirectory </> "AbiProbe.hs"
        source =
            Text.unlines
                [ "module Main where"
                , "import qualified " <> moduleName <> " as Plugin"
                , "import HotSwap.Plugin (descriptorWordsFor)"
                , "main :: IO ()"
                , "main = print (descriptorWordsFor Plugin.invoke)"
                ]
    ByteString.writeFile path (Text.encodeUtf8 source)
    pure path

writeGeneratedModule ::
    FilePath ->
    Text ->
    (Word64, Word64, Word64) ->
    IO FilePath
writeGeneratedModule buildDirectory moduleName descriptor = do
    let path = buildDirectory </> abiModuleName <.> "hs"
        binding = Text.pack (abiBindingFor descriptor)
        source =
            Text.unlines
                [ "module " <> Text.pack abiModuleName <> " (" <> binding <> ") where"
                , "import qualified " <> moduleName <> " as Plugin"
                , "import HotSwap.Plugin (Export (Export))"
                , "{-# NOINLINE " <> binding <> " #-}"
                , binding <> " = Export Plugin.invoke"
                ]
    ByteString.writeFile path (Text.encodeUtf8 source)
    pure path

runLimitedProcess ::
    CompilerConfig ->
    CreateProcess ->
    IO (Either CompileFailure String)
runLimitedProcess config process = do
    outcome <-
        timeout
            (compileTimeoutMicroseconds config)
            (readCreateProcessWithExitCode process "")
    pure $ case outcome of
        Nothing -> Left CompilationTimedOut
        Just (ExitFailure _, standardOutput, standardError) ->
            Left (CompilationFailed (truncateDiagnostic (standardOutput <> standardError)))
        Just (ExitSuccess, standardOutput, _) -> Right standardOutput

parseDescriptor :: String -> Either CompileFailure (Word64, Word64, Word64)
parseDescriptor output =
    case readMaybe output of
        Just descriptor@(magic, _, _)
            | magic == abiMagic -> Right descriptor
        _ -> Left (CompilationFailed "compiler could not determine the invoke ABI")

freshIdentifier :: Text -> IO Text
freshIdentifier requestedSlot = do
    timestamp <- round . (* 1_000_000) <$> getPOSIXTime :: IO Integer
    unique <- hashUnique <$> newUnique
    pure
        ( requestedSlot
            <> "-"
            <> Text.pack (show timestamp)
            <> "-"
            <> Text.pack (show unique)
        )

commandOutput :: FilePath -> [String] -> IO Text
commandOutput command arguments =
    Text.strip . Text.pack <$> readProcess command arguments ""

truncateDiagnostic :: String -> Text
truncateDiagnostic = Text.take 65_536 . Text.pack
