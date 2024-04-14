{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.CmdPath
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'path' command. Query for project configuration
-- information.
module Distribution.Client.CmdPath
  ( pathCommand
  , pathAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdInstall.ClientInstallFlags
  ( cinstInstalldir
  )
import Distribution.Client.Config
  ( defaultCacheHome
  , defaultInstallPath
  , defaultStoreDir
  , getConfigFilePath
  )
import Distribution.Client.DistDirLayout (CabalDirLayout (..), distProjectRootDirectory)
import Distribution.Client.Errors
import Distribution.Client.GlobalFlags
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig.Types
  ( ProjectConfig (..)
  , ProjectConfigBuildOnly (..)
  , ProjectConfigShared (..)
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Client.ScriptUtils
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , yesNoOpt
  )
import Distribution.Client.Utils.Json
  ( (.=)
  )
import qualified Distribution.Client.Utils.Json as Json
import Distribution.Client.Version
  ( cabalInstallVersion
  )
import Distribution.ReadE
  ( ReadE (ReadE)
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , noArg
  , option
  , reqArg
  )
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
  ( Flag (..)
  , flagToList
  , fromFlagOrDefault
  )
import Distribution.Simple.Program
import Distribution.Simple.Utils
  ( die'
  , dieWithException
  , withOutputMarker
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

pathCommand :: CommandUI (NixStyleFlags PathFlags)
pathCommand =
  CommandUI
    { commandName = "path"
    , commandSynopsis = "Query for simple project information"
    , commandDescription = Just $ \_ ->
        wrapText $
          "Query for configuration and project information such as project GHC.\n"
            <> "The output order of query keys is implementation defined and should not be relied on.\n"
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          <> "  "
          <> pname
          <> " path --store-dir\n"
          <> "    Print the store-dir location of cabal.\n"
          <> "  "
          <> pname
          <> " path --output-format=json --compiler-info\n"
          <> "    Print compiler information in json format.\n"
          <> "  "
          <> pname
          <> " path --output-format=json --installdir --compiler-info\n"
          <> "    Print compiler information and installation directory in json format.\n"
          <> "  "
          <> pname
          <> " path --output-format=key-value --installdir\n"
          <> "    Print the installation directory, taking project information into account.\n"
          <> "  "
          <> pname
          <> " path -z --output-format=key-value --installdir\n"
          <> "    Print the installation directory, without taking project information into account.\n"
    , commandUsage = \pname ->
        "Usage: " <> pname <> " path [FLAGS]\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultPathFlags
    , commandOptions = nixStyleOptions pathOptions
    }

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

data PathOutputFormat
  = JSON
  | KeyValue
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PathFlags = PathFlags
  { pathCompiler :: Flag Bool
  , pathOutputFormat :: Flag PathOutputFormat
  , pathDirectories :: Flag [ConfigPath]
  }
  deriving (Eq, Show)

defaultPathFlags :: PathFlags
defaultPathFlags =
  PathFlags
    { pathCompiler = mempty
    , pathOutputFormat = mempty
    , pathDirectories = mempty
    }

pathOutputFormatParser :: ReadE (Flag PathOutputFormat)
pathOutputFormatParser = ReadE $ \case
  "json" -> Right $ Flag JSON
  "key-value" -> Right $ Flag KeyValue
  policy ->
    Left $
      "Cannot parse the status output format '"
        <> policy
        <> "'"

pathOutputFormatString :: PathOutputFormat -> String
pathOutputFormatString JSON = "json"
pathOutputFormatString KeyValue = "key-value"

pathOutputFormatPrinter
  :: Flag PathOutputFormat -> [String]
pathOutputFormatPrinter = \case
  (Flag format) -> [pathOutputFormatString format]
  NoFlag -> []

pathOptions :: ShowOrParseArgs -> [OptionField PathFlags]
pathOptions showOrParseArgs =
  [ option
      []
      ["output-format"]
      "Output format of the requested path locations"
      pathOutputFormat
      (\v flags -> flags{pathOutputFormat = v})
      ( reqArg
          (intercalate "|" $ map pathOutputFormatString [minBound .. maxBound])
          pathOutputFormatParser
          pathOutputFormatPrinter
      )
  , option
      []
      ["compiler-info"]
      "Print information of the project compiler"
      pathCompiler
      (\v flags -> flags{pathCompiler = v})
      (yesNoOpt showOrParseArgs)
  ]
    <> map pathOption [minBound .. maxBound]
  where
    pathOption s =
      option
        []
        [pathName s]
        ("Print cabal's " <> pathName s)
        pathDirectories
        (\v flags -> flags{pathDirectories = Flag $ concat (flagToList (pathDirectories flags) <> flagToList v)})
        (noArg (Flag [s]))

-- | A path that can be retrieved by the @cabal path@ command.
data ConfigPath
  = ConfigPathCacheHome
  | ConfigPathRemoteRepoCache
  | ConfigPathLogsDir
  | ConfigPathStoreDir
  | ConfigPathConfigFile
  | ConfigPathInstallDir
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The configuration name for this path.
pathName :: ConfigPath -> String
pathName ConfigPathCacheHome = "cache-home"
pathName ConfigPathRemoteRepoCache = "remote-repo-cache"
pathName ConfigPathLogsDir = "logs-dir"
pathName ConfigPathStoreDir = "store-dir"
pathName ConfigPathConfigFile = "config-file"
pathName ConfigPathInstallDir = "installdir"

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

-- | Entry point for the 'path' command.
pathAction :: NixStyleFlags PathFlags -> [String] -> GlobalFlags -> IO ()
pathAction flags@NixStyleFlags{extraFlags = pathFlags', ..} cliTargetStrings globalFlags = withContextAndSelectors AcceptNoTargets Nothing flags [] globalFlags OtherCommand $ \_ baseCtx _ -> do
  let pathFlags =
        if pathCompiler pathFlags' == NoFlag && pathDirectories pathFlags' == NoFlag
          then -- if not a single key to query is given, query everything!

            pathFlags'
              { pathCompiler = Flag True
              , pathDirectories = Flag [minBound .. maxBound]
              }
          else pathFlags'
  when (not $ null cliTargetStrings) $
    dieWithException verbosity CmdPathAcceptsNoTargets
  when (buildSettingDryRun (buildSettings baseCtx)) $
    dieWithException verbosity CmdPathCommandDoesn'tSupportDryRun

  compilerPathOutputs <-
    if not $ fromFlagOrDefault False (pathCompiler pathFlags)
      then pure Nothing
      else do
        (compiler, _, progDb) <- runRebuild (distProjectRootDirectory . distDirLayout $ baseCtx) $ configureCompiler verbosity (distDirLayout baseCtx) (projectConfig baseCtx)
        compilerProg <- requireCompilerProg verbosity compiler
        (configuredCompilerProg, _) <- requireProgram verbosity compilerProg progDb
        pure $ Just $ mkCompilerInfo configuredCompilerProg compiler

  paths <- for (fromFlagOrDefault [] $ pathDirectories pathFlags) $ \p -> do
    t <- getPathLocation baseCtx p
    pure (pathName p, t)

  let pathOutputs =
        PathOutputs
          { pathOutputsCompilerInfo = compilerPathOutputs
          , pathOutputsConfigPaths = paths
          }

  let output = case fromFlagOrDefault KeyValue (pathOutputFormat pathFlags) of
        JSON ->
          Json.encodeToString (showAsJson pathOutputs) <> "\n"
        KeyValue -> do
          showAsKeyValuePair pathOutputs

  putStr $ withOutputMarker verbosity output
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- | Find the FilePath location for common configuration paths.
--
-- TODO: this should come from a common source of truth to avoid code path divergence
getPathLocation :: ProjectBaseContext -> ConfigPath -> IO FilePath
getPathLocation _ ConfigPathCacheHome =
  defaultCacheHome
getPathLocation baseCtx ConfigPathRemoteRepoCache =
  pure $ buildSettingCacheDir (buildSettings baseCtx)
getPathLocation baseCtx ConfigPathLogsDir =
  pure $ cabalLogsDirectory (cabalDirLayout baseCtx)
getPathLocation baseCtx ConfigPathStoreDir =
  fromFlagOrDefault
    defaultStoreDir
    (pure <$> projectConfigStoreDir (projectConfigShared (projectConfig baseCtx)))
getPathLocation baseCtx ConfigPathConfigFile =
  getConfigFilePath (projectConfigConfigFile (projectConfigShared (projectConfig baseCtx)))
getPathLocation baseCtx ConfigPathInstallDir =
  fromFlagOrDefault
    defaultInstallPath
    (pure <$> cinstInstalldir (projectConfigClientInstallFlags $ projectConfigBuildOnly (projectConfig baseCtx)))

-- ----------------------------------------------------------------------------
-- Helpers for determining compiler information
-- ----------------------------------------------------------------------------

requireCompilerProg :: Verbosity -> Compiler -> IO Program
requireCompilerProg verbosity compiler =
  case compilerFlavor compiler of
    GHC -> pure ghcProgram
    GHCJS -> pure ghcjsProgram
    flavour ->
      die' verbosity $
        "path: Unsupported compiler flavour: "
          <> prettyShow flavour

-- ----------------------------------------------------------------------------
-- Output
-- ----------------------------------------------------------------------------

data PathOutputs = PathOutputs
  { pathOutputsCompilerInfo :: Maybe PathCompilerInfo
  , pathOutputsConfigPaths :: [(String, FilePath)]
  }
  deriving (Show, Eq, Ord)

data PathCompilerInfo = PathCompilerInfo
  { pathCompilerInfoFlavour :: CompilerFlavor
  , pathCompilerInfoId :: CompilerId
  , pathCompilerInfoPath :: FilePath
  }
  deriving (Show, Eq, Ord)

mkCompilerInfo :: ConfiguredProgram -> Compiler -> PathCompilerInfo
mkCompilerInfo compilerProgram compiler =
  PathCompilerInfo
    { pathCompilerInfoFlavour = compilerFlavor compiler
    , pathCompilerInfoId = compilerId compiler
    , pathCompilerInfoPath = programPath compilerProgram
    }

-- ----------------------------------------------------------------------------
-- JSON
-- ----------------------------------------------------------------------------

showAsJson :: PathOutputs -> Json.Value
showAsJson pathOutputs =
  let
    cabalInstallJson =
      Json.object
        [ "cabal-version" .= jdisplay cabalInstallVersion
        ]

    compilerInfoJson = case pathOutputsCompilerInfo pathOutputs of
      Nothing -> Json.object []
      Just pci -> compilerInfoToJson pci

    pathsJson = Json.object $ map (\(k, v) -> k .= Json.String v) (pathOutputsConfigPaths pathOutputs)
   in
    mergeJsonObjects $
      [ cabalInstallJson
      , compilerInfoJson
      , pathsJson
      ]

jdisplay :: Pretty a => a -> Json.Value
jdisplay = Json.String . prettyShow

mergeJsonObjects :: [Json.Value] -> Json.Value
mergeJsonObjects = Json.object . foldl' go []
  where
    go acc (Json.Object objs) =
      acc <> objs
    go _ _ =
      error "mergeJsonObjects: Only objects can be merged"

compilerInfoToJson :: PathCompilerInfo -> Json.Value
compilerInfoToJson pci =
  Json.object
    [ "compiler"
        .= Json.object
          [ "flavour" .= jdisplay (pathCompilerInfoFlavour pci)
          , "id" .= jdisplay (pathCompilerInfoId pci)
          , "path" .= Json.String (pathCompilerInfoPath pci)
          ]
    ]

-- ----------------------------------------------------------------------------
-- Key Value Pair outputs
-- ----------------------------------------------------------------------------

showAsKeyValuePair :: PathOutputs -> String
showAsKeyValuePair pathOutputs =
  let
    cInfo = case pathOutputsCompilerInfo pathOutputs of
      Nothing -> []
      Just pci -> compilerInfoToKeyValue pci

    paths = pathOutputsConfigPaths pathOutputs

    pairs = cInfo <> paths

    showPair (k, v) = k <> ": " <> v
   in
    case pairs of
      [(_, v)] -> v
      xs -> unlines $ map showPair xs

compilerInfoToKeyValue :: PathCompilerInfo -> [(String, String)]
compilerInfoToKeyValue pci =
  [ ("compiler-flavour", prettyShow $ pathCompilerInfoFlavour pci)
  , ("compiler-id", prettyShow $ pathCompilerInfoId pci)
  , ("compiler-path", pathCompilerInfoPath pci)
  ]
