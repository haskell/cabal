{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.CmdStatus
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'status' command. Query for project information
-- such as targets in the project or which ghc version is going to be used
-- to build the project.
-----------------------------------------------------------------------------

module Distribution.Client.CmdStatus (
  statusCommand, statusAction,
  ) where

import qualified Data.Map as Map

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.DistDirLayout
import Distribution.Client.TargetProblem
import Distribution.Client.CmdErrorMessages
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), yesNoOpt )
import Distribution.Client.Utils.Json
         ( (.=) )
import qualified Distribution.Client.Utils.Json as Json
import Distribution.Client.Version
         ( cabalInstallVersion )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Parsec (parsecCommaList, parsecToken)
import Distribution.ReadE
         ( ReadE(ReadE), parsecToReadE )
import Distribution.Simple.BuildPaths (buildInfoPref)
import Distribution.Simple.Command
         ( CommandUI(..), option, reqArg, ShowOrParseArgs, OptionField )
import Distribution.Simple.Compiler
import Distribution.Simple.Program
import Distribution.Simple.Flag
         ( Flag(..), fromFlagOrDefault )
import Distribution.Simple.Utils
         ( wrapText, die', withOutputMarker, ordNub )
import Distribution.Verbosity
         ( normal )
import Distribution.Version

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

statusCommand :: CommandUI (NixStyleFlags StatusFlags)
statusCommand = CommandUI
  { commandName = "status"
  , commandSynopsis = "Query for simple project information"
  , commandDescription  = Just $ \_ -> wrapText $
      "Query for available targets and project information such as project GHC."
  , commandNotes = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " status --output-format=json --compiler-info\n"
     ++ "    Print the compiler that is used for this project in the json format.\n"
     ++ "  " ++ pname ++ " status --output-format=json --build-info=./src/Foo.hs\n"
     ++ "    Print the location of the component \"src/Foo.hs\" belongs to.\n"
     ++ "  " ++ pname ++ " status --output-format=json --build-info=./src/Foo.hs\n"
     ++ "    Print both, compiler information and build-info location for the given target.\n"
     ++ "  " ++ pname ++ " status --output-format=json --build-info=./src/Foo.hs --build-info=./test/Bar.hs\n"
     ++ "    Print build-info location for multiple targets.\n"
  , commandUsage = \pname ->
      "Usage: " ++ pname ++ " status [FLAGS]\n"
  , commandDefaultFlags = defaultNixStyleFlags defaultStatusFlags
  , commandOptions      = nixStyleOptions statusOptions

  }

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

data StatusOutputFormat
  = JSON
  deriving (Eq, Ord, Show, Read)

data StatusFlags = StatusFlags
  { statusBuildInfo :: [String]
  , statusCompiler :: Flag Bool
  , statusOutputFormat :: Flag StatusOutputFormat
  } deriving (Eq, Show, Read)

defaultStatusFlags :: StatusFlags
defaultStatusFlags = StatusFlags
  { statusBuildInfo = mempty
  , statusCompiler = mempty
  , statusOutputFormat = mempty
  }

statusOutputFormatParser :: ReadE (Flag StatusOutputFormat)
statusOutputFormatParser = ReadE $ \case
  "json" -> Right $ Flag JSON
  policy -> Left  $ "Cannot parse the status output format '"
            <> policy <> "'"

statusOutputFormatPrinter
  :: Flag StatusOutputFormat -> [String]
statusOutputFormatPrinter = \case
  (Flag JSON) -> ["json"]
  NoFlag      -> []

statusOptions :: ShowOrParseArgs -> [OptionField StatusFlags]
statusOptions showOrParseArgs =
  [ option [] ["output-format"]
    "Output Format for the information"
    statusOutputFormat (\v flags -> flags { statusOutputFormat = v })
    (reqArg "json"
      statusOutputFormatParser
      statusOutputFormatPrinter
    )
  , option [] ["build-info"]
    "List all available targets in the project"
    statusBuildInfo (\v flags -> flags { statusBuildInfo = v ++ statusBuildInfo flags})
    (reqArg "TARGET" buildInfoTargetReadE (fmap show))
  , option [] ["compiler-info"]
    "Print information of the project compiler"
    statusCompiler (\v flags -> flags { statusCompiler = v })
    (yesNoOpt showOrParseArgs)
  ]
  where
    buildInfoTargetReadE :: ReadE [String]
    buildInfoTargetReadE =
      parsecToReadE
        -- This error should never be shown
        ("couldn't parse targets: " ++)
        -- TODO: wrong parser, kills filepaths with spaces
        (parsecCommaList parsecToken)

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

-- | Entry point for the 'status' command.
statusAction :: NixStyleFlags StatusFlags -> [String] -> GlobalFlags -> IO ()
statusAction flags@NixStyleFlags { extraFlags = statusFlags, ..} cliTargetStrings globalFlags = do
  when (NoFlag == statusOutputFormat statusFlags) $ do
    die' verbosity "The status command requires the flag '--output-format'."
  when (not $ null cliTargetStrings) $
    die' verbosity "The status command takes not target arguments directly. Use appropriate flags to pass in target information."

  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand
  (_, elaboratedPlan, elabSharedConfig, _, _) <-
    rebuildInstallPlan verbosity
                        (distDirLayout baseCtx)
                        (cabalDirLayout baseCtx)
                        (projectConfig baseCtx)
                        (localPackages baseCtx)

  let initialJson = Json.object
        [ "cabal-version" .= jdisplay cabalInstallVersion
        ]

  compilerJson <- if not $ fromFlagOrDefault False (statusCompiler statusFlags)
    then pure $ Json.object [] -- Neutral element
    else do
      let compiler = pkgConfigCompiler elabSharedConfig
      compilerProg <- requireCompilerProg verbosity compiler
      let progDb = pkgConfigCompilerProgs elabSharedConfig
      (configuredCompilerProg, _) <- requireProgram verbosity compilerProg progDb
      pure $ mkCompilerInfo configuredCompilerProg compiler

  buildInfoJson <- if null (statusBuildInfo statusFlags)
    then pure $ Json.object [] -- Neutral element
    else do
      let targetStrings = statusBuildInfo statusFlags
      targetSelectors <- readTargetSelectors (localPackages baseCtx) Nothing targetStrings >>= \case
        Left err   -> reportTargetSelectorProblems verbosity err
        Right sels -> pure sels

      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).
      -- TODO: don't throw on targets that are invalid.
      targets <- either (reportBuildTargetProblems verbosity) return
                $ resolveTargets
                    selectPackageTargets
                    selectComponentTarget
                    elaboratedPlan
                    Nothing
                    targetSelectors

      pure $ mkBuildInfoJson (distDirLayout baseCtx) elabSharedConfig
              elaboratedPlan targets targetSelectors targetStrings

  let statusJson = mergeJsonObjects [initialJson, compilerJson, buildInfoJson]

  -- Final output
  putStrLn $ withOutputMarker verbosity $ Json.encodeToString statusJson
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty

-- ----------------------------------------------------------------------------
-- Helpers for determining and serialising compiler information
-- ----------------------------------------------------------------------------

requireCompilerProg :: Verbosity -> Compiler -> IO Program
requireCompilerProg verbosity compiler =
  case compilerFlavor compiler of
    GHC -> pure ghcProgram
    GHCJS -> pure ghcjsProgram
    flavour -> die' verbosity $
      "status: Unsupported compiler flavour: "
      <> prettyShow flavour

mkCompilerInfo :: ConfiguredProgram -> Compiler -> Json.Value
mkCompilerInfo compilerProgram compiler =
  Json.object
    [ "compiler" .= Json.object
        [ "flavour"     .= Json.String (prettyShow $ compilerFlavor compiler)
        , "compiler-id" .= Json.String (showCompilerId compiler)
        , "path"        .= Json.String (programPath compilerProgram)
        ]
    ]

-- ----------------------------------------------------------------------------
-- Helpers for determining and serialising build info
-- ----------------------------------------------------------------------------

mkBuildInfoJson :: DistDirLayout -> ElaboratedSharedConfig -> ElaboratedInstallPlan -> TargetsMap -> [TargetSelector] -> [String] -> Json.Value
mkBuildInfoJson distDirLayout elaboratedSharedConfig elaboratedPlan targetsMap targetSelectors targetStrings = Json.object
  [ "build-info" .= Json.Array allTargetsJsons
  ]
  where
    allTargetsJsons =
      [ planPackageToJ elab ts
      | (uid, elab) <- Map.assocs subsetInstallPlan
      , (_, tss) <- targetsMap Map.! uid
      , ts <- ordNub $ toList tss
      ]

    subsetInstallPlan = Map.restrictKeys (InstallPlan.toMap elaboratedPlan) (Map.keysSet targetsMap)

    targetsTable = Map.fromList $ zip targetSelectors targetStrings

    tsToOriginalTarget ts = targetsTable Map.! ts

    planPackageToJ :: ElaboratedPlanPackage -> TargetSelector -> Json.Value
    planPackageToJ pkg ts =
      case pkg of
        InstallPlan.PreExisting ipi -> installedPackageInfoToJ ipi
        InstallPlan.Configured elab -> elaboratedPackageToJ elab ts
        InstallPlan.Installed  elab -> elaboratedPackageToJ elab ts
        -- Note that the --build-info currently only uses the elaborated plan,
        -- not the improved plan. So we will not get the Installed state for
        -- that case, but the code supports it in case we want to use this
        -- later in some use case where we want the status of the build.

    -- TODO: what should we do if we run in this case?
    -- Happens on `--build-info=containers` while we are not in the containers project.
    installedPackageInfoToJ :: InstalledPackageInfo -> Json.Value
    installedPackageInfoToJ _ipi =
      -- Pre-existing packages lack configuration information such as their flag
      -- settings or non-lib components. We only get pre-existing packages for
      -- the global/core packages however, so this isn't generally a problem.
      -- So these packages are never local to the project.
      --
      Json.object []

    elaboratedPackageToJ :: ElaboratedConfiguredPackage -> TargetSelector -> Json.Value
    elaboratedPackageToJ elab ts = Json.object
      [ "target" .= Json.String (tsToOriginalTarget ts)
      , "path" .= maybe Json.Null Json.String buildInfoFileLocation
      ]
      where
      dist_dir :: FilePath
      dist_dir = distBuildDirectory distDirLayout
                    (elabDistDirParams elaboratedSharedConfig elab)

      -- | Only add build-info file location if the Setup.hs CLI
      -- is recent enough to be able to generate build info files.
      -- Otherwise, write 'null'.
      --
      -- Consumers of `status` can use the nullability of this file location
      -- to indicate that the given component uses `build-type: Custom`
      -- with an old lib:Cabal version.
      buildInfoFileLocation :: Maybe FilePath
      buildInfoFileLocation
        | elabSetupScriptCliVersion elab < mkVersion [3, 7, 0, 0]
        = Nothing
        | otherwise
        = Just (buildInfoPref dist_dir)

-- ----------------------------------------------------------------------------
-- Target selectors and helpers
-- ----------------------------------------------------------------------------

-- | This defines what a 'TargetSelector' means for the @status@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @status@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
--
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either TargetProblem' [k]
selectPackageTargets targetSelector targets

    -- If there are any buildable targets then we select those
  | not (null targetsBuildable)
  = Right targetsBuildable

    -- If there are targets but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targets')

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targets'         = forgetTargetsDetail targets
    targetsBuildable = selectBuildableTargetsWith
                         (buildable targetSelector)
                         targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems =
  reportTargetProblems verbosity "status" problems

-- ----------------------------------------------------------------------------
-- JSON serialisation helpers
-- ----------------------------------------------------------------------------

jdisplay :: Pretty a => a -> Json.Value
jdisplay = Json.String . prettyShow

mergeJsonObjects :: [Json.Value] -> Json.Value
mergeJsonObjects = Json.object . foldl' go []
  where
    go acc (Json.Object objs) =
      acc <> objs
    go _   _                  =
      error "mergeJsonObjects: Only objects can be merged"
