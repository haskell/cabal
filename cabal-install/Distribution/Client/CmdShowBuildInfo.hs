{-# LANGUAGE RecordWildCards #-}
-- | cabal-install CLI command: show-build-info
--
module Distribution.Client.CmdShowBuildInfo (
    -- * The @show-build-info@ CLI and action
    showBuildInfoCommand,
    showBuildInfoAction
  ) where

import Distribution.Client.Compat.Prelude
         ( when, find, fromMaybe )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
import Distribution.Client.TargetProblem
         ( TargetProblem (..), TargetProblem' )

import Distribution.Client.Setup
         ( GlobalFlags )
import Distribution.Simple.Setup
         (Flag(..), haddockVerbosity, configVerbosity, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), option, reqArg', usageAlternatives )
import Distribution.Verbosity
         ( Verbosity, silent )
import Distribution.Simple.Utils
         ( wrapText, die' )
import Distribution.Types.UnitId
         ( UnitId, mkUnitId )
import Distribution.Types.Version
         ( mkVersion )
import Distribution.Types.PackageDescription
         ( buildType )
import Distribution.Pretty
         ( prettyShow )

import qualified Data.Map as Map
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Client.SetupWrapper
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.ProjectPlanning
        ( setupHsConfigureFlags, setupHsConfigureArgs, setupHsBuildFlags
        , setupHsScriptOptions )
import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.DistDirLayout
        ( distBuildDirectory )
import Distribution.Client.Types
        ( PackageLocation(..), GenericReadyPackage(..) )
import Distribution.Client.JobControl
        ( newLock, Lock )
import Distribution.Simple.Configure
        (getPersistBuildConfig,  tryGetPersistBuildConfig )

import Distribution.Simple.ShowBuildInfo
import Distribution.Utils.Json

import Distribution.Simple.BuildTarget (readTargetInfos)
import Distribution.Types.LocalBuildInfo (neededTargetsInBuildOrder')
import Distribution.Compat.Graph (IsNode(nodeKey))
import Distribution.Simple.Setup (BuildFlags(buildArgs))
import Distribution.Types.TargetInfo (TargetInfo(targetCLBI))

showBuildInfoCommand :: CommandUI (NixStyleFlags ShowBuildInfoFlags)
showBuildInfoCommand = CommandUI {
  commandName         = "show-build-info",
  commandSynopsis     = "Show project build information",
  commandUsage        = usageAlternatives "show-build-info" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Provides detailed json output for the given package.\n"
     ++ "Contains information about the different build components and compiler flags.\n",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " show-build-info\n"
     ++ "    Shows build information about the current package\n"
     ++ "  " ++ pname ++ " show-build-info .\n"
     ++ "    Shows build information about the current package\n"
     ++ "  " ++ pname ++ " show-build-info ./pkgname \n"
     ++ "    Shows build information about the package located in './pkgname'\n"
     ++ cmdCommonHelpTextNewBuildBeta,
  commandOptions = nixStyleOptions $ \_ ->
      [ option [] ["buildinfo-json-output"]
              "Write the result to the given file instead of stdout"
              buildInfoOutputFile (\pf flags -> flags { buildInfoOutputFile = pf })
              (reqArg' "FILE" Just (maybe [] pure)),
        option [] ["unit-ids-json"]
              "Show build-info only for selected unit-id's."
              buildInfoUnitIds (\pf flags -> flags { buildInfoUnitIds = pf })
              (reqArg' "UNIT-ID" (Just . words) (fromMaybe []))
      ],
  commandDefaultFlags = defaultNixStyleFlags defaultShowBuildInfoFlags
  }

data ShowBuildInfoFlags = ShowBuildInfoFlags
    { buildInfoOutputFile :: Maybe FilePath
    , buildInfoUnitIds :: Maybe [String]
    }

defaultShowBuildInfoFlags :: ShowBuildInfoFlags
defaultShowBuildInfoFlags = ShowBuildInfoFlags
    { buildInfoOutputFile = Nothing
    , buildInfoUnitIds = Nothing
    }

-- | The @show-build-info@ exports information about a package and the compiler
-- configuration used to build it as JSON, that can be used by other tooling.
-- See "Distribution.Simple.ShowBuildInfo" for more information.
showBuildInfoAction :: NixStyleFlags ShowBuildInfoFlags -> [String] -> GlobalFlags -> IO ()
showBuildInfoAction flags@NixStyleFlags { extraFlags = (ShowBuildInfoFlags fileOutput unitIds), ..}
  targetStrings globalFlags = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand
  let baseCtx' = baseCtx
        { buildSettings = (buildSettings baseCtx) { buildSettingDryRun = True }
        }

  targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                  =<< readTargetSelectors (localPackages baseCtx') Nothing flags targetStrings

  buildCtx <-
    runProjectPreBuildPhase verbosity baseCtx' $ \elaboratedPlan -> do
      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).
      targets <- either (reportShowBuildInfoTargetProblems verbosity) return
                $ resolveTargets
                    selectPackageTargets
                    selectComponentTarget
                    elaboratedPlan
                    Nothing
                    targetSelectors

      let elaboratedPlan' = pruneInstallPlanToTargets
                        TargetActionBuild
                        targets
                        elaboratedPlan

      -- This will be the build plan for building the dependencies required.
      elaboratedPlan'' <- either (die' verbosity . renderCannotPruneDependencies) return
                          $ pruneInstallPlanToDependencies
                              (Map.keysSet targets) elaboratedPlan'

      return (elaboratedPlan'', targets)

  buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
  runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

  scriptLock <- newLock
  showTargets fileOutput unitIds verbosity baseCtx' buildCtx scriptLock
  where
    -- Default to silent verbosity otherwise it will pollute our json output
    verbosity = fromFlagOrDefault silent (configVerbosity configFlags)
    -- Also shut up haddock since it dumps warnings to stdout
    flags' = flags { haddockFlags = haddockFlags { haddockVerbosity = Flag silent } }
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags'
                  mempty -- ClientInstallFlags, not needed here

showTargets :: Maybe FilePath -> Maybe [String] -> Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> IO ()
showTargets fileOutput unitIds verbosity baseCtx buildCtx lock = do

  -- TODO: can we use --disable-per-component so that we only get one package?
  let configured = [p | InstallPlan.Configured p <- InstallPlan.toList (elaboratedPlanOriginal buildCtx)]
      targets = maybe (fst <$> (Map.toList . targetsMap $ buildCtx)) (map mkUnitId) unitIds

  components <- concat <$> mapM (getComponentInfo verbosity baseCtx buildCtx
                                  lock configured) targets

  let compilerInfo = mkCompilerInfo (pkgConfigCompilerProgs (elaboratedShared buildCtx))
                                    (pkgConfigCompiler (elaboratedShared buildCtx))

      json = mkBuildInfo' compilerInfo components
      res = renderJson json ""

  case fileOutput of
    Nothing -> putStrLn res
    Just fp -> writeFile fp res

getComponentInfo :: Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> [ElaboratedConfiguredPackage] -> UnitId -> IO [Json]
getComponentInfo verbosity baseCtx buildCtx lock pkgs targetUnitId =
  case mbPkg of
    Nothing -> die' verbosity $ "No unit " ++ prettyShow targetUnitId
    Just pkg -> do
      let shared = elaboratedShared buildCtx
          install = elaboratedPlanOriginal buildCtx
          dirLayout = distDirLayout baseCtx
          buildDir = distBuildDirectory dirLayout (elabDistDirParams shared pkg)
          buildType' = buildType (elabPkgDescription pkg)
          flags = setupHsBuildFlags pkg shared verbosity buildDir
          srcDir = case (elabPkgSourceLocation pkg) of
            LocalUnpackedPackage fp -> fp
            _ -> ""
          scriptOptions = setupHsScriptOptions
              (ReadyPackage pkg)
              install
              shared
              dirLayout
              srcDir
              buildDir
              False
              lock
          configureFlags = setupHsConfigureFlags (ReadyPackage pkg) shared verbosity buildDir
          configureArgs = setupHsConfigureArgs pkg

      -- Check cabal version is correct
      (cabalVersion, _, _) <- getSetupMethod verbosity scriptOptions
                                            (elabPkgDescription pkg) buildType'
      when (cabalVersion < mkVersion [3, 0, 0, 0])
        ( die' verbosity $ "Only a Cabal version >= 3.0.0.0 is supported for this command.\n"
              ++ "Found version: " ++ prettyShow cabalVersion ++ "\n"
              ++ "For component: " ++ prettyShow targetUnitId
        )
      -- Configure the package if there's no existing config
      lbi' <- tryGetPersistBuildConfig buildDir
      case lbi' of
        Left _ -> setupWrapper
                    verbosity
                    scriptOptions
                    (Just $ elabPkgDescription pkg)
                    (Cabal.configureCommand
                      (pkgConfigCompilerProgs (elaboratedShared buildCtx)))
                    (const configureFlags)
                    (const configureArgs)
        Right _ -> pure ()

      -- Do the bit the Cabal library would normally do here
      lbi <- getPersistBuildConfig buildDir
      let pkgDesc = elabPkgDescription pkg
      targets <- readTargetInfos verbosity pkgDesc lbi (buildArgs flags)
      let targetsToBuild = neededTargetsInBuildOrder' pkgDesc lbi (map nodeKey targets)
      return $ map (mkComponentInfo pkgDesc lbi . targetCLBI) targetsToBuild

    where
      mbPkg :: Maybe ElaboratedConfiguredPackage
      mbPkg = find ((targetUnitId ==) . elabUnitId) pkgs

-- | This defines what a 'TargetSelector' means for the @show-build-info@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @show-build-info@ command select all components. Unlike the @build@
-- command, we want to show info for tests and benchmarks even without the
-- @--enable-tests@\/@--enable-benchmarks@ flag set.
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
    targetsBuildable = selectBuildableTargets targets

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @show-build-info@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic


reportShowBuildInfoTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportShowBuildInfoTargetProblems verbosity problems =
  reportTargetProblems verbosity "show-build-info" problems
