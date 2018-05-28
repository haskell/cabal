-- | cabal-install CLI command: build
--
module Distribution.Client.CmdShowBuildInfo (
    -- * The @build@ CLI and action
    showBuildInfoCommand,
    showBuildInfoAction
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdInstall.ClientInstallFlags

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault, TestFlags )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Verbosity
         ( Verbosity, silent )
import Distribution.Simple.Utils
         ( wrapText, die')
import Distribution.Types.UnitId (UnitId)

import qualified Data.Map as Map
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Client.SetupWrapper
import Distribution.Simple.Program ( defaultProgramDb )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.ProjectPlanning (
  setupHsConfigureFlags, setupHsConfigureArgs,
  setupHsBuildFlags, setupHsBuildArgs, 
  setupHsScriptOptions
  )
import Distribution.Client.DistDirLayout (distBuildDirectory)
import Distribution.Client.Types ( PackageLocation(..), GenericReadyPackage(..) )
import Distribution.Client.JobControl (newLock, Lock)
import Distribution.Simple.Configure (tryGetPersistBuildConfig)
import Data.List (find)

showBuildInfoCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, TestFlags)
showBuildInfoCommand = Client.installCommand {
  commandName         = "new-show-build-info",
  commandSynopsis     = "Show project build information",
  commandUsage        = usageAlternatives "new-show-build-info" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Build one or more targets from within the project. The available "
     ++ "targets are the packages in the project as well as individual "
     ++ "components within those packages, including libraries, executables, "
     ++ "test-suites or benchmarks. Targets can be specified by name or "
     ++ "location. If no target is specified then the default is to build "
     ++ "the package in the current directory.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-build\n"
     ++ "    Build the package in the current directory or all packages in the project\n"
     ++ "  " ++ pname ++ " new-build pkgname\n"
     ++ "    Build the package named pkgname in the project\n"
     ++ "  " ++ pname ++ " new-build ./pkgfoo\n"
     ++ "    Build the package in the ./pkgfoo directory\n"
     ++ "  " ++ pname ++ " new-build cname\n"
     ++ "    Build the component named cname module Distribution.Client.InstallPlanin the project\n"
     ++ "  " ++ pname ++ " new-build cname --module Distribution.Client.InstallPlanenable-profiling\n"
     ++ "    Build the component in profilingmodule Distribution.Client.InstallPlan mode (including dependencies as needed)\n\n"

     ++ cmdCommonHelpTextNewBuildBeta
   }


-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
showBuildInfoAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, TestFlags)
            -> [String] -> GlobalFlags -> IO ()
showBuildInfoAction (configFlags, configExFlags, installFlags, haddockFlags, testFlags)
            targetStrings globalFlags = do

  baseCtx <- establishProjectBaseContext verbosity cliConfig
  let baseCtx' = baseCtx {
                    buildSettings = (buildSettings baseCtx) {
                      buildSettingDryRun = True
                    }
                  }

  targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                  =<< readTargetSelectors (localPackages baseCtx') Nothing targetStrings

  buildCtx <-
    runProjectPreBuildPhase verbosity baseCtx' $ \elaboratedPlan -> do
      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).
      targets <- either (reportTargetProblems verbosity) return
                $ resolveTargets
                    selectPackageTargets
                    selectComponentTarget
                    TargetProblemCommon
                    elaboratedPlan
                    Nothing
                    targetSelectors

      -- Don't prune the plan though, as we want a list of all configured packages
      return (elaboratedPlan, targets)

  scriptLock <- newLock
  showTargets verbosity baseCtx' buildCtx scriptLock
  
  where
    -- Default to silent verbosity otherwise it will pollute our json output
    verbosity = fromFlagOrDefault silent (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags defaultClientInstallFlags
                  haddockFlags
                  testFlags

-- Pretty nasty piecemeal out of json, but I can't see a way to retrieve output of the setupWrapper'd tasks
showTargets :: Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> IO ()
showTargets verbosity baseCtx buildCtx lock = do
  putStr "["
  mapM_ showSeparated (zip [0..] targets)
  putStrLn "]"
    where configured = [p | InstallPlan.Configured p <- InstallPlan.toList (elaboratedPlanOriginal buildCtx)]
          targets = fst <$> (Map.toList . targetsMap $ buildCtx)
          doShowInfo unitId = showInfo verbosity baseCtx buildCtx lock configured unitId
          showSeparated (idx, unitId)
              | idx == length targets - 1 = doShowInfo unitId
              | otherwise = doShowInfo unitId >> putStrLn "," 

showInfo :: Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> [ElaboratedConfiguredPackage] -> UnitId -> IO ()
showInfo verbosity baseCtx buildCtx lock pkgs targetUnitId
  | Nothing <- mbPkg = die' verbosity $ "No unit " ++ show targetUnitId 
  | Just pkg <- mbPkg = do
    let shared = elaboratedShared buildCtx
        install = elaboratedPlanOriginal buildCtx
        dirLayout = distDirLayout baseCtx
        buildDir = distBuildDirectory dirLayout (elabDistDirParams shared pkg)
        flags = setupHsBuildFlags pkg shared verbosity buildDir
        args = setupHsBuildArgs pkg
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
    --Configure the package if there's no existing config
    lbi <- tryGetPersistBuildConfig buildDir
    case lbi of
      Left _ -> setupWrapper 
                  verbosity 
                  scriptOptions 
                  (Just $ elabPkgDescription pkg) 
                  (Cabal.configureCommand defaultProgramDb) 
                  (const $ configureFlags)
                  (const configureArgs)
      Right _ -> pure ()
    setupWrapper 
      verbosity 
      scriptOptions 
      (Just $ elabPkgDescription pkg) 
      (Cabal.showBuildInfoCommand defaultProgramDb) 
      (const flags) 
      (const args)
    where mbPkg = find ((targetUnitId ==) . elabUnitId) pkgs

-- | This defines what a 'TargetSelector' means for the @write-autogen-files@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @write-autogen-files@ command select all components except non-buildable and disabled
-- tests\/benchmarks, fail if there are no such components
--
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either TargetProblem [k]
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
                      -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic subtarget


-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @build@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon

     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled TargetSelector [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   TargetSelector
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "build" problem
renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "build" targetSelector targets
renderTargetProblem(TargetProblemNoTargets targetSelector) =
    renderTargetProblemNoTargets "build" targetSelector