-- | cabal-install CLI command: build
--
module Distribution.Client.CmdWriteAutogenFiles (
    -- * The @build@ CLI and action
    writeAutogenFilesCommand,
    writeAutogenFilesAction
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdInstall.ClientInstallFlags

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags, WriteAutogenFilesFlags(..) )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault, Flag(..), TestFlags )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText, die' )
import Distribution.Simple.Configure (tryGetPersistBuildConfig)

import qualified Data.Map as Map
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Client.SetupWrapper
import Distribution.Simple.Program ( defaultProgramDb )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.ProjectPlanning (
  setupHsScriptOptions, setupHsConfigureFlags, setupHsConfigureArgs
  )
import Distribution.Client.DistDirLayout (distBuildDirectory)
import Distribution.Client.Types ( PackageLocation(..), GenericReadyPackage(..) )
import Distribution.Client.JobControl (newLock, Lock)

writeAutogenFilesCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, TestFlags)
writeAutogenFilesCommand = Client.installCommand {
  commandName         = "new-write-autogen-files",
  commandSynopsis     = "",
  commandUsage        = usageAlternatives "new-write-autogen-files" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $ 
        "Generate and write out the Paths_<pkg>.hs and cabal_macros.h files\n"
     ++ "for all components in the project",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-write-autogen-files\n"
     ++ "    Write for all packages in the project\n"
  }

writeAutogenFilesAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, TestFlags) 
                            -> [String] -> GlobalFlags -> IO ()
writeAutogenFilesAction (configFlags, configExFlags, installFlags, haddockFlags, testFlags)
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
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
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

          let elaboratedPlan' = pruneInstallPlanToTargets
                                  TargetActionBuild
                                  targets
                                  elaboratedPlan
          elaboratedPlan'' <-
            if buildSettingOnlyDeps (buildSettings baseCtx')
              then either (reportCannotPruneDependencies verbosity) return $
                    pruneInstallPlanToDependencies (Map.keysSet targets)
                                                  elaboratedPlan'
              else return elaboratedPlan'

          return (elaboratedPlan'', targets)

    scriptLock <- newLock
    writeAutogenFiles verbosity baseCtx' buildCtx scriptLock (configured buildCtx)
    
    where
      verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
      cliConfig = commandLineFlagsToProjectConfig
                    globalFlags configFlags configExFlags
                    installFlags defaultClientInstallFlags
                    haddockFlags
                    testFlags
      configured ctx = [p | InstallPlan.Configured p <- InstallPlan.toList (elaboratedPlanToExecute ctx)]


writeAutogenFiles :: Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> [ElaboratedConfiguredPackage] -> IO ()
writeAutogenFiles verbosity baseCtx buildCtx lock pkgs = mapM_ runWrapper pkgs
  where runWrapper pkg = do
          let shared = elaboratedShared buildCtx
              install = elaboratedPlanOriginal buildCtx
              dirLayout = distDirLayout baseCtx
              buildDir = distBuildDirectory dirLayout (elabDistDirParams shared pkg)
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
          --Configure the package if there's no existing config, 
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
            (Cabal.writeAutogenFilesCommand defaultProgramDb) 
            (const $ WriteAutogenFilesFlags (Flag buildDir) (Flag verbosity))
            (const [])

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

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
    die' verbosity . renderCannotPruneDependencies

