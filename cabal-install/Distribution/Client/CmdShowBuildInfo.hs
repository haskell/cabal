-- | cabal-install CLI command: new-show-build-info
--
module Distribution.Client.CmdShowBuildInfo where
-- (
--     -- * The @show-build-info@ CLI and action
--     showBuildInfoCommand,
--     showBuildInfoAction
--   )


import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdInstall.ClientInstallFlags

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags
         )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, TestFlags
         , fromFlagOrDefault
         )
import Distribution.Simple.Command
         ( CommandUI(..), option, reqArg', usageAlternatives
         )
import Distribution.Verbosity
         ( Verbosity, silent )
import Distribution.Simple.Utils
         ( wrapText, die')
import Distribution.Types.UnitId (UnitId, mkUnitId)
import Distribution.Deprecated.Text (display)

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
import qualified Distribution.Client.CmdInstall as CmdInstall
import Data.List (find)
import Data.Maybe (fromMaybe)

showBuildInfoCommand :: CommandUI ShowBuildInfoFlags
showBuildInfoCommand = CmdInstall.installCommand {
  commandName         = "new-show-build-info",
  commandSynopsis     = "Show project build information",
  commandUsage        = usageAlternatives "new-show-build-info" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Provides detailed json output for the given package.\n"
     ++ "Contains information about the different build components and compiler flags.\n",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-show-build-info\n"
     ++ "    Shows build information about the current package\n"
     ++ "  " ++ pname ++ " new-show-build-info .\n"
     ++ "    Shows build information about the current package\n"
     ++ "  " ++ pname ++ " new-show-build-info ./pkgname \n"
     ++ "    Shows build information about the package located in './pkgname'\n"
     ++ cmdCommonHelpTextNewBuildBeta,
  commandOptions = \showOrParseArgs ->
      Client.liftOptions buildInfoInstallCommandFlags (\pf flags -> flags { buildInfoInstallCommandFlags = pf }) (commandOptions CmdInstall.installCommand showOrParseArgs)
      ++
      [ option [] ["buildinfo-json-output"]
              "Write the result to the given file instead of stdout"
              buildInfoOutputFile (\pf flags -> flags { buildInfoOutputFile = pf })
              (reqArg' "FILE" Just (maybe [] pure)),
        option [] ["unit-ids-json"]
              "Show build-info only for selected unit-id's."
              buildInfoUnitIds (\pf flags -> flags { buildInfoUnitIds = pf })
              (reqArg' "UNIT-ID" (Just . words) (fromMaybe []))
      ],
  commandDefaultFlags = defaultShowBuildInfoFlags

   }

data ShowBuildInfoFlags = ShowBuildInfoFlags
    { buildInfoInstallCommandFlags :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, TestFlags, ClientInstallFlags)
    , buildInfoOutputFile :: Maybe FilePath
    , buildInfoUnitIds :: Maybe [String]
    }

defaultShowBuildInfoFlags :: ShowBuildInfoFlags
defaultShowBuildInfoFlags = ShowBuildInfoFlags
    { buildInfoInstallCommandFlags = (mempty, mempty, mempty, mempty, mempty, mempty)
    , buildInfoOutputFile = Nothing
    , buildInfoUnitIds = Nothing
    }

-- | The @show-build-info@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
showBuildInfoAction :: ShowBuildInfoFlags -> [String] -> GlobalFlags -> IO ()
showBuildInfoAction (ShowBuildInfoFlags (configFlags, configExFlags, installFlags, haddockFlags, testFlags, clientInstallFlags) fileOutput unitIds)
  targetStrings globalFlags = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig
  let baseCtx' = baseCtx
        { buildSettings = (buildSettings baseCtx) { buildSettingDryRun = True }
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
  showTargets fileOutput unitIds verbosity baseCtx' buildCtx scriptLock
  where
    -- Default to silent verbosity otherwise it will pollute our json output
    verbosity = fromFlagOrDefault silent (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags clientInstallFlags
                  haddockFlags
                  testFlags

-- Pretty nasty piecemeal out of json, but I can't see a way to retrieve output of the setupWrapper'd tasks
showTargets :: Maybe FilePath -> Maybe [String] -> Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> IO ()
showTargets fileOutput unitIds verbosity baseCtx buildCtx lock =
  case fileOutput of
    -- TODO: replace with writeFileAtomic
    Nothing -> do
      putStr "["
      unroll putStr targets
      putStrLn "]"
    Just fp -> do
      writeFile fp "["
      unroll (appendFile fp) targets
      appendFile fp "]"

    where configured = [p | InstallPlan.Configured p <- InstallPlan.toList (elaboratedPlanOriginal buildCtx)]
          targets = maybe (fst <$> (Map.toList . targetsMap $ buildCtx)) (map mkUnitId) unitIds
          doShowInfo unitId = showInfo fileOutput unitIds verbosity baseCtx buildCtx lock configured unitId

          unroll :: (String -> IO ()) -> [UnitId] -> IO ()
          unroll _ [x] = doShowInfo x
          unroll printer (x:xs) = doShowInfo x >> printer "," >> unroll printer xs
          unroll _ [] = return ()

showInfo :: Maybe FilePath -> Maybe [String] -> Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> [ElaboratedConfiguredPackage] -> UnitId -> IO ()
showInfo fileOutput unitIds verbosity baseCtx buildCtx lock pkgs targetUnitId
  | Nothing <- mbPkg = die' verbosity $ "No unit " ++ display targetUnitId
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
                  (const configureFlags)
                  (const configureArgs)
      Right _ -> pure ()

    setupWrapper
      verbosity
      scriptOptions
      (Just $ elabPkgDescription pkg)
      (Cabal.showBuildInfoCommand defaultProgramDb)
      (const (Cabal.ShowBuildInfoFlags
        { Cabal.buildInfoBuildFlags = flags
        , Cabal.buildInfoOutputFile = fileOutput
        , Cabal.buildInfoUnitIds    = unitIds
        }
        )
      )
      (const args)
    where mbPkg = find ((targetUnitId ==) . elabUnitId) pkgs

-- | This defines what a 'TargetSelector' means for the @new-show-build-info@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @new-show-build-info@ command select all components except non-buildable and disabled
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
-- For the @show-build-info@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic subtarget


-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @show-build-info@ command.
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