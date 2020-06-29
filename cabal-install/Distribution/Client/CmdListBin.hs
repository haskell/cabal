{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Distribution.Client.CmdListBin (
    listbinCommand,
    listbinAction,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Control.Monad.IO.Class                    (liftIO)
import Distribution.Client.DistDirLayout         (DistDirLayout (..), ProjectRoot (..))
import Distribution.Client.NixStyleOptions
       (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.ProjectConfig
       (ProjectConfig, projectConfigConfigFile, projectConfigShared, withProjectOrGlobalConfigR)
import Distribution.Client.ProjectFlags          (ProjectFlags (..))
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad          (RebuildEnv (..), runRebuildEx)
import Distribution.Client.Setup                 (GlobalFlags (..))
import Distribution.Simple.BuildPaths            (dllExtension, exeExtension)
import Distribution.Simple.Command               (CommandUI (..))
import Distribution.Simple.Setup                 (configVerbosity, fromFlagOrDefault)
import Distribution.Simple.Utils                 (die', wrapText)
import Distribution.System                       (Platform)
import Distribution.Verbosity                    (silent, verboseStderr)
import System.Directory                          (getCurrentDirectory)
import System.FilePath                           ((<.>), (</>))

import qualified Data.Map                                    as Map
import qualified Distribution.Client.InstallPlan             as IP
import qualified Distribution.Client.SingleCompTargetProblem as SCTP
import qualified Distribution.Simple.InstallDirs             as InstallDirs
import qualified Distribution.Solver.Types.ComponentDeps     as CD

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

listbinCommand :: CommandUI (NixStyleFlags ())
listbinCommand = CommandUI
    { commandName = "list-bin"
    , commandSynopsis = "list path to a single executable."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " list-bin [FLAGS] TARGET\n"
    , commandDescription  = Just $ \_ -> wrapText
        "List path to a build product."
    , commandNotes = Nothing
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions      = nixStyleOptions (const [])
    }

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

listbinAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
listbinAction flags@NixStyleFlags{..} args globalFlags = do
    -- fail early if multiple target selectors specified
    target <- case args of
        []  -> die' verbosity "One target is required, none provided"
        [x] -> return x
        _   -> die' verbosity "One target is required, given multiple"

    -- configure
    let env = RebuildEnv "" True
    (baseCtx, distDirLayout) <- runRebuildEx env $
        withProjectOrGlobalConfigR verbosity ignoreProject globalConfigFlag
        (liftIO withProject) (liftIO .  withoutProject)
    let localPkgs = localPackages baseCtx

    -- elaborate target selectors
    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
        =<< readTargetSelectors localPkgs Nothing [target]

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            targets <- either (SCTP.reportTargetProblems verbosity "list-bin") return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         elaboratedPlan
                         Nothing
                         targetSelectors

            -- Reject multiple targets, or at least targets in different
            -- components. It is ok to have two module/file targets in the
            -- same component, but not two that live in different components.
            --
            -- Note that we discard the target and return the whole 'TargetsMap',
            -- so this check will be repeated (and must succeed) after
            -- the 'runProjectPreBuildPhase'. Keep it in mind when modifying this.
            _ <- SCTP.singleComponentOrElse componentKindsListBin
                   (SCTP.reportTargetProblems
                      verbosity
                      "list-bin"
                      [SCTP.multipleTargetsProblem targets])
                   targets

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            return (elaboratedPlan', targets)

    (selectedUnitId, _selectedComponent) <-
      -- Slight duplication with 'runProjectPreBuildPhase'.
      SCTP.singleComponentOrElse componentKindsListBin
        (die' verbosity $ "No or multiple targets given, but the run "
                       ++ "phase has been reached. This is a bug.")
        $ targetsMap buildCtx

    printPlan verbosity baseCtx buildCtx

    binfiles <- case Map.lookup selectedUnitId $ IP.toMap (elaboratedPlanOriginal buildCtx) of
        Nothing  -> die' verbosity "No or multiple targets given..."
        Just gpp -> return $ IP.foldPlanPackage
            (const []) -- IPI don't have executables
            (elaboratedPackage distDirLayout (elaboratedShared buildCtx))
            gpp

    case binfiles of
        [exe] -> putStrLn exe
        _     -> die' verbosity "No or multiple targets given"
  where
    defaultVerbosity = verboseStderr silent
    verbosity = fromFlagOrDefault defaultVerbosity (configVerbosity configFlags)
    ignoreProject = flagIgnoreProject projectFlags
    prjConfig = commandLineFlagsToProjectConfig globalFlags flags mempty -- ClientInstallFlags, not needed here
    globalConfigFlag = projectConfigConfigFile (projectConfigShared prjConfig)

    withProject :: IO (ProjectBaseContext, DistDirLayout)
    withProject = do
        baseCtx <- establishProjectBaseContext verbosity prjConfig OtherCommand
        return (baseCtx, distDirLayout baseCtx)

    withoutProject :: ProjectConfig -> IO (ProjectBaseContext, DistDirLayout)
    withoutProject config = do
        cwd <- getCurrentDirectory
        baseCtx <- establishProjectBaseContextWithRoot verbosity (config <> prjConfig) (ProjectRootImplicit cwd) OtherCommand
        return (baseCtx, distDirLayout baseCtx)

    -- this is copied from
    elaboratedPackage
        :: DistDirLayout
        -> ElaboratedSharedConfig
        -> ElaboratedConfiguredPackage
        -> [FilePath]
    elaboratedPackage distDirLayout elaboratedSharedConfig elab = case elabPkgOrComp elab of
        ElabPackage pkg ->
            [ bin
            | (c, _) <- CD.toList $ CD.zip (pkgLibDependencies pkg)
                                           (pkgExeDependencies pkg)
            , bin <- bin_file c
            ]
        ElabComponent comp -> bin_file (compSolverName comp)
      where
        dist_dir = distBuildDirectory distDirLayout (elabDistDirParams elaboratedSharedConfig elab)

        bin_file c = case c of
            CD.ComponentExe s   -> [bin_file' s]
            CD.ComponentTest s  -> [bin_file' s]
            CD.ComponentBench s -> [bin_file' s]
            CD.ComponentFLib s  -> [flib_file' s]
            _                -> []

        plat :: Platform
        plat = pkgConfigPlatform elaboratedSharedConfig

        bin_file' s =
            if elabBuildStyle elab == BuildInplaceOnly
            then dist_dir </> "build" </> prettyShow s </> prettyShow s <.> exeExtension plat
            else InstallDirs.bindir (elabInstallDirs elab) </> prettyShow s <.> exeExtension plat

        flib_file' s =
            if elabBuildStyle elab == BuildInplaceOnly
            then dist_dir </> "build" </> prettyShow s </> ("lib" ++ prettyShow s) <.> dllExtension plat
            else InstallDirs.bindir (elabInstallDirs elab) </> ("lib" ++ prettyShow s) <.> dllExtension plat

-- | Component kinds we can list-bin
componentKindsListBin :: [ComponentKind]
componentKindsListBin = [ExeKind, TestKind, BenchKind, FLibKind]

selectPackageTargets :: TargetSelector -> [AvailableTarget k] -> Either SCTP.SingleCompTargetProblem [k]
selectPackageTargets = SCTP.selectPackageTargets componentKindsListBin

selectComponentTarget :: SubComponentTarget -> AvailableTarget k -> Either SCTP.SingleCompTargetProblem  k
selectComponentTarget = SCTP.selectComponentTarget componentKindsListBin
