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

import Distribution.Client.CmdErrorMessages
       (plural, renderListCommaAnd, renderTargetProblem, renderTargetProblemNoTargets,
       renderTargetSelector, showTargetSelector, targetSelectorFilter, targetSelectorPluralPkgs)
import Distribution.Client.DistDirLayout         (DistDirLayout (..), ProjectRoot (..))
import Distribution.Client.NixStyleOptions
       (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.ProjectConfig
       (ProjectConfig, projectConfigConfigFile, projectConfigShared, withProjectOrGlobalConfig)
import Distribution.Client.ProjectFlags          (ProjectFlags (..))
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Setup                 (GlobalFlags (..))
import Distribution.Client.TargetProblem         (TargetProblem (..))
import Distribution.Simple.BuildPaths            (dllExtension, exeExtension)
import Distribution.Simple.Command               (CommandUI (..))
import Distribution.Simple.Setup                 (configVerbosity, fromFlagOrDefault)
import Distribution.Simple.Utils                 (die', ordNub, wrapText)
import Distribution.System                       (Platform)
import Distribution.Types.ComponentName          (showComponentName)
import Distribution.Types.UnitId                 (UnitId)
import Distribution.Types.UnqualComponentName    (UnqualComponentName)
import Distribution.Verbosity                    (silent, verboseStderr)
import System.Directory                          (getCurrentDirectory)
import System.FilePath                           ((<.>), (</>))

import qualified Data.Map                                as Map
import qualified Data.Set                                as Set
import qualified Distribution.Client.InstallPlan         as IP
import qualified Distribution.Simple.InstallDirs         as InstallDirs
import qualified Distribution.Solver.Types.ComponentDeps as CD

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
    (baseCtx, distDirLayout) <- withProjectOrGlobalConfig verbosity ignoreProject globalConfigFlag withProject withoutProject
    let localPkgs = localPackages baseCtx

    -- elaborate target selectors
    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
        =<< readTargetSelectors localPkgs Nothing [target]

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            targets <- either (reportTargetProblems verbosity) return
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
            _ <- singleComponentOrElse
                   (reportTargetProblems
                      verbosity
                      [multipleTargetsProblem targets])
                   targets

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            return (elaboratedPlan', targets)

    (selectedUnitId, _selectedComponent) <-
      -- Slight duplication with 'runProjectPreBuildPhase'.
      singleComponentOrElse
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

        -- here and in PlanOutput,
        -- use binDirectoryFor?
        bin_file' s =
            if elabBuildStyle elab == BuildInplaceOnly
            then dist_dir </> "build" </> prettyShow s </> prettyShow s <.> exeExtension plat
            else InstallDirs.bindir (elabInstallDirs elab) </> prettyShow s <.> exeExtension plat

        flib_file' s =
            if elabBuildStyle elab == BuildInplaceOnly
            then dist_dir </> "build" </> prettyShow s </> ("lib" ++ prettyShow s) <.> dllExtension plat
            else InstallDirs.bindir (elabInstallDirs elab) </> ("lib" ++ prettyShow s) <.> dllExtension plat

-------------------------------------------------------------------------------
-- Target Problem: the very similar to CmdRun
-------------------------------------------------------------------------------

singleComponentOrElse :: IO (UnitId, UnqualComponentName) -> TargetsMap -> IO (UnitId, UnqualComponentName)
singleComponentOrElse action targetsMap =
  case Set.toList . distinctTargetComponents $ targetsMap
  of [(unitId, CExeName component)] -> return (unitId, component)
     [(unitId, CTestName component)] -> return (unitId, component)
     [(unitId, CBenchName component)] -> return (unitId, component)
     [(unitId, CFLibName component)] -> return (unitId, component)
     _   -> action

-- | This defines what a 'TargetSelector' means for the @list-bin@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @list-bin@ command we select the exe or flib if there is only one
-- and it's buildable. Fail if there are no or multiple buildable exe components.
--
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either ListBinTargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable)
  = Left (matchesMultipleProblem targetSelector targetsExesBuildable')

    -- If there are executables but none are buildable then we report those
  | not (null targetsExes)
  = Left (TargetProblemNoneEnabled targetSelector targetsExes)

    -- If there are no executables but some other targets then we report that
  | not (null targets)
  = Left (noComponentsProblem targetSelector)

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    -- Targets that can be executed
    targetsExecutableLike =
      concatMap (\kind -> filterTargetsKind kind targets)
                [ExeKind, TestKind, BenchKind]
    (targetsExesBuildable,
     targetsExesBuildable') = selectBuildableTargets' targetsExecutableLike

    targetsExes             = forgetTargetsDetail targetsExecutableLike


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @run@ command we just need to check it is a executable-like
-- (an executable, a test, or a benchmark), in addition
-- to the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either ListBinTargetProblem  k
selectComponentTarget subtarget@WholeComponent t
  = case availableTargetComponentName t
    of CExeName _ -> component
       CTestName _ -> component
       CBenchName _ -> component
       CFLibName _ -> component
       _ -> Left (componentNotRightKindProblem pkgid cname)
    where pkgid = availableTargetPackageId t
          cname = availableTargetComponentName t
          component = selectComponentTargetBasic subtarget t

selectComponentTarget subtarget t
  = Left (isSubComponentProblem (availableTargetPackageId t)
           (availableTargetComponentName t)
           subtarget)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @run@ command.
--
data ListBinProblem =
     -- | The 'TargetSelector' matches targets but no executables
     TargetProblemNoRightComps      TargetSelector

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap

     -- | The 'TargetSelector' refers to a component that is not an executable
   | TargetProblemComponentNotRightKind PackageId ComponentName

     -- | Asking to run an individual file or module is not supported
   | TargetProblemIsSubComponent  PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

type ListBinTargetProblem = TargetProblem ListBinProblem

noComponentsProblem :: TargetSelector -> ListBinTargetProblem
noComponentsProblem = CustomTargetProblem . TargetProblemNoRightComps

matchesMultipleProblem :: TargetSelector -> [AvailableTarget ()] -> ListBinTargetProblem
matchesMultipleProblem selector targets = CustomTargetProblem $
    TargetProblemMatchesMultiple selector targets

multipleTargetsProblem :: TargetsMap -> TargetProblem ListBinProblem
multipleTargetsProblem = CustomTargetProblem . TargetProblemMultipleTargets

componentNotRightKindProblem :: PackageId -> ComponentName -> TargetProblem ListBinProblem
componentNotRightKindProblem pkgid name = CustomTargetProblem $
    TargetProblemComponentNotRightKind pkgid name

isSubComponentProblem
  :: PackageId
  -> ComponentName
  -> SubComponentTarget
  -> TargetProblem ListBinProblem
isSubComponentProblem pkgid name subcomponent = CustomTargetProblem $
    TargetProblemIsSubComponent pkgid name subcomponent

reportTargetProblems :: Verbosity -> [ListBinTargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderListBinTargetProblem

renderListBinTargetProblem :: ListBinTargetProblem -> String
renderListBinTargetProblem (TargetProblemNoTargets targetSelector) =
    case targetSelectorFilter targetSelector of
      Just kind | kind /= ExeKind
        -> "The list-bin command is for finding binaries, but the target '"
           ++ showTargetSelector targetSelector ++ "' refers to "
           ++ renderTargetSelector targetSelector ++ "."

      _ -> renderTargetProblemNoTargets "list-bin" targetSelector
renderListBinTargetProblem problem =
    renderTargetProblem "list-bin" renderListBinProblem problem

renderListBinProblem :: ListBinProblem -> String
renderListBinProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "The list-bin command is for finding a single binary at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which includes "
 ++ renderListCommaAnd ( ("the "++) <$>
                         showComponentName <$>
                         availableTargetComponentName <$>
                         foldMap
                           (\kind -> filterTargetsKind kind targets)
                           [ExeKind, TestKind, BenchKind] )
 ++ "."

renderListBinProblem (TargetProblemMultipleTargets selectorMap) =
    "The list-bin command is for finding a single binary at once. The targets "
 ++ renderListCommaAnd [ "'" ++ showTargetSelector ts ++ "'"
                       | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different executables."

renderListBinProblem (TargetProblemComponentNotRightKind pkgid cname) =
    "The list-bin command is for finding binaries, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " from the package "
 ++ prettyShow pkgid ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent

renderListBinProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
    "The list-bin command can only find a binary as a whole, "
 ++ "not files or modules within them, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget

renderListBinProblem (TargetProblemNoRightComps targetSelector) =
    "Cannot list-bin the target '" ++ showTargetSelector targetSelector
 ++ "' which refers to " ++ renderTargetSelector targetSelector
 ++ " because "
 ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
 ++ " not contain any executables or foreign libraries."
