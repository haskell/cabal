{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: run
--
module Distribution.Client.CmdRun (
    -- * The @run@ CLI and action
    runCommand,
    runAction,

    -- * Internals exposed for testing
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Types.ComponentName
         ( componentNameString )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText, die', ordNub )
----
import Distribution.Package
  ( PackageIdentifier(pkgName)
  , PackageName
  , unPackageName
  , UnitId
  )
import Distribution.Client.ProjectPlanning
  ( ComponentTarget(ComponentTarget)
  , ElaboratedConfiguredPackage(..)
  , ElaboratedInstallPlan
  --, PackageTarget(..)
  , SubComponentTarget(WholeComponent)
  --, binDirectoryFor
  --, pruneInstallPlanToTargets
  )
import Distribution.Client.Targets ( PackageTarget(..) )
import Distribution.Client.InstallPlan
  ( GenericPlanPackage(..)
  , toGraph
  , toList
  )
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedPackageOrComponent(..)
  , ElaboratedComponent(compComponentName)
  , BuildStyle(BuildInplaceOnly, BuildAndInstall)
  , ElaboratedSharedConfig
  , elabDistDirParams
  , compSolverName
  )
import Distribution.Types.Executable
  ( Executable(exeName)
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  )
import Distribution.Types.PackageDescription
  ( PackageDescription(executables, package)
  )
import Distribution.Simple.Program.Run
  ( runProgramInvocation
  , simpleProgramInvocation
  )
import Data.Char (isSpace)
import Distribution.Compat.ReadP
import Distribution.Types.PackageId (pkgName, PackageIdentifier(..))
import Distribution.Client.InstallPlan (foldPlanPackage)
import Data.Maybe (catMaybes, isNothing)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Compat.Graph (Graph)
import Distribution.Simple.Utils (notice,info)
import Distribution.Client.DistDirLayout (DistDirLayout, distBuildDirectory)
import System.FilePath ((</>))
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Types.ComponentName (ComponentName(CExeName))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Solver.Types.ComponentDeps (Component(ComponentExe))

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (when)


runCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
runCommand = Client.installCommand {
  commandName         = "new-run",
  commandSynopsis     = "Run an executable.",
  commandUsage        = usageAlternatives "new-run"
                          [ "[TARGET] [FLAGS] [-- EXECUTABLE_FLAGS]" ],
  commandDescription  = Just $ \pname -> wrapText $
        "Runs the specified executable, first ensuring it is up to date.\n\n"

     ++ "Any executable in any package in the project can be specified. "
     ++ "A package can be specified if contains just one executable. "
     ++ "The default is to use the package in the current directory if it "
     ++ "contains just one executable.\n\n"

     ++ "Extra arguments can be passed to the program, but use '--' to "
     ++ "separate arguments for the program from arguments for " ++ pname
     ++ ". The executable is run in an environment where it can find its "
     ++ "data files inplace in the build tree.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-run\n"
     ++ "    Run the executable in the package in the current directory\n"
     ++ "  " ++ pname ++ " new-run foo-tool\n"
     ++ "    Run the named executable (in any package in the project)\n"
     ++ "  " ++ pname ++ " new-run pkgfoo:foo-tool\n"
     ++ "    Run the executable 'foo-tool' in the package 'pkgfoo'\n"
     ++ "  " ++ pname ++ " new-run foo -O2 -- dothing --fooflag\n"
     ++ "    Build with '-O2' and run the program, passing it extra arguments.\n\n"

     ++ cmdCommonHelpTextNewBuildBeta
   }


-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
runAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
          -> [String] -> GlobalFlags -> IO ()
runAction (configFlags, configExFlags, installFlags, haddockFlags)
            targetStrings globalFlags = do

    baseCtx <- establishProjectBaseContext verbosity cliConfig

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                   =<< readTargetSelectors (localPackages baseCtx)
                         (take 1 targetStrings) -- we drop the exe's args

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            when (buildSettingOnlyDeps (buildSettings baseCtx)) $
              die' verbosity $
                  "The run command does not support '--only-dependencies'. "
               ++ "You may wish to use 'build --only-dependencies' and then "
               ++ "use 'run'."

            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         targetSelectors

            -- Reject multiple targets, or at least targets in different
            -- components. It is ok to have two module/file targets in the
            -- same component, but not two that live in different components.
            when (Set.size (distinctTargetComponents targets) > 1) $
              reportTargetProblems verbosity
                [TargetProblemMultipleTargets targets]

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            return elaboratedPlan'

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

    -- We get the selectors for the package and component.
    -- These are wrapped in Maybes, because the user
    -- might not specify them
    (selectedPackage, selectedComponent) <-
       -- this should always match [x] anyway because
       -- we already check for a single target in TargetSelector.hs
       case selectorPackageAndComponent <$> targetSelectors
         of [x] -> return x
            [ ] -> die'
                     verbosity
                     "No targets given"
            _   -> die'
                     verbosity
                     "Multiple targets given"

    let elaboratedPlan = elaboratedPlanOriginal buildCtx
        matchingElaboratedConfiguredPackages =
          extractMatchingElaboratedConfiguredPackages
            selectedPackage
            selectedComponent
            elaboratedPlan

    -- the names to match. used only for user feedback, as
    -- later on we extract the real ones (whereas these are
    -- wrapped in a Maybe) from the package itself
    let selectedPackageNameToMatch = getPackageName <$> selectedPackage
        selectedComponentNameToMatch = getExeComponentName =<< selectedComponent

    -- For each ElaboratedConfiguredPackage in the install plan, we
    -- identify candidate executables. We only keep them if both the
    -- package name and executable name match what the user asked for
    -- (a missing specification matches everything).
    --
    -- In the common case, we expect this to pick out a single
    -- ElaboratedConfiguredPackage that provides a single way of building
    -- an appropriately-named executable. In that case we prune our
    -- install plan to that UnitId and PackageTarget and continue.
    --
    -- However, multiple packages/components could provide that
    -- executable, or it's possible we don't find the executable anywhere
    -- in the build plan. I suppose in principle it's also possible that
    -- a single package provides an executable in two different ways,
    -- though that's probably a bug if. Anyway it's a good lint to report
    -- an error in all of these cases, even if some seem like they
    -- shouldn't happen.
    (pkg,exe) <- case matchingElaboratedConfiguredPackages of
      [] -> die' verbosity $ "Unknown executable"
                          ++ case selectedComponentNameToMatch
                             of Just x -> " " ++ x
                                Nothing -> ""
                          ++ case selectedPackageNameToMatch
                             of Just x -> " in package " ++ x
                                Nothing -> ""
      [(elabPkg,exe)] -> do
        info verbosity $ "Selecting " ++ display (elabUnitId elabPkg)
                      ++ case selectedComponentNameToMatch
                         of Just x -> " to supply " ++ x
                            Nothing -> ""
        return (elabPkg, unUnqualComponentName exe)
      elabPkgs -> die' verbosity
        $ "Multiple matching executables found"
        ++ case selectedComponentNameToMatch
           of Just x -> " matching " ++ x
              Nothing -> ""
        ++ ":\n"
        ++ unlines (fmap (\(p,_) -> " - in package " ++ display (elabUnitId p)) elabPkgs)
    let exePath = binDirectoryFor (distDirLayout baseCtx)
                                  (elaboratedShared buildCtx)
                                  pkg
                                  exe
               </> exe
    print exePath
    let args = drop 1 targetStrings
    runProgramInvocation
      verbosity
      (simpleProgramInvocation exePath args)
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

-- Package selection
------

getPackageName :: PackageIdentifier -> String
getPackageName (PackageIdentifier packageName _) =
  unPackageName packageName

getExeComponentName :: ComponentName -> Maybe String
getExeComponentName (CExeName unqualComponentName) =
  Just $ unUnqualComponentName unqualComponentName
getExeComponentName _ = Nothing

selectorPackageAndComponent :: TargetSelector PackageId
                          -> (Maybe PackageId, Maybe ComponentName)
selectorPackageAndComponent (TargetPackage _ pkg _) =
  (Just pkg, Nothing)
selectorPackageAndComponent (TargetAllPackages _) =
  (Nothing, Nothing)
selectorPackageAndComponent (TargetComponent pkg component _) =
  (Just pkg, Just component)

-- | Extract all 'ElaboratedConfiguredPackage's and executable names
--  that match the user-provided component/package
-- The component can be either:
--  * specified by the user (both Just)
--  * deduced from an user-specified package (the component is unspecified, Nothing)
--  * deduced from the cwd (both the package and the component are unspecified)
extractMatchingElaboratedConfiguredPackages
  :: Maybe PackageId -- ^ the package to match
  -> Maybe ComponentName -- ^ the component to match
  -> ElaboratedInstallPlan -- ^ a plan in with to search for matching exes
  -> [(ElaboratedConfiguredPackage, UnqualComponentName)] -- ^ the matching package and the exe name
extractMatchingElaboratedConfiguredPackages
  pkgId component = catMaybes
                  . fmap sequenceA -- get the Maybe outside the tuple
                  . fmap (\p -> (p, executableOfPackage p))
                  . catMaybes
                  . fmap (foldPlanPackage
                           (const Nothing)
                           (\x -> if match x
                                  then Just x
                                  else Nothing))
                  . toList
  where
    justIfCondition f x = if f x then Just x else Nothing
    match :: ElaboratedConfiguredPackage -> Bool
    match p = matchPackage pkgId p && matchComponent component p

matchPackage :: Maybe PackageId
             -> ElaboratedConfiguredPackage
             -> Bool
matchPackage pkgId pkg =
  pkgId == Just (elabPkgSourceId pkg)
  || isNothing pkgId --if the package is unspecified (Nothing), all packages match

matchComponent :: Maybe ComponentName
               -> ElaboratedConfiguredPackage
               -> Bool
matchComponent component pkg =
  componentString == executableOfPackage pkg
  || isNothing componentString --if the component is unspecified (Nothing), all components match
  where componentString = componentNameString =<< component

executableOfPackage :: ElaboratedConfiguredPackage
                    -> Maybe UnqualComponentName
executableOfPackage p =
  case elabPkgOrComp p
  of ElabComponent comp -> case compComponentName comp
                           of Just (CExeName exe) -> Just exe
                              _                   -> Nothing
     _ -> Nothing
{-executableOfPackage p =
  case elabPkgOrComp p
  of ElabComponent comp -> case compSolverName comp
                           of ComponentExe exe -> Just exe
                              _                -> Nothing
     _ -> Nothing-} --MAYBE this one instead of the other one?

-- Path construction
------

-- | The path to the @build@ directory for an inplace build.
inplaceBinRoot
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> FilePath
inplaceBinRoot layout config package
  =  distBuildDirectory layout (elabDistDirParams config package)
 </> "build"

-- | The path to the directory that contains a specific executable.
binDirectoryFor
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> FilePath
  -> FilePath
binDirectoryFor layout config package exe = case elabBuildStyle package of
  BuildAndInstall -> installedBinDirectory package
  BuildInplaceOnly -> inplaceBinRoot layout config package </> exe

-- package has been built and installed.
installedBinDirectory :: ElaboratedConfiguredPackage -> FilePath
installedBinDirectory = InstallDirs.bindir . elabInstallDirs


-- | This defines what a 'TargetSelector' means for the @run@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @run@ command we select the exe if there is only one and it's
-- buildable. Fail if there are no or multiple buildable exe components.
--
selectPackageTargets :: TargetSelector PackageId
                     -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsExesBuildable')

    -- If there are executables but none are buildable then we report those
  | not (null targetsExes)
  = Left (TargetProblemNoneEnabled targetSelector targetsExes)

    -- If there are no executables but some other targets then we report that
  | not (null targets)
  = Left (TargetProblemNoExes targetSelector)

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    (targetsExesBuildable,
     targetsExesBuildable') = selectBuildableTargets'
                            . filterTargetsKind ExeKind
                            $ targets

    targetsExes             = forgetTargetsDetail
                            . filterTargetsKind ExeKind
                            $ targets


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @run@ command we just need to check it is a executable, in addition
-- to the basic checks on being buildable etc.
--
selectComponentTarget :: PackageId -> ComponentName -> SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem  k
selectComponentTarget pkgid cname subtarget@WholeComponent t
  | CExeName _ <- availableTargetComponentName t
  = either (Left . TargetProblemCommon) return $
           selectComponentTargetBasic pkgid cname subtarget t
  | otherwise
  = Left (TargetProblemComponentNotExe pkgid cname)

selectComponentTarget pkgid cname subtarget _
  = Left (TargetProblemIsSubComponent pkgid cname subtarget)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @run@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon
     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled (TargetSelector PackageId) [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   (TargetSelector PackageId)

     -- | The 'TargetSelector' matches targets but no executables
   | TargetProblemNoExes      (TargetSelector PackageId)

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple (TargetSelector PackageId) [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap

     -- | The 'TargetSelector' refers to a component that is not an executable
   | TargetProblemComponentNotExe PackageId ComponentName

     -- | Asking to run an individual file or module is not supported
   | TargetProblemIsSubComponent  PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "run" problem

renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "run" targetSelector targets

renderTargetProblem (TargetProblemNoExes targetSelector) =
    "Cannot run the target '" ++ showTargetSelector targetSelector
 ++ "' which refers to " ++ renderTargetSelector targetSelector
 ++ " because "
 ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
 ++ " not contain any executables."

renderTargetProblem (TargetProblemNoTargets targetSelector) =
    case targetSelectorFilter targetSelector of
      Just kind | kind /= ExeKind
        -> "The run command is for running executables, but the target '"
           ++ showTargetSelector targetSelector ++ "' refers to "
           ++ renderTargetSelector targetSelector ++ "."

      _ -> renderTargetProblemNoTargets "run" targetSelector
  where
    targetSelectorFilter (TargetPackage  _ _ mkfilter) = mkfilter
    targetSelectorFilter (TargetAllPackages  mkfilter) = mkfilter
    targetSelectorFilter (TargetComponent _ _ _)       = Nothing


renderTargetProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "The run command is for running a single executable at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which includes the executables "
 ++ renderListCommaAnd
      [ display name
      | cname@CExeName{} <- map availableTargetComponentName targets
      , let Just name = componentNameString cname
      ]
 ++ "."

renderTargetProblem (TargetProblemMultipleTargets selectorMap) =
    "The run command is for running a single executable at once. The targets "
 ++ renderListCommaAnd [ "'" ++ showTargetSelector ts ++ "'"
                       | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different executables."

renderTargetProblem (TargetProblemComponentNotExe pkgid cname) =
    "The run command is for running executables, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " from the package "
 ++ display pkgid ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent

renderTargetProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
    "The run command can only run an executable as a whole, "
 ++ "not files or modules within them, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget

