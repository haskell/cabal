{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | cabal-install CLI command: run
--
module Distribution.Client.CmdRun (
    -- * The @run@ CLI and action
    runCommand,
    runAction,
    handleShebang, validScript,

    -- * Internals exposed for testing
    matchesMultipleProblem,
    noExesProblem,
    selectPackageTargets,
    selectComponentTarget
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (toList)

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
         ( renderTargetSelector, showTargetSelector,
           renderTargetProblem,
           renderTargetProblemNoTargets, plural, targetSelectorPluralPkgs,
           targetSelectorFilter, renderListCommaAnd )
import Distribution.Client.TargetProblem
         ( TargetProblem (..) )

import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.Setup
         ( GlobalFlags(..), ConfigFlags(..) )
import Distribution.Client.GlobalFlags
         ( defaultGlobalFlags )
import Distribution.Simple.Flag
         ( fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Types.ComponentName
         ( showComponentName )
import Distribution.Verbosity
         ( normal )
import Distribution.Simple.Utils
         ( wrapText, die', info, notice )
import Distribution.Client.ProjectPlanning
         ( ElaboratedConfiguredPackage(..)
         , ElaboratedInstallPlan, binDirectoryFor )
import Distribution.Client.ProjectPlanning.Types
         ( dataDirsEnvironmentForPlan )
import Distribution.Client.InstallPlan
         ( toList, foldPlanPackage )
import Distribution.Types.UnqualComponentName
         ( UnqualComponentName, unUnqualComponentName )
import Distribution.Simple.Program.Run
         ( runProgramInvocation, ProgramInvocation(..),
           emptyProgramInvocation )
import Distribution.Types.UnitId
         ( UnitId )
import Distribution.Client.ScriptUtils
         ( AcceptNoTargets(..), withContextAndSelectors, updateAndPersistScriptContext, TargetContext(..) )

import qualified Data.Set as Set
import System.Directory
         ( doesFileExist )
import System.FilePath
         ( (</>), isValid, isPathSeparator )

runCommand :: CommandUI (NixStyleFlags ())
runCommand = CommandUI
  { commandName         = "v2-run"
  , commandSynopsis     = "Run an executable."
  , commandUsage        = usageAlternatives "v2-run"
                          [ "[TARGET] [FLAGS] [-- EXECUTABLE_FLAGS]" ]
  , commandDescription  = Just $ \pname -> wrapText $
         "Runs the specified executable-like component (an executable, a test, "
      ++ "or a benchmark), first ensuring it is up to date.\n\n"

      ++ "Any executable-like component in any package in the project can be "
      ++ "specified. A package can be specified if contains just one "
      ++ "executable-like, preferring a single executable. The default is to "
      ++ "use the package in the current directory if it contains just one "
      ++ "executable-like.\n\n"

      ++ "Extra arguments can be passed to the program, but use '--' to "
      ++ "separate arguments for the program from arguments for " ++ pname
      ++ ". The executable is run in an environment where it can find its "
      ++ "data files inplace in the build tree.\n\n"

      ++ "Dependencies are built or rebuilt as necessary. Additional "
      ++ "configuration flags can be specified on the command line and these "
      ++ "extend the project configuration from the 'cabal.project', "
      ++ "'cabal.project.local' and other files."
  , commandNotes        = Just $ \pname ->
         "Examples:\n"
      ++ "  " ++ pname ++ " v2-run\n"
      ++ "    Run the executable-like in the package in the current directory\n"
      ++ "  " ++ pname ++ " v2-run foo-tool\n"
      ++ "    Run the named executable-like (in any package in the project)\n"
      ++ "  " ++ pname ++ " v2-run pkgfoo:foo-tool\n"
      ++ "    Run the executable-like 'foo-tool' in the package 'pkgfoo'\n"
      ++ "  " ++ pname ++ " v2-run foo -O2 -- dothing --fooflag\n"
      ++ "    Build with '-O2' and run the program, passing it extra arguments.\n"

  , commandDefaultFlags = defaultNixStyleFlags ()
  , commandOptions      = nixStyleOptions (const [])
  }

-- | The @run@ command runs a specified executable-like component, building it
-- first if necessary. The component can be either an executable, a test,
-- or a benchmark. This is particularly useful for passing arguments to
-- exes/tests/benchs by simply appending them after a @--@.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
runAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
runAction flags@NixStyleFlags {..} targetStrings globalFlags
  = withContextAndSelectors RejectNoTargets "" (Just ExeKind) flags targetStrings globalFlags $ \targetCtx ctx targetSelectors -> do
    baseCtx <- case targetCtx of
      ProjectContext                      -> return ctx
      GlobalContext                       -> return ctx
      ScriptContext path exemeta contents -> updateAndPersistScriptContext ctx path exemeta contents

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
            _ <- singleExeOrElse
                   (reportTargetProblems
                      verbosity
                      [multipleTargetsProblem targets])
                   targets

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            return (elaboratedPlan', targets)

    (selectedUnitId, selectedComponent) <-
      -- Slight duplication with 'runProjectPreBuildPhase'.
      singleExeOrElse
        (die' verbosity $ "No or multiple targets given, but the run "
                       ++ "phase has been reached. This is a bug.")
        $ targetsMap buildCtx

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes


    let elaboratedPlan = elaboratedPlanToExecute buildCtx
        matchingElaboratedConfiguredPackages =
          matchingPackagesByUnitId
            selectedUnitId
            elaboratedPlan

    let exeName = unUnqualComponentName selectedComponent

    -- In the common case, we expect @matchingElaboratedConfiguredPackages@
    -- to consist of a single element that provides a single way of building
    -- an appropriately-named executable. In that case we take that
    -- package and continue.
    --
    -- However, multiple packages/components could provide that
    -- executable, or it's possible we don't find the executable anywhere
    -- in the build plan. I suppose in principle it's also possible that
    -- a single package provides an executable in two different ways,
    -- though that's probably a bug if. Anyway it's a good lint to report
    -- an error in all of these cases, even if some seem like they
    -- shouldn't happen.
    pkg <- case matchingElaboratedConfiguredPackages of
      [] -> die' verbosity $ "Unknown executable "
                          ++ exeName
                          ++ " in package "
                          ++ prettyShow selectedUnitId
      [elabPkg] -> do
        info verbosity $ "Selecting "
                       ++ prettyShow selectedUnitId
                       ++ " to supply " ++ exeName
        return elabPkg
      elabPkgs -> die' verbosity
        $ "Multiple matching executables found matching "
        ++ exeName
        ++ ":\n"
        ++ unlines (fmap (\p -> " - in package " ++ prettyShow (elabUnitId p)) elabPkgs)
    let exePath = binDirectoryFor (distDirLayout baseCtx)
                                  (elaboratedShared buildCtx)
                                  pkg
                                  exeName
               </> exeName
    let args = drop 1 targetStrings
        dryRun = buildSettingDryRun (buildSettings baseCtx)
              || buildSettingOnlyDownload (buildSettings baseCtx)

    if dryRun
       then notice verbosity "Running of executable suppressed by flag(s)"
       else
         runProgramInvocation
           verbosity
           emptyProgramInvocation {
             progInvokePath  = exePath,
             progInvokeArgs  = args,
             progInvokeEnv   = dataDirsEnvironmentForPlan
                                 (distDirLayout baseCtx)
                                 elaboratedPlan
           }
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- | Used by the main CLI parser as heuristic to decide whether @cabal@ was
-- invoked as a script interpreter, i.e. via
--
-- > #! /usr/bin/env cabal
--
-- or
--
-- > #! /usr/bin/cabal
--
-- As the first argument passed to `cabal` will be a filepath to the
-- script to be interpreted.
--
-- See also 'handleShebang'
validScript :: String -> IO Bool
validScript script
  | isValid script && any isPathSeparator script = doesFileExist script
  | otherwise = return False

-- | Handle @cabal@ invoked as script interpreter, see also 'validScript'
--
-- First argument is the 'FilePath' to the script to be executed; second
-- argument is a list of arguments to be passed to the script.
handleShebang :: FilePath -> [String] -> IO ()
handleShebang script args =
  runAction (commandDefaultFlags runCommand) (script:args) defaultGlobalFlags

singleExeOrElse :: IO (UnitId, UnqualComponentName) -> TargetsMap -> IO (UnitId, UnqualComponentName)
singleExeOrElse action targetsMap =
  case Set.toList . distinctTargetComponents $ targetsMap
  of [(unitId, CExeName component)] -> return (unitId, component)
     [(unitId, CTestName component)] -> return (unitId, component)
     [(unitId, CBenchName component)] -> return (unitId, component)
     _   -> action

-- | Filter the 'ElaboratedInstallPlan' keeping only the
-- 'ElaboratedConfiguredPackage's that match the specified
-- 'UnitId'.
matchingPackagesByUnitId :: UnitId
                         -> ElaboratedInstallPlan
                         -> [ElaboratedConfiguredPackage]
matchingPackagesByUnitId uid =
          catMaybes
          . fmap (foldPlanPackage
                    (const Nothing)
                    (\x -> if elabUnitId x == uid
                           then Just x
                           else Nothing))
          . toList

-- | This defines what a 'TargetSelector' means for the @run@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @run@ command we select the exe if there is only one and it's
-- buildable. Fail if there are no or multiple buildable exe components.
--
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either RunTargetProblem [k]
selectPackageTargets targetSelector targets

  -- If there is a single executable component, select that. See #7403
  | [target] <- targetsExesBuildable
  = Right [target]

  -- Otherwise, if there is a single executable-like component left, select that.
  | [target] <- targetsExeLikesBuildable
  = Right [target]

  -- but fail if there are multiple buildable executables.
  | not (null targetsExeLikesBuildable)
  = Left (matchesMultipleProblem targetSelector targetsExeLikesBuildable')

    -- If there are executables but none are buildable then we report those
  | not (null targetsExeLikes')
  = Left (TargetProblemNoneEnabled targetSelector targetsExeLikes')

    -- If there are no executables but some other targets then we report that
  | not (null targets)
  = Left (noExesProblem targetSelector)

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    -- Targets that are precisely executables
    targetsExes = filterTargetsKind ExeKind targets
    targetsExesBuildable = selectBuildableTargets targetsExes

    -- Any target that could be executed
    targetsExeLikes = targetsExes
                   ++ filterTargetsKind TestKind targets
                   ++ filterTargetsKind BenchKind targets

    (targetsExeLikesBuildable,
     targetsExeLikesBuildable') = selectBuildableTargets' targetsExeLikes

    targetsExeLikes'             = forgetTargetsDetail targetsExeLikes


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @run@ command we just need to check it is a executable-like
-- (an executable, a test, or a benchmark), in addition
-- to the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either RunTargetProblem  k
selectComponentTarget subtarget@WholeComponent t
  = case availableTargetComponentName t
    of CExeName _ -> component
       CTestName _ -> component
       CBenchName _ -> component
       _ -> Left (componentNotExeProblem pkgid cname)
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
data RunProblem =
     -- | The 'TargetSelector' matches targets but no executables
     TargetProblemNoExes      TargetSelector

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap

     -- | The 'TargetSelector' refers to a component that is not an executable
   | TargetProblemComponentNotExe PackageId ComponentName

     -- | Asking to run an individual file or module is not supported
   | TargetProblemIsSubComponent  PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

type RunTargetProblem = TargetProblem RunProblem

noExesProblem :: TargetSelector -> RunTargetProblem
noExesProblem = CustomTargetProblem . TargetProblemNoExes

matchesMultipleProblem :: TargetSelector -> [AvailableTarget ()] -> RunTargetProblem
matchesMultipleProblem selector targets = CustomTargetProblem $
    TargetProblemMatchesMultiple selector targets

multipleTargetsProblem :: TargetsMap -> TargetProblem RunProblem
multipleTargetsProblem = CustomTargetProblem . TargetProblemMultipleTargets

componentNotExeProblem :: PackageId -> ComponentName -> TargetProblem RunProblem
componentNotExeProblem pkgid name = CustomTargetProblem $
    TargetProblemComponentNotExe pkgid name

isSubComponentProblem
  :: PackageId
  -> ComponentName
  -> SubComponentTarget
  -> TargetProblem RunProblem
isSubComponentProblem pkgid name subcomponent = CustomTargetProblem $
    TargetProblemIsSubComponent pkgid name subcomponent

reportTargetProblems :: Verbosity -> [RunTargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderRunTargetProblem

renderRunTargetProblem :: RunTargetProblem -> String
renderRunTargetProblem (TargetProblemNoTargets targetSelector) =
    case targetSelectorFilter targetSelector of
      Just kind | kind /= ExeKind
        -> "The run command is for running executables, but the target '"
           ++ showTargetSelector targetSelector ++ "' refers to "
           ++ renderTargetSelector targetSelector ++ "."

      _ -> renderTargetProblemNoTargets "run" targetSelector
renderRunTargetProblem problem =
    renderTargetProblem "run" renderRunProblem problem

renderRunProblem :: RunProblem -> String
renderRunProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "The run command is for running a single executable at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which includes "
 ++ renderListCommaAnd ( ("the "++) <$>
                         showComponentName <$>
                         availableTargetComponentName <$>
                         foldMap
                           (\kind -> filterTargetsKind kind targets)
                           [ExeKind, TestKind, BenchKind] )
 ++ "."

renderRunProblem (TargetProblemMultipleTargets selectorMap) =
    "The run command is for running a single executable at once. The targets "
 ++ renderListCommaAnd [ "'" ++ showTargetSelector ts ++ "'"
                       | ts <- uniqueTargetSelectors selectorMap ]
 ++ " refer to different executables."

renderRunProblem (TargetProblemComponentNotExe pkgid cname) =
    "The run command is for running executables, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " from the package "
 ++ prettyShow pkgid ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent

renderRunProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
    "The run command can only run an executable as a whole, "
 ++ "not files or modules within them, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget

renderRunProblem (TargetProblemNoExes targetSelector) =
    "Cannot run the target '" ++ showTargetSelector targetSelector
 ++ "' which refers to " ++ renderTargetSelector targetSelector
 ++ " because "
 ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
 ++ " not contain any executables."
