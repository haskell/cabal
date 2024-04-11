{-# LANGUAGE RecordWildCards #-}

-- | cabal-install CLI command: bench
module Distribution.Client.CmdBench
  ( -- * The @bench@ CLI and action
    benchCommand
  , benchAction

    -- * Internals exposed for testing
  , componentNotBenchmarkProblem
  , isSubComponentProblem
  , noBenchmarksProblem
  , selectPackageTargets
  , selectComponentTarget
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdErrorMessages
  ( plural
  , renderTargetProblem
  , renderTargetProblemNoTargets
  , renderTargetSelector
  , showTargetSelector
  , targetSelectorFilter
  , targetSelectorPluralPkgs
  )
import Distribution.Client.Errors
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags
  )
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  )
import Distribution.Client.Utils
  ( giveRTSWarning
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )
import Distribution.Simple.Flag
  ( fromFlagOrDefault
  )
import Distribution.Simple.Setup (CommonSetupFlags (..))
import Distribution.Simple.Utils
  ( dieWithException
  , warn
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )

import GHC.Environment
  ( getFullArgs
  )

benchCommand :: CommandUI (NixStyleFlags ())
benchCommand =
  CommandUI
    { commandName = "v2-bench"
    , commandSynopsis = "Run benchmarks."
    , commandUsage = usageAlternatives "v2-bench" ["[TARGETS] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Runs the specified benchmarks, first ensuring they are up to "
            ++ "date.\n\n"
            ++ "Any benchmark in any package in the project can be specified. "
            ++ "A package can be specified in which case all the benchmarks in the "
            ++ "package are run. The default is to run all the benchmarks in the "
            ++ "package in the current directory.\n\n"
            ++ "Dependencies are built or rebuilt as necessary. Additional "
            ++ "configuration flags can be specified on the command line and these "
            ++ "extend the project configuration from the 'cabal.project', "
            ++ "'cabal.project.local' and other files."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-bench\n"
          ++ "    Run all the benchmarks in the package in the current directory\n"
          ++ "  "
          ++ pname
          ++ " v2-bench pkgname\n"
          ++ "    Run all the benchmarks in the package named pkgname\n"
          ++ "  "
          ++ pname
          ++ " v2-bench cname\n"
          ++ "    Run the benchmark named cname\n"
          ++ "  "
          ++ pname
          ++ " v2-bench cname -O2\n"
          ++ "    Run the benchmark built with '-O2' (including local libs used)\n"
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = nixStyleOptions (const [])
    }

-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
benchAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
benchAction flags@NixStyleFlags{..} targetStrings globalFlags = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand

  targetSelectors <-
    either (reportTargetSelectorProblems verbosity) return
      =<< readTargetSelectors (localPackages baseCtx) (Just BenchKind) targetStrings

  buildCtx <-
    runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
      when (buildSettingOnlyDeps (buildSettings baseCtx)) $
        dieWithException verbosity BenchActionException

      fullArgs <- getFullArgs
      when ("+RTS" `elem` fullArgs) $
        warn verbosity $
          giveRTSWarning "bench"

      -- Interpret the targets on the command line as bench targets
      -- (as opposed to say build or haddock targets).
      targets <-
        either (reportTargetProblems verbosity) return $
          resolveTargets
            selectPackageTargets
            selectComponentTarget
            elaboratedPlan
            Nothing
            targetSelectors

      let elaboratedPlan' =
            pruneInstallPlanToTargets
              TargetActionBench
              targets
              elaboratedPlan
      return (elaboratedPlan', targets)

  printPlan verbosity baseCtx buildCtx

  buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
  runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    verbosity = fromFlagOrDefault normal (setupVerbosity $ configCommonFlags configFlags)
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @bench@ command we select all buildable benchmarks,
-- or fail if there are no benchmarks or no buildable benchmarks.
selectPackageTargets
  :: TargetSelector
  -> [AvailableTarget k]
  -> Either BenchTargetProblem [k]
selectPackageTargets targetSelector targets
  -- If there are any buildable benchmark targets then we select those
  | not (null targetsBenchBuildable) =
      Right targetsBenchBuildable
  -- If there are benchmarks but none are buildable then we report those
  | not (null targetsBench) =
      Left (TargetProblemNoneEnabled targetSelector targetsBench)
  -- If there are no benchmarks but some other targets then we report that
  | not (null targets) =
      Left (noBenchmarksProblem targetSelector)
  -- If there are no targets at all then we report that
  | otherwise =
      Left (TargetProblemNoTargets targetSelector)
  where
    targetsBenchBuildable =
      selectBuildableTargets
        . filterTargetsKind BenchKind
        $ targets

    targetsBench =
      forgetTargetsDetail
        . filterTargetsKind BenchKind
        $ targets

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @bench@ command we just need to check it is a benchmark, in addition
-- to the basic checks on being buildable etc.
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either BenchTargetProblem k
selectComponentTarget subtarget@WholeComponent t
  | CBenchName _ <- availableTargetComponentName t =
      selectComponentTargetBasic subtarget t
  | otherwise =
      Left
        ( componentNotBenchmarkProblem
            (availableTargetPackageId t)
            (availableTargetComponentName t)
        )
selectComponentTarget subtarget t =
  Left
    ( isSubComponentProblem
        (availableTargetPackageId t)
        (availableTargetComponentName t)
        subtarget
    )

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @bench@ command.
data BenchProblem
  = -- | The 'TargetSelector' matches targets but no benchmarks
    TargetProblemNoBenchmarks TargetSelector
  | -- | The 'TargetSelector' refers to a component that is not a benchmark
    TargetProblemComponentNotBenchmark PackageId ComponentName
  | -- | Asking to benchmark an individual file or module is not supported
    TargetProblemIsSubComponent PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

type BenchTargetProblem = TargetProblem BenchProblem

noBenchmarksProblem :: TargetSelector -> TargetProblem BenchProblem
noBenchmarksProblem = CustomTargetProblem . TargetProblemNoBenchmarks

componentNotBenchmarkProblem :: PackageId -> ComponentName -> TargetProblem BenchProblem
componentNotBenchmarkProblem pkgid name =
  CustomTargetProblem $
    TargetProblemComponentNotBenchmark pkgid name

isSubComponentProblem
  :: PackageId
  -> ComponentName
  -> SubComponentTarget
  -> TargetProblem BenchProblem
isSubComponentProblem pkgid name subcomponent =
  CustomTargetProblem $
    TargetProblemIsSubComponent pkgid name subcomponent

reportTargetProblems :: Verbosity -> [BenchTargetProblem] -> IO a
reportTargetProblems verbosity =
  dieWithException verbosity . RenderBenchTargetProblem . map renderBenchTargetProblem

renderBenchTargetProblem :: BenchTargetProblem -> String
renderBenchTargetProblem (TargetProblemNoTargets targetSelector) =
  case targetSelectorFilter targetSelector of
    Just kind
      | kind /= BenchKind ->
          "The bench command is for running benchmarks, but the target '"
            ++ showTargetSelector targetSelector
            ++ "' refers to "
            ++ renderTargetSelector targetSelector
            ++ "."
    _ -> renderTargetProblemNoTargets "benchmark" targetSelector
renderBenchTargetProblem problem =
  renderTargetProblem "benchmark" renderBenchProblem problem

renderBenchProblem :: BenchProblem -> String
renderBenchProblem (TargetProblemNoBenchmarks targetSelector) =
  "Cannot run benchmarks for the target '"
    ++ showTargetSelector targetSelector
    ++ "' which refers to "
    ++ renderTargetSelector targetSelector
    ++ " because "
    ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
    ++ " not contain any benchmarks."
renderBenchProblem (TargetProblemComponentNotBenchmark pkgid cname) =
  "The bench command is for running benchmarks, but the target '"
    ++ showTargetSelector targetSelector
    ++ "' refers to "
    ++ renderTargetSelector targetSelector
    ++ " from the package "
    ++ prettyShow pkgid
    ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent
renderBenchProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
  "The bench command can only run benchmarks as a whole, "
    ++ "not files or modules within them, but the target '"
    ++ showTargetSelector targetSelector
    ++ "' refers to "
    ++ renderTargetSelector targetSelector
    ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget
