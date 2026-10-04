-- | Building the dependencies of a project, and nothing else: everything
-- @cabal build all --only-dependencies@ would build, except the packages
-- that buck2 builds from source itself (see 'isBuiltLocally').
module Distribution.Client.Buck2.BuildDependencies
  ( buildDependencies
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Distribution.Client.CmdBuild as CmdBuild
import Distribution.Client.CmdErrorMessages (renderCannotPruneDependencies, reportTargetProblems)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectBuilding (unpackInplaceSources)
import Distribution.Client.ProjectConfig (projectConfigWithBuilderRepoContext)
import Distribution.Client.ProjectOrchestration

-- 'pruneInstallPlanToTargets' is hidden: 'ProjectOrchestration' re-exports
-- its own wrapper of the same name (taking a 'TargetsMap' directly,
-- matching what 'resolveTargetsFromSolver' below returns), which would
-- otherwise be ambiguous with 'ProjectPlanning's lower-level original.
import Distribution.Client.ProjectPlanning hiding (pruneInstallPlanToTargets)

import Distribution.Simple.Utils (dieWithException, notice)

import Distribution.Client.Buck2.LocalPackages (isBuiltLocally)
import Distribution.Client.Errors (CabalInstallException (ReportCannotPruneDependencies))

-- | Build every dependency of the selected targets - never the local
-- packages themselves - exactly as @cabal build --only-dependencies@ would,
-- by resolving and pruning the install plan the same way
-- 'CmdBuild.buildAction' does (reusing its 'CmdBuild.selectPackageTargets'
-- and 'CmdBuild.selectComponentTarget'), so every flag @cabal build@
-- understands (@-f...@, @--enable-profiling@, @--enable-tests@, ...)
-- works here too. Doing this in-process, rather than delegating to
-- 'CmdBuild.buildAction' as an opaque action, is what lets the caller see
-- the same (test\/benchmark-flag-aware) dependency closure that just got
-- built, in 'elaboratedPlanToExecute'.
--
-- Packages that buck2 builds itself are also excluded when they aren't
-- local to the project (they build inplace because they depend on a local
-- package), but ones that come from a tarball still need their source
-- unpacked, which cabal only does as a side effect of building them: that
-- part is done here too.
buildDependencies :: Verbosity -> ProjectBaseContext -> [TargetSelector] -> IO ProjectBuildContext
buildDependencies verbosity baseCtx targetSelectors = do
  buildCtx@ProjectBuildContext{elaboratedPlanOriginal, elaboratedShared} <-
    runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
      targets <-
        either (reportTargetProblems verbosity "buck2") return $
          resolveTargetsFromSolver
            CmdBuild.selectPackageTargets
            CmdBuild.selectComponentTarget
            elaboratedPlan
            Nothing
            targetSelectors
      let elaboratedPlan' = pruneInstallPlanToTargets TargetActionBuild targets elaboratedPlan
          excluded =
            Map.keysSet targets
              <> Set.fromList
                [ elabUnitId elab
                | InstallPlan.Configured elab <- InstallPlan.toList elaboratedPlan'
                , isBuiltLocally elab
                ]
      elaboratedPlan'' <-
        either (dieWithException verbosity . ReportCannotPruneDependencies . renderCannotPruneDependencies) return $
          pruneInstallPlanToDependencies excluded elaboratedPlan'
      return (elaboratedPlan'', targets)

  notice verbosity "cabal buck2: building dependencies (cabal build all --only-dependencies)"
  printPlan verbosity baseCtx buildCtx
  buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
  runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

  unpackInplaceSources
    verbosity
    (distDirLayout baseCtx)
    elaboratedShared
    (projectConfigWithBuilderRepoContext verbosity (buildSettings baseCtx))
    [ elab
    | InstallPlan.Configured elab <- InstallPlan.toList elaboratedPlanOriginal
    , isBuiltLocally elab
    , not (elabLocalToProject elab)
    ]
  return buildCtx
