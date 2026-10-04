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

import Distribution.Client.ProjectPlanning hiding
  ( pruneInstallPlanToTargets -- we want the one from 'ProjectOrchestration'.
  )

import Distribution.Simple.Utils (dieWithException, notice)

import Distribution.Client.Buck2.LocalPackages (isBuiltLocally)
import Distribution.Client.Errors (CabalInstallException (ReportCannotPruneDependencies))

-- | Build every dependency of the selected targets, excluding packages
-- that are forced to build locally (see 'isBuiltLocally').
-- This is similar but not quite the same as the code for @CmdBuild@.
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

  notice verbosity "cabal buck2: building dependencies"
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
