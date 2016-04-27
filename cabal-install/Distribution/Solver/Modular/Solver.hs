module Distribution.Solver.Modular.Solver
    ( SolverConfig(..)
    , solve
    ) where

import Data.Map as M

import Distribution.Compiler (CompilerInfo)

import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.Settings

import Distribution.Client.Dependency.Types
         ( PackagePreferences, LabeledPackageConstraint )

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Builder
import Distribution.Solver.Modular.Cycles
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Explore
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.Preference as P
import Distribution.Solver.Modular.Validate
import Distribution.Solver.Modular.Linking

import Distribution.Simple.Setup (BooleanFlag(..))

-- | Various options for the modular solver.
data SolverConfig = SolverConfig {
  preferEasyGoalChoices :: ReorderGoals,
  independentGoals      :: IndependentGoals,
  avoidReinstalls       :: AvoidReinstalls,
  shadowPkgs            :: ShadowPkgs,
  strongFlags           :: StrongFlags,
  maxBackjumps          :: Maybe Int,
  enableBackjumping     :: EnableBackjumping
}

-- | Run all solver phases.
--
-- In principle, we have a valid tree after 'validationPhase', which
-- means that every 'Done' node should correspond to valid solution.
--
-- There is one exception, though, and that is cycle detection, which
-- has been added relatively recently. Cycles are only removed directly
-- before exploration.
--
-- Semantically, there is no difference. Cycle detection, as implemented
-- now, only occurs for 'Done' nodes we encounter during exploration,
-- and cycle detection itself does not change the shape of the tree,
-- it only marks some 'Done' nodes as 'Fail', if they contain cyclic
-- solutions.
--
-- There is a tiny performance impact, however, in doing cycle detection
-- directly after validation. Probably because cycle detection maintains
-- some information, and the various reorderings implemented by
-- 'preferencesPhase' and 'heuristicsPhase' are ever so slightly more
-- costly if that information is already around during the reorderings.
--
-- With the current positioning directly before the 'explorePhase', there
-- seems to be no statistically significant performance impact of cycle
-- detection in the common case where there are no cycles.
--
solve :: SolverConfig ->                      -- ^ solver parameters
         CompilerInfo ->
         Index ->                             -- ^ all available packages as an index
         PkgConfigDb ->                       -- ^ available pkg-config pkgs
         (PN -> PackagePreferences) ->        -- ^ preferences
         Map PN [LabeledPackageConstraint] -> -- ^ global constraints
         [PN] ->                              -- ^ global goals
         Log Message (Assignment, RevDepMap)
solve sc cinfo idx pkgConfigDB userPrefs userConstraints userGoals =
  explorePhase     $
  detectCyclesPhase$
  heuristicsPhase  $
  preferencesPhase $
  validationPhase  $
  prunePhase       $
  buildPhase
  where
    explorePhase     = backjumpAndExplore (enableBackjumping sc)
    heuristicsPhase  = (if asBool (preferEasyGoalChoices sc)
                         then P.preferEasyGoalChoices -- also leaves just one choice
                         else P.firstGoal) . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       P.deferWeakFlagChoices .
                       P.deferSetupChoices .
                       P.preferBaseGoalChoice .
                       P.preferLinked
    preferencesPhase = P.preferPackagePreferences userPrefs
    validationPhase  = P.enforceManualFlags . -- can only be done after user constraints
                       P.enforcePackageConstraints userConstraints .
                       P.enforceSingleInstanceRestriction .
                       validateLinking idx .
                       validateTree cinfo idx pkgConfigDB
    prunePhase       = (if asBool (avoidReinstalls sc) then P.avoidReinstalls (const True) else id) .
                       -- packages that can never be "upgraded":
                       P.requireInstalled (`elem` [ PackageName "base"
                                                  , PackageName "ghc-prim"
                                                  , PackageName "integer-gmp"
                                                  , PackageName "integer-simple"
                                                  ])
    buildPhase       = addLinking $ buildTree idx (independentGoals sc) userGoals
