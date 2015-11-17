module Distribution.Solver.Modular.Solver
    ( SolverConfig(..)
    , solve
    ) where

import Data.Map as M

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Builder
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Explore
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.Preference as P
import Distribution.Solver.Modular.Validate
import Distribution.Solver.Modular.Linking
import Distribution.Solver.Types

-- | Various options for the modular solver.
data SolverConfig = SolverConfig {
  preferEasyGoalChoices :: Bool,
  independentGoals      :: Bool,
  avoidReinstalls       :: Bool,
  shadowPkgs            :: Bool,
  strongFlags           :: Bool,
  maxBackjumps          :: Maybe Int
}

solve :: SolverConfig ->          -- solver parameters
         Index ->                 -- all available packages as an index
         (PN -> PackagePreferences) ->        -- preferences
         Map PN [LabeledPackageConstraint] -> -- global constraints
         [PN] ->                              -- global goals
         Log Message (Assignment, RevDepMap)
solve sc idx userPrefs userConstraints userGoals =
  explorePhase     $
  heuristicsPhase  $
  preferencesPhase $
  validationPhase  $
  prunePhase       $
  buildPhase
  where
    explorePhase     = exploreTreeLog . backjump
    heuristicsPhase  = P.firstGoal . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       P.deferSetupChoices .
                       P.deferWeakFlagChoices .
                       P.preferBaseGoalChoice .
                       if preferEasyGoalChoices sc
                         then P.lpreferEasyGoalChoices
                         else id .
                       P.preferLinked
    preferencesPhase = P.preferPackagePreferences userPrefs
    validationPhase  = P.enforceManualFlags . -- can only be done after user constraints
                       P.enforcePackageConstraints userConstraints .
                       P.enforceSingleInstanceRestriction .
                       validateLinking idx .
                       validateTree idx
    prunePhase       = (if avoidReinstalls sc then P.avoidReinstalls (const True) else id) .
                       -- packages that can never be "upgraded":
                       P.requireInstalled (`elem` [ PackageName "base"
                                                  , PackageName "ghc-prim"
                                                  , PackageName "integer-gmp"
                                                  , PackageName "integer-simple"
                                                  ])
    buildPhase       = addLinking $ buildTree idx (independentGoals sc) userGoals
