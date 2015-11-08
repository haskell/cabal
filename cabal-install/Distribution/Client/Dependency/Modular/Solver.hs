module Distribution.Client.Dependency.Modular.Solver
    ( SolverConfig(..)
    , solve
    ) where

import Data.Map as M

import Distribution.Compiler (CompilerInfo)

import Distribution.Client.Dependency.Types

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Builder
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Explore
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Log
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import qualified Distribution.Client.Dependency.Modular.Preference as P
import Distribution.Client.Dependency.Modular.Validate
import Distribution.Client.Dependency.Modular.Linking

-- | Various options for the modular solver.
data SolverConfig = SolverConfig {
  preferEasyGoalChoices :: Bool,
  independentGoals      :: Bool,
  avoidReinstalls       :: Bool,
  shadowPkgs            :: Bool,
  strongFlags           :: Bool,
  maxBackjumps          :: Maybe Int
}

solve :: SolverConfig ->                      -- solver parameters
         CompilerInfo ->
         Index ->                             -- all available packages as an index
         (PN -> PackagePreferences) ->        -- preferences
         Map PN [LabeledPackageConstraint] -> -- global constraints
         [PN] ->                              -- global goals
         Log Message (Assignment, RevDepMap)
solve sc cinfo idx userPrefs userConstraints userGoals =
  explorePhase     $
  heuristicsPhase  $
  preferencesPhase $
  validationPhase  $
  prunePhase       $
  buildPhase
  where
    explorePhase     = backjumpAndExplore
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
                       validateTree cinfo idx
    prunePhase       = (if avoidReinstalls sc then P.avoidReinstalls (const True) else id) .
                       -- packages that can never be "upgraded":
                       P.requireInstalled (`elem` [ PackageName "base"
                                                  , PackageName "ghc-prim"
                                                  , PackageName "integer-gmp"
                                                  , PackageName "integer-simple"
                                                  ])
    buildPhase       = addLinking $ buildTree idx (independentGoals sc) userGoals
