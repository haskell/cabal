module Distribution.Client.Dependency.Modular.Solver where

import Data.Map as M

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

-- Configurable interface to the solver.

data SolverConfig = SolverConfig {
  preferEasyGoalChoices :: Bool,
  pStrategy             :: PackagesPreferenceDefault,
  avoidReinstalls       :: Bool
}

defaultSolverConfig :: SolverConfig
defaultSolverConfig = SolverConfig {
  preferEasyGoalChoices = True,
  pStrategy             = PreferLatestForSelected, -- latest for goals only
  avoidReinstalls       = True
}

originalSolve :: SolverConfig ->   -- solver parameters
                 Index ->          -- all available packages as an index
                 Preferences ->    -- global preferences
                 Preferences ->    -- user preferences (override the global preferences)
                 [PN] ->           -- global goals
                 [Dep PN] ->       -- global constraints
                 P.GlobalFlags ->  -- global flag choices
                 P.LocalFlags ->   -- local flag choices
                 Log Message (Assignment, RevDepMap)
originalSolve sc idx globalPrefs userPrefs userGoals userDeps globalFlags localFlags =
  explorePhase     $
  heuristicsPhase  $
  preferencesPhase $
  pStrategyPhase   $
  validationPhase  $
  prunePhase       $
  buildPhase
  where
    explorePhase = exploreTreeLog
    heuristicsPhase = if preferEasyGoalChoices sc
                        then P.deferDefaultFlagChoices . P.preferEasyGoalChoices
                        else id
    preferencesPhase = P.preferPreferences   userPrefs .
                       P.preferPreferences globalPrefs
    pStrategyPhase = case pStrategy sc of
                       PreferAllLatest ->
                         P.preferLatest . P.preferInstalled
                       PreferLatestForSelected ->
                         P.preferLatestFor (`elem` userGoals) .
                         P.preferInstalled . P.preferLatest
                       PreferAllInstalled ->
                         P.preferInstalled . P.preferLatest
    validationPhase = P.enforceFlagChoices globalFlags localFlags .
                      validateTree idx userDeps
    prunePhase = (if avoidReinstalls sc then P.avoidReinstalls (const True) else id) .
                 P.requireInstalled (== PackageName "base") -- never try to install a new "base"
    buildPhase = buildTree idx userGoals

solve :: SolverConfig ->   -- solver parameters
         Index ->          -- all available packages as an index
         (PN -> PackagePreferences) -> -- preferences
         Map PN PackageConstraint ->   -- global constraints
         [PN] ->                       -- global goals
         Log Message (Assignment, RevDepMap)
solve sc idx userPrefs userConstraints userGoals =
  explorePhase     $
  heuristicsPhase  $
  preferencesPhase $
  validationPhase  $
  prunePhase       $
  buildPhase
  where
    explorePhase = exploreTreeLog
    heuristicsPhase = if preferEasyGoalChoices sc
                        then P.deferDefaultFlagChoices . P.preferEasyGoalChoices
                        else id
    preferencesPhase = P.preferPackagePreferences userPrefs
    validationPhase = P.enforcePackageConstraints userConstraints .
                      validateTree idx []
    prunePhase = (if avoidReinstalls sc then P.avoidReinstalls (const True) else id) .
                 P.requireInstalled (== PackageName "base") -- never try to install a new "base"
    buildPhase = buildTree idx userGoals

-- | For cabal-install integration.
defaultSolver :: Index ->          -- all available packages as an index
                 (PN -> PackagePreferences) -> -- preferences
                 Map PN PackageConstraint ->   -- global constraints
                 [PN] ->                       -- global goals
                 Log Message (Assignment, RevDepMap)
defaultSolver = solve defaultSolverConfig

-- | For the standalone command-line interface (temporary).
otherSolver ::   Index ->          -- all available packages as an index
                 Preferences ->    -- global preferences
                 Preferences ->    -- user preferences (override the global preferences)
                 [PN] ->           -- global goals
                 [Dep PN] ->       -- global constraints
                 P.GlobalFlags ->  -- global flag choices
                 P.LocalFlags ->   -- local flag choices
                 Log Message (Assignment, RevDepMap)
otherSolver = originalSolve defaultSolverConfig
