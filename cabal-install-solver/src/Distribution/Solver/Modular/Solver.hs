{-# LANGUAGE CPP #-}
#ifdef DEBUG_TRACETREE
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
module Distribution.Solver.Modular.Solver
    ( SolverConfig(..)
    , solve
    , PruneAfterFirstSuccess(..)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Distribution.Verbosity

import Distribution.Compiler (CompilerInfo)

import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PackagePreferences
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.Variable

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
import Distribution.Solver.Modular.PSQ (PSQ)
import Distribution.Solver.Modular.RetryLog
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.PSQ as PSQ

import Distribution.Simple.Setup (BooleanFlag(..))

#ifdef DEBUG_TRACETREE
import qualified Distribution.Solver.Modular.ConflictSet as CS
import qualified Distribution.Solver.Modular.WeightedPSQ as W
import qualified Distribution.Deprecated.Text as T

import Debug.Trace.Tree (gtraceJson)
import Debug.Trace.Tree.Simple
import Debug.Trace.Tree.Generic
import Debug.Trace.Tree.Assoc (Assoc(..))
#endif

-- | Various options for the modular solver.
data SolverConfig = SolverConfig {
  reorderGoals           :: ReorderGoals,
  countConflicts         :: CountConflicts,
  fineGrainedConflicts   :: FineGrainedConflicts,
  minimizeConflictSet    :: MinimizeConflictSet,
  independentGoals       :: IndependentGoals,
  avoidReinstalls        :: AvoidReinstalls,
  shadowPkgs             :: ShadowPkgs,
  strongFlags            :: StrongFlags,
  onlyConstrained        :: OnlyConstrained,
  maxBackjumps           :: Maybe Int,
  enableBackjumping      :: EnableBackjumping,
  solveExecutables       :: SolveExecutables,
  goalOrder              :: Maybe (Variable QPN -> Variable QPN -> Ordering),
  solverVerbosity        :: Verbosity,
  pruneAfterFirstSuccess :: PruneAfterFirstSuccess
}

-- | Whether to remove all choices after the first successful choice at each
-- level in the search tree.
newtype PruneAfterFirstSuccess = PruneAfterFirstSuccess Bool

-- | Run all solver phases.
--
-- In principle, we have a valid tree after 'validationPhase', which
-- means that every 'Done' node should correspond to valid solution.
--
-- There is one exception, though, and that is cycle detection, which
-- has been added relatively recently. Cycles are only removed directly
-- before exploration.
--
solve :: SolverConfig                         -- ^ solver parameters
      -> CompilerInfo
      -> Index                                -- ^ all available packages as an index
      -> PkgConfigDb                          -- ^ available pkg-config pkgs
      -> (PN -> PackagePreferences)           -- ^ preferences
      -> M.Map PN [LabeledPackageConstraint]  -- ^ global constraints
      -> S.Set PN                             -- ^ global goals
      -> RetryLog Message SolverFailure (Assignment, RevDepMap)
solve sc cinfo idx pkgConfigDB userPrefs userConstraints userGoals =
  explorePhase      .
  traceTree "cycles.json" id .
  detectCycles      .
  traceTree "heuristics.json" id .
  trav (
   heuristicsPhase  .
   preferencesPhase .
   validationPhase
  ) .
  traceTree "semivalidated.json" id .
  validationCata    .
  traceTree "pruned.json" id .
  trav prunePhase   .
  traceTree "build.json" id $
  buildPhase
  where
    explorePhase     = backjumpAndExplore (maxBackjumps sc)
                                          (enableBackjumping sc)
                                          (fineGrainedConflicts sc)
                                          (countConflicts sc)
                                          idx
    detectCycles     = detectCyclesPhase
    heuristicsPhase  =
      let
          sortGoals = case goalOrder sc of
                        Nothing -> goalChoiceHeuristics .
                                   P.deferSetupExeChoices .
                                   P.deferWeakFlagChoices .
                                   P.preferBaseGoalChoice
                        Just order -> P.firstGoal .
                                      P.sortGoals order
          PruneAfterFirstSuccess prune = pruneAfterFirstSuccess sc
      in sortGoals .
         (if prune then P.pruneAfterFirstSuccess else id)
    preferencesPhase = P.preferLinked .
                       P.preferPackagePreferences userPrefs
    validationPhase  = P.enforcePackageConstraints userConstraints .
                       P.enforceManualFlags userConstraints
    validationCata   = P.enforceSingleInstanceRestriction .
                       validateLinking idx .
                       validateTree cinfo idx pkgConfigDB
    prunePhase       = (if asBool (avoidReinstalls sc) then P.avoidReinstalls (const True) else id) .
                       (case onlyConstrained sc of
                          OnlyConstrainedAll ->
                            P.onlyConstrained pkgIsExplicit
                          OnlyConstrainedNone ->
                            id)
    buildPhase       = buildTree idx (independentGoals sc) (S.toList userGoals)

    allExplicit = M.keysSet userConstraints `S.union` userGoals

    pkgIsExplicit :: PN -> Bool
    pkgIsExplicit pn = S.member pn allExplicit

    -- When --reorder-goals is set, we use preferReallyEasyGoalChoices, which
    -- prefers (keeps) goals only if the have 0 or 1 enabled choice.
    --
    -- In the past, we furthermore used P.firstGoal to trim down the goal choice nodes
    -- to just a single option. This was a way to work around a space leak that was
    -- unnecessary and is now fixed, so we no longer do it.
    --
    -- If --count-conflicts is active, it will then choose among the remaining goals
    -- the one that has been responsible for the most conflicts so far.
    --
    -- Otherwise, we simply choose the first remaining goal.
    --
    goalChoiceHeuristics
      | asBool (reorderGoals sc) = P.preferReallyEasyGoalChoices
      | otherwise                = id {- P.firstGoal -}

-- | Dump solver tree to a file (in debugging mode)
--
-- This only does something if the @debug-tracetree@ configure argument was
-- given; otherwise this is just the identity function.
traceTree ::
#ifdef DEBUG_TRACETREE
  GSimpleTree a =>
#endif
     FilePath  -- ^ Output file
  -> (a -> a)  -- ^ Function to summarize the tree before dumping
  -> a -> a
#ifdef DEBUG_TRACETREE
traceTree = gtraceJson
#else
traceTree _ _ = id
#endif

#ifdef DEBUG_TRACETREE
instance GSimpleTree (Tree d c) where
  fromGeneric = go
    where
      go :: Tree d c -> SimpleTree
      go (PChoice qpn _ _       psq) = Node "P" $ Assoc $ L.map (uncurry (goP qpn)) $ psqToList  psq
      go (FChoice _   _ _ _ _ _ psq) = Node "F" $ Assoc $ L.map (uncurry goFS)      $ psqToList  psq
      go (SChoice _   _ _ _     psq) = Node "S" $ Assoc $ L.map (uncurry goFS)      $ psqToList  psq
      go (GoalChoice  _         psq) = Node "G" $ Assoc $ L.map (uncurry goG)       $ PSQ.toList psq
      go (Done _rdm _s)              = Node "D" $ Assoc []
      go (Fail cs _reason)           = Node "X" $ Assoc [("CS", Leaf $ goCS cs)]

      psqToList :: W.WeightedPSQ w k v -> [(k, v)]
      psqToList = L.map (\(_, k, v) -> (k, v)) . W.toList

      -- Show package choice
      goP :: QPN -> POption -> Tree d c -> (String, SimpleTree)
      goP _        (POption (I ver _loc) Nothing)  subtree = (T.display ver, go subtree)
      goP (Q _ pn) (POption _           (Just pp)) subtree = (showQPN (Q pp pn), go subtree)

      -- Show flag or stanza choice
      goFS :: Bool -> Tree d c -> (String, SimpleTree)
      goFS val subtree = (show val, go subtree)

      -- Show goal choice
      goG :: Goal QPN -> Tree d c -> (String, SimpleTree)
      goG (Goal var gr) subtree = (showVar var ++ " (" ++ shortGR gr ++ ")", go subtree)

      -- Variation on 'showGR' that produces shorter strings
      -- (Actually, QGoalReason records more info than necessary: we only need
      -- to know the variable that introduced the goal, not the value assigned
      -- to that variable)
      shortGR :: QGoalReason -> String
      shortGR UserGoal            = "user"
      shortGR (DependencyGoal dr) = showDependencyReason dr

      -- Show conflict set
      goCS :: ConflictSet -> String
      goCS cs = "{" ++ (intercalate "," . L.map showVar . CS.toList $ cs) ++ "}"
#endif

-- | Replace all goal reasons with a dummy goal reason in the tree
--
-- This is useful for debugging (when experimenting with the impact of GRs)
_removeGR :: Tree d c -> Tree d QGoalReason
_removeGR = trav go
  where
   go :: TreeF d c (Tree d QGoalReason) -> TreeF d QGoalReason (Tree d QGoalReason)
   go (PChoiceF qpn rdm _       psq) = PChoiceF qpn rdm dummy       psq
   go (FChoiceF qfn rdm _ a b d psq) = FChoiceF qfn rdm dummy a b d psq
   go (SChoiceF qsn rdm _ a     psq) = SChoiceF qsn rdm dummy a     psq
   go (GoalChoiceF  rdm         psq) = GoalChoiceF  rdm             (goG psq)
   go (DoneF rdm s)                  = DoneF rdm s
   go (FailF cs reason)              = FailF cs reason

   goG :: PSQ (Goal QPN) (Tree d QGoalReason) -> PSQ (Goal QPN) (Tree d QGoalReason)
   goG = PSQ.fromList
       . L.map (\(Goal var _, subtree) -> (Goal var dummy, subtree))
       . PSQ.toList

   dummy :: QGoalReason
   dummy =
       DependencyGoal $
       DependencyReason
           (Q (PackagePath DefaultNamespace QualToplevel) (mkPackageName "$"))
           M.empty S.empty
