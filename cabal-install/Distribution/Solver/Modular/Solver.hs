{-# LANGUAGE CPP #-}
#ifdef DEBUG_TRACETREE
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
module Distribution.Solver.Modular.Solver
    ( SolverConfig(..)
    , solve
    ) where

import Data.Map as M
import Data.List as L
import Data.Version

import Distribution.Compiler (CompilerInfo)

import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PackagePreferences
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.Settings

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
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.PSQ as PSQ

import Distribution.Simple.Setup (BooleanFlag(..))

#ifdef DEBUG_TRACETREE
import Distribution.Solver.Modular.Flag
import qualified Distribution.Solver.Modular.ConflictSet as CS

import Debug.Trace.Tree (gtraceJson)
import Debug.Trace.Tree.Simple
import Debug.Trace.Tree.Generic
import Debug.Trace.Tree.Assoc (Assoc(..))
#endif

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
  detectCycles     $
  heuristicsPhase  $
  preferencesPhase $
  validationPhase  $
  prunePhase       $
  buildPhase
  where
    explorePhase     = backjumpAndExplore (enableBackjumping sc)
    detectCycles     = traceTree "cycles.json" id . detectCyclesPhase
    heuristicsPhase  = (if asBool (preferEasyGoalChoices sc)
                         then P.preferEasyGoalChoices -- also leaves just one choice
                         else P.firstGoal) . -- after doing goal-choice heuristics, commit to the first choice (saves space)
                       traceTree "heuristics.json" id .
                       P.deferWeakFlagChoices .
                       P.deferSetupChoices .
                       P.preferBaseGoalChoice
    preferencesPhase = P.preferLinked .
                       P.preferPackagePreferences userPrefs
    validationPhase  = traceTree "validated.json" id .
                       P.enforceManualFlags . -- can only be done after user constraints
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
    buildPhase       = traceTree "build.json" id
                     $ addLinking
                     $ buildTree idx (independentGoals sc) userGoals

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
instance GSimpleTree (Tree QGoalReason) where
  fromGeneric = go
    where
      go :: Tree QGoalReason -> SimpleTree
      go (PChoice qpn _     psq) = Node "P" $ Assoc $ L.map (uncurry (goP qpn)) $ PSQ.toList psq
      go (FChoice _   _ _ _ psq) = Node "F" $ Assoc $ L.map (uncurry goFS)      $ PSQ.toList psq
      go (SChoice _   _ _   psq) = Node "S" $ Assoc $ L.map (uncurry goFS)      $ PSQ.toList psq
      go (GoalChoice        psq) = Node "G" $ Assoc $ L.map (uncurry goG)       $ PSQ.toList psq
      go (Done _rdm)             = Node "D" $ Assoc []
      go (Fail cs _reason)       = Node "X" $ Assoc [("CS", Leaf $ goCS cs)]

      -- Show package choice
      goP :: QPN -> POption -> Tree QGoalReason -> (String, SimpleTree)
      goP _        (POption (I ver _loc) Nothing)  subtree = (showVersion ver, go subtree)
      goP (Q _ pn) (POption _           (Just pp)) subtree = (showQPN (Q pp pn), go subtree)

      -- Show flag or stanza choice
      goFS :: Bool -> Tree QGoalReason -> (String, SimpleTree)
      goFS val subtree = (show val, go subtree)

      -- Show goal choice
      goG :: Goal QPN -> Tree QGoalReason -> (String, SimpleTree)
      goG (Goal var gr) subtree = (showVar var ++ " (" ++ shortGR gr ++ ")", go subtree)

      -- Variation on 'showGR' that produces shorter strings
      -- (Actually, QGoalReason records more info than necessary: we only need
      -- to know the variable that introduced the goal, not the value assigned
      -- to that variable)
      shortGR :: QGoalReason -> String
      shortGR UserGoal               = "user"
      shortGR (PDependency (PI nm _)) = showQPN nm
      shortGR (FDependency nm _)      = showQFN nm
      shortGR (SDependency nm)        = showQSN nm

      -- Show conflict set
      goCS :: ConflictSet QPN -> String
      goCS cs = "{" ++ (intercalate "," . L.map showVar . CS.toList $ cs) ++ "}"
#endif

-- | Replace all goal reasons with a dummy goal reason in the tree
--
-- This is useful for debugging (when experimenting with the impact of GRs)
_removeGR :: Tree QGoalReason -> Tree QGoalReason
_removeGR = trav go
  where
   go :: TreeF QGoalReason (Tree QGoalReason) -> TreeF QGoalReason (Tree QGoalReason)
   go (PChoiceF qpn _     psq) = PChoiceF qpn dummy     psq
   go (FChoiceF qfn _ a b psq) = FChoiceF qfn dummy a b psq
   go (SChoiceF qsn _ a   psq) = SChoiceF qsn dummy a   psq
   go (GoalChoiceF        psq) = GoalChoiceF            (goG psq)
   go (DoneF rdm)              = DoneF rdm
   go (FailF cs reason)        = FailF cs reason

   goG :: PSQ (Goal QPN) (Tree QGoalReason) -> PSQ (Goal QPN) (Tree QGoalReason)
   goG = PSQ.fromList
       . L.map (\(Goal var _, subtree) -> (Goal var dummy, subtree))
       . PSQ.toList

   dummy :: QGoalReason
   dummy = PDependency
         $ PI (Q (PackagePath DefaultNamespace Unqualified) (PackageName "$"))
              (I (Version [1] []) InRepo)
