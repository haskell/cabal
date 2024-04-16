{-# LANGUAGE TypeFamilies #-}
module Distribution.Solver.Modular.ValidateDependencies (
    detectInvalidDepGraphPhase
  , RevDepMapNode(..)
  , revDepMapToGraph
  ) where

import Prelude hiding (cycle)
import qualified Data.Map as M

import qualified Distribution.Compat.Graph as G
import Distribution.Simple.Utils (ordNub)
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Types.ComponentDeps (Component)
import Distribution.Solver.Types.PackagePath

-- | Run checks on the dependencies at every node, such as the cycle check
-- 'findCycles' and the private scope closure check 'findBadPrivClosures'
detectInvalidDepGraphPhase :: [QPN -> RevDepMap -> Maybe (ConflictSet, FailReason)]
                           -- ^ The list of checks to run at every node, run left to right.
                           -> Tree d c -> Tree d c
detectInvalidDepGraphPhase checks = go
  where
    -- Only check children of choice nodes.
    go :: Tree d c -> Tree d c
    go (PChoice qpn rdm gr                         cs) =
        PChoice qpn rdm gr     $ fmap (checkChild qpn)   (fmap go cs)
    go (FChoice qfn@(FN qpn _) rdm gr w m d cs) =
        FChoice qfn rdm gr w m d $ fmap (checkChild qpn) (fmap go cs)
    go (SChoice qsn@(SN qpn _) rdm gr w     cs) =
        SChoice qsn rdm gr w   $ fmap (checkChild qpn)   (fmap go cs)
    go (GoalChoice rdm cs) = GoalChoice rdm (fmap go cs)
    go x@(Fail _ _) = x
    go x@(Done _ _) = x

    checkChild :: QPN -> Tree d c -> Tree d c
    checkChild qpn x@(PChoice _  rdm _       _) = failIfBad qpn rdm x
    checkChild qpn x@(FChoice _  rdm _ _ _ _ _) = failIfBad qpn rdm x
    checkChild qpn x@(SChoice _  rdm _ _     _) = failIfBad qpn rdm x
    checkChild qpn x@(GoalChoice rdm         _) = failIfBad qpn rdm x
    checkChild _   x@(Fail _ _)                 = x
    checkChild qpn x@(Done       rdm _)         = failIfBad qpn rdm x

    failIfBad :: QPN -> RevDepMap -> Tree d c -> Tree d c
    failIfBad qpn rdm x = maybe x (uncurry Fail) (goCheck checks) where
      goCheck :: [QPN -> RevDepMap -> Maybe (ConflictSet, FailReason)] -> Maybe (ConflictSet, FailReason)
      goCheck [] = Nothing -- no more check, succeed
      goCheck (c:cs) =
        case c qpn rdm of
          Nothing ->
            -- success for this check, but we need to run all checks
            goCheck cs
          Just f -> Just f

data RevDepMapNode = RevDepMapNode QPN [(Component, QPN)]

instance G.IsNode RevDepMapNode where
  type Key RevDepMapNode = QPN
  nodeKey (RevDepMapNode qpn _) = qpn
  nodeNeighbors (RevDepMapNode _ ns) = ordNub $ map snd ns

revDepMapToGraph :: RevDepMap -> G.Graph RevDepMapNode
revDepMapToGraph rdm = G.fromDistinctList
                       [RevDepMapNode qpn ns | (qpn, ns) <- M.toList $ revDeps rdm]
