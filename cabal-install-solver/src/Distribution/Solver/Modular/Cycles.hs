{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Solver.Modular.Cycles (
    findCycles
  ) where

import Prelude hiding (cycle)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Distribution.Compat.Graph as G
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Modular.ValidateDependencies


-- | Given the reverse dependency map from a node in the tree, check
-- if the solution is cyclic. If it is, return the conflict set containing
-- all decisions that could potentially break the cycle.
--
-- TODO: The conflict set should also contain flag and stanza variables.
findCycles :: QPN -> RevDepMap -> Maybe (ConflictSet, FailReason)
findCycles pkg rdm = (,CyclicDependencies) <$>
    -- This function has two parts: a faster cycle check that is called at every
    -- step and a slower calculation of the conflict set.
    --
    -- 'hasCycle' checks for cycles incrementally by only looking for cycles
    -- containing the current package, 'pkg'. It searches for cycles in the
    -- 'RevDepMap', which is the data structure used to store reverse
    -- dependencies in the search tree. We store the reverse dependencies in a
    -- map, because Data.Map is smaller and/or has better sharing than
    -- Distribution.Compat.Graph.
    --
    -- If there is a cycle, we call G.cycles to find a strongly connected
    -- component. Then we choose one cycle from the component to use for the
    -- conflict set. Choosing only one cycle can lead to a smaller conflict set,
    -- such as when a choice to enable testing introduces many cycles at once.
    -- In that case, all cycles contain the current package and are in one large
    -- strongly connected component.
    --
    if hasCycle
    then let scc :: G.Graph RevDepMapNode
             scc = case G.cycles $ revDepMapToGraph rdm of
                     []    -> findCyclesError "cannot find a strongly connected component"
                     c : _ -> G.fromDistinctList c

             next :: QPN -> QPN
             next p = case G.neighbors scc p of
                        Just (n : _) -> G.nodeKey n
                        _            -> findCyclesError "cannot find next node in the cycle"

             -- This function also assumes that all cycles contain 'pkg'.
             oneCycle :: [QPN]
             oneCycle = case iterate next pkg of
                          []     -> findCyclesError "empty cycle"
                          x : xs -> x : takeWhile (/= x) xs
         in Just $ CS.fromList $ map P oneCycle
    else Nothing
  where
    hasCycle :: Bool
    hasCycle = pkg `S.member` closure (neighbors pkg)

    closure :: [QPN] -> S.Set QPN
    closure = foldl go S.empty
      where
        go :: S.Set QPN -> QPN -> S.Set QPN
        go s x =
            if x `S.member` s
            then s
            else foldl go (S.insert x s) $ neighbors x

    neighbors :: QPN -> [QPN]
    neighbors x = case x `M.lookup` revDeps rdm of
                    Nothing -> findCyclesError "cannot find node"
                    Just xs -> map snd xs

    findCyclesError = error . ("Distribution.Solver.Modular.Cycles.findCycles: " ++)

