{-# LANGUAGE CPP #-}
module Distribution.Client.Dependency.Modular.Cycles (
    detectCyclesPhase
  ) where

import Prelude hiding (cycle)
import Data.Graph (SCC)
import qualified Data.Graph as Gr
import qualified Data.Map   as Map

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.ConflictSet as CS

-- | Find and reject any solutions that are cyclic
detectCyclesPhase :: Tree QGoalReason -> Tree QGoalReason
detectCyclesPhase = cata go
  where
    -- The only node of interest is DoneF
    go :: TreeF QGoalReason (Tree QGoalReason) -> Tree QGoalReason
    go (PChoiceF qpn gr     cs) = PChoice qpn gr     cs
    go (FChoiceF qfn gr w m cs) = FChoice qfn gr w m cs
    go (SChoiceF qsn gr w   cs) = SChoice qsn gr w   cs
    go (GoalChoiceF         cs) = GoalChoice         cs
    go (FailF cs reason)        = Fail cs reason

    -- We check for cycles only if we have actually found a solution
    -- This minimizes the number of cycle checks we do as cycles are rare
    go (DoneF revDeps) = do
      case findCycles revDeps of
        Nothing     -> Done revDeps
        Just relSet -> Fail relSet CyclicDependencies

-- | Given the reverse dependency map from a 'Done' node in the tree, as well
-- as the full conflict set containing all decisions that led to that 'Done'
-- node, check if the solution is cyclic. If it is, return the conflict set
-- containing all decisions that could potentially break the cycle.
findCycles :: RevDepMap -> Maybe (ConflictSet QPN)
findCycles revDeps =
    case cycles of
      []  -> Nothing
      c:_ -> Just $ CS.unions $ map (varToConflictSet . P) c
  where
    cycles :: [[QPN]]
    cycles = [vs | Gr.CyclicSCC vs <- scc]

    scc :: [SCC QPN]
    scc = Gr.stronglyConnComp . map aux . Map.toList $ revDeps

    aux :: (QPN, [(comp, QPN)]) -> (QPN, QPN, [QPN])
    aux (fr, to) = (fr, fr, map snd to)
