{-# LANGUAGE CPP #-}
module Distribution.Client.Dependency.Modular.Cycles (
    detectCyclesPhase
  ) where

import Prelude hiding (cycle)
import Control.Monad
import Control.Monad.Reader
import Data.Graph (SCC)
import Data.Set   (Set)
import qualified Data.Graph       as Gr
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Traversable as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree

type DetectCycles = Reader (ConflictSet QPN)

-- | Find and reject any solutions that are cyclic
detectCyclesPhase :: Tree QGoalReasonChain -> Tree QGoalReasonChain
detectCyclesPhase = (`runReader` Set.empty) .  cata go
  where
    -- Most cases are simple; we just need to remember which choices we made
    go :: TreeF QGoalReasonChain (DetectCycles (Tree QGoalReasonChain)) -> DetectCycles (Tree QGoalReasonChain)
    go (PChoiceF qpn gr     cs) = PChoice qpn gr     <$> local (extendConflictSet $ P qpn) (T.sequence cs)
    go (FChoiceF qfn gr w m cs) = FChoice qfn gr w m <$> local (extendConflictSet $ F qfn) (T.sequence cs)
    go (SChoiceF qsn gr w   cs) = SChoice qsn gr w   <$> local (extendConflictSet $ S qsn) (T.sequence cs)
    go (GoalChoiceF         cs) = GoalChoice         <$>                                   (T.sequence cs)
    go (FailF cs reason)        = return $ Fail cs reason

    -- We check for cycles only if we have actually found a solution
    -- This minimizes the number of cycle checks we do as cycles are rare
    go (DoneF revDeps) = do
      fullSet <- ask
      return $ case findCycles fullSet revDeps of
        Nothing     -> Done revDeps
        Just relSet -> Fail relSet CyclicDependencies

-- | Given the reverse dependency map from a 'Done' node in the tree, as well
-- as the full conflict set containing all decisions that led to that 'Done'
-- node, check if the solution is cyclic. If it is, return the conflict set
-- containing all decisions that could potentially break the cycle.
findCycles :: ConflictSet QPN -> RevDepMap -> Maybe (ConflictSet QPN)
findCycles fullSet revDeps = do
    guard $ not (null cycles)
    return $ relevantConflictSet (Set.fromList (concat cycles)) fullSet
  where
    cycles :: [[QPN]]
    cycles = [vs | Gr.CyclicSCC vs <- scc]

    scc :: [SCC QPN]
    scc = Gr.stronglyConnComp . map aux . Map.toList $ revDeps

    aux :: (QPN, [(comp, QPN)]) -> (QPN, QPN, [QPN])
    aux (fr, to) = (fr, fr, map snd to)

-- | Construct the relevant conflict set given the full conflict set that
-- lead to this decision and the set of packages involved in the cycle
relevantConflictSet :: Set QPN -> ConflictSet QPN -> ConflictSet QPN
relevantConflictSet cycle = Set.filter isRelevant
  where
    isRelevant :: Var QPN -> Bool
    isRelevant (P qpn)                  = qpn `Set.member` cycle
    isRelevant (F (FN (PI qpn _i) _fn)) = qpn `Set.member` cycle
    isRelevant (S (SN (PI qpn _i) _sn)) = qpn `Set.member` cycle
