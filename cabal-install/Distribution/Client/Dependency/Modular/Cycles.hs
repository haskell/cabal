{-# LANGUAGE CPP #-}
module Distribution.Client.Dependency.Modular.Cycles (
    detectCyclesPhase
  ) where

import Prelude hiding (cycle)
import Control.Monad
import Control.Monad.Reader
import Data.Graph (SCC)
import Data.Map   (Map)
import qualified Data.Graph       as Gr
import qualified Data.Map         as Map
import qualified Data.Traversable as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.ConflictSet as CS

type DetectCycles = Reader (Map QPN QGoalReasonChain)

-- | Find and reject any solutions that are cyclic
detectCyclesPhase :: Tree QGoalReasonChain -> Tree QGoalReasonChain
detectCyclesPhase = (`runReader` Map.empty) .  cata go
  where
    -- Most cases are simple; we just need to remember which choices we made
    go :: TreeF QGoalReasonChain (DetectCycles (Tree QGoalReasonChain)) -> DetectCycles (Tree QGoalReasonChain)
    go (PChoiceF qpn gr     cs) = PChoice qpn gr     <$> local (Map.insert qpn gr) (T.sequence cs)
    go (FChoiceF qfn gr w m cs) = FChoice qfn gr w m <$> T.sequence cs
    go (SChoiceF qsn gr w   cs) = SChoice qsn gr w   <$> T.sequence cs
    go (GoalChoiceF         cs) = GoalChoice         <$> T.sequence cs
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
findCycles :: Map QPN QGoalReasonChain -> RevDepMap -> Maybe (ConflictSet QPN)
findCycles grs revDeps = do
    guard $ not (null cycles)
    return $ CS.unions $ map (\(qpn, gr) -> toConflictSet $ Goal (P qpn) gr) $ head cycles
  where
    cycles :: [[(QPN, QGoalReasonChain)]]
    cycles = [vs | Gr.CyclicSCC vs <- scc]

    scc :: [SCC (QPN, QGoalReasonChain)]
    scc = Gr.stronglyConnComp . map aux . Map.toList $ revDeps

    aux :: (QPN, [(comp, QPN)]) -> ((QPN, QGoalReasonChain), QPN, [QPN])
    aux (fr, to) = ((fr, grs Map.! fr), fr, map snd to)
