{-# LANGUAGE CPP #-}
module Distribution.Client.Dependency.Modular.Linking (
    addLinking
  ) where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map         as M
import qualified Data.Traversable as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.PSQ as P

{-------------------------------------------------------------------------------
  Add linking
-------------------------------------------------------------------------------}

type RelatedGoals = Map (PN, I) [PP]
type Linker       = Reader RelatedGoals

addLinking :: Tree QGoalReasonChain -> Tree QGoalReasonChain
addLinking = (`runReader` M.empty) .  cata go
  where
    go :: TreeF QGoalReasonChain (Linker (Tree QGoalReasonChain)) -> Linker (Tree QGoalReasonChain)

    -- The only nodes of interest are package nodes
    go (PChoiceF qpn gr cs) = do
      env <- ask
      cs' <- T.sequence $ P.mapWithKey (goP qpn) cs
      let newCs = concatMap (linkChoices env qpn) (P.toList cs')
      return $ PChoice qpn gr (cs' `P.union` P.fromList newCs)

    -- For all other nodes we just recurse
    go (FChoiceF qfn gr t m cs)       = FChoice qfn gr t m  <$> T.sequence cs
    go (SChoiceF qsn gr t   cs)       = SChoice qsn gr t    <$> T.sequence cs
    go (GoalChoiceF         cs)       = GoalChoice          <$> T.sequence cs
    go (DoneF revDepMap)              = return $ Done revDepMap
    go (FailF conflictSet failReason) = return $ Fail conflictSet failReason

    -- Recurse underneath package choices. Here we just need to make sure
    -- that we record the package choice so that it is available below
    goP :: QPN -> POption -> Linker (Tree QGoalReasonChain) -> Linker (Tree QGoalReasonChain)
    goP (Q pp pn) (POption i Nothing) = local (M.insertWith (++) (pn, i) [pp])
    goP _ _ = alreadyLinked

linkChoices :: RelatedGoals -> QPN -> (POption, Tree QGoalReasonChain) -> [(POption, Tree QGoalReasonChain)]
linkChoices related (Q _pp pn) (POption i Nothing, subtree) =
    map aux (M.findWithDefault [] (pn, i) related)
  where
    aux :: PP -> (POption, Tree QGoalReasonChain)
    aux pp = (POption i (Just pp), subtree)
linkChoices _ _ (POption _ (Just _), _) =
    alreadyLinked

alreadyLinked :: a
alreadyLinked = error "addLinking called on tree that already contains linked nodes"
