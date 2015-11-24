module Distribution.Solver.Modular.Explore
    ( backjump
    , exploreTreeLog
    ) where

import Control.Applicative as A
import Data.Foldable
import Data.List as L
import Data.Map as M
import Data.Set as S

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.Internal.PSQ as P
import Distribution.Solver.Modular.Tree

-- | Backjumping.
--
-- A tree traversal that tries to propagate conflict sets
-- up the tree from the leaves, and thereby cut branches.
-- All the tricky things are done in the function 'combine'.
backjump :: Tree a -> Tree (Maybe (ConflictSet QPN))
backjump = snd . cata go
  where
    go (FailF c fr) = (Just c, Fail c fr)
    go (DoneF rdm ) = (Nothing, Done rdm)
    go (PChoiceF qpn _     ts) = (c, PChoice qpn c     (P.fromList ts'))
      where
        ~(c, ts') = combine (P qpn) (P.toList ts) S.empty
    go (FChoiceF qfn _ b m ts) = (c, FChoice qfn c b m (P.fromList ts'))
      where
        ~(c, ts') = combine (F qfn) (P.toList ts) S.empty
    go (SChoiceF qsn _ b   ts) = (c, SChoice qsn c b   (P.fromList ts'))
      where
        ~(c, ts') = combine (S qsn) (P.toList ts) S.empty
    go (GoalChoiceF        ts) = (c, GoalChoice        (P.fromList ts'))
      where
        ~(cs, ts') = unzip $ L.map (\ (k, (x, v)) -> (x, (k, v))) $ P.toList ts
        c          = case cs of []    -> Nothing
                                d : _ -> d

-- | The 'combine' function is at the heart of backjumping. It takes
-- the variable we're currently considering, and a list of children
-- annotated with their respective conflict sets, and an accumulator
-- for the result conflict set. It returns a combined conflict set
-- for the parent node, and a (potentially shortened) list of children
-- with the annotations removed.
--
-- It is *essential* that we produce the results as early as possible.
-- In particular, we have to produce the list of children prior to
-- traversing the entire list -- otherwise we lose the desired behaviour
-- of being able to traverse the tree from left to right incrementally.
--
-- We can shorten the list of children if we find an individual conflict
-- set that does not contain the current variable. In this case, we can
-- just lift the conflict set to the current level, because the current
-- level cannot possibly have contributed to this conflict, so no other
-- choice at the current level would avoid the conflict.
--
-- If any of the children might contain a successful solution
-- (indicated by Nothing), then Nothing will be the combined
-- conflict set. If all children contain conflict sets, we can
-- take the union as the combined conflict set.
combine :: Var QPN -> [(a, (Maybe (ConflictSet QPN), b))] ->
           ConflictSet QPN -> (Maybe (ConflictSet QPN), [(a, b)])
combine _   []                      c = (Just c, [])
combine var ((k, (     d, v)) : xs) c = (\ ~(e, ys) -> (e, (k, v) : ys)) $
                                        case d of
                                          Just e | not (simplifyVar var `S.member` e) -> (Just e, [])
                                                 | otherwise                          -> combine var xs (e `S.union` c)
                                          Nothing                                     -> (Nothing, snd $ combine var xs S.empty)

-- | Version of 'explore' that returns a 'Log'.
exploreLog :: Tree (Maybe (ConflictSet QPN)) ->
              (Assignment -> Log Message (Assignment, RevDepMap))
exploreLog = cata go
  where
    go (FailF c fr)          _           = failWith (Failure c fr)
    go (DoneF rdm)           a           = succeedWith Success (a, rdm)
    go (PChoiceF qpn c     ts) (A pa fa sa)   =
      backjumpInfo c $
      asum $                                      -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ i@(POption k _) r -> tryWith (TryP qpn i) $     -- log and ...
                    r (A (M.insert qpn k pa) fa sa)) -- record the pkg choice
      ts
    go (FChoiceF qfn c _ _ ts) (A pa fa sa)   =
      backjumpInfo c $
      asum $                                      -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryF qfn k) $          -- log and ...
                    r (A pa (M.insert qfn k fa) sa)) -- record the pkg choice
      ts
    go (SChoiceF qsn c _   ts) (A pa fa sa)   =
      backjumpInfo c $
      asum $                                      -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryS qsn k) $          -- log and ...
                    r (A pa fa (M.insert qsn k sa))) -- record the pkg choice
      ts
    go (GoalChoiceF        ts) a           =
      P.casePSQ ts
        (failWith (Failure S.empty EmptyGoalChoice))   -- empty goal choice is an internal error
        (\ k v _xs -> continueWith (Next (close k)) (v a))     -- commit to the first goal choice

-- | Add in information about pruned trees.
--
-- TODO: This isn't quite optimal, because we do not merely report the shape of the
-- tree, but rather make assumptions about where that shape originated from. It'd be
-- better if the pruning itself would leave information that we could pick up at this
-- point.
backjumpInfo :: Maybe (ConflictSet QPN) -> Log Message a -> Log Message a
backjumpInfo c m = m <|> case c of -- important to produce 'm' before matching on 'c'!
                           Nothing -> A.empty
                           Just cs -> failWith (Failure cs Backjump)

-- | Interface.
exploreTreeLog :: Tree (Maybe (ConflictSet QPN)) -> Log Message (Assignment, RevDepMap)
exploreTreeLog t = exploreLog t (A M.empty M.empty M.empty)
