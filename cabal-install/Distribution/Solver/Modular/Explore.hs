module Distribution.Solver.Modular.Explore
    ( backjump
    , backjumpAndExplore
    ) where

import Control.Monad.State.Lazy
import Data.Foldable as F
import Data.List as L (foldl')
import Data.Map as M

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import qualified Distribution.Solver.Modular.PSQ as P
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Settings (EnableBackjumping(..), CountConflicts(..))
import qualified Distribution.Solver.Types.Progress as P

type Explore = State ConflictMap

-- | This function takes the variable we're currently considering, an
-- initial conflict set and a
-- list of children's logs. Each log yields either a solution or a
-- conflict set. The result is a combined log for the parent node that
-- has explored a prefix of the children.
--
-- We can stop traversing the children's logs if we find an individual
-- conflict set that does not contain the current variable. In this
-- case, we can just lift the conflict set to the current level,
-- because the current level cannot possibly have contributed to this
-- conflict, so no other choice at the current level would avoid the
-- conflict.
--
-- If any of the children might contain a successful solution, we can
-- return it immediately. If all children contain conflict sets, we can
-- take the union as the combined conflict set.
--
-- The initial conflict set corresponds to the justification that we
-- have to choose this goal at all. There is a reason why we have
-- introduced the goal in the first place, and this reason is in conflict
-- with the (virtual) option not to choose anything for the current
-- variable. See also the comments for 'avoidSet'.
--
backjump :: EnableBackjumping -> Var QPN
         -> ConflictSet QPN -> P.PSQ k (Explore (ConflictSetLog a))
         -> Explore (ConflictSetLog a)
backjump (EnableBackjumping enableBj) var initial xs =
    F.foldr combine logBackjump xs initial
  where
    combine ::  Explore (ConflictSetLog a)
            -> (ConflictSet QPN -> Explore (ConflictSetLog a))
            ->  ConflictSet QPN -> Explore (ConflictSetLog a)
    combine x f csAcc = do
      l <- x
      case l of
        P.Done d  -> return (P.Done d)
        P.Fail cs
          | enableBj && not (var `CS.member` cs) -> logBackjump cs
          | otherwise                            -> f (csAcc `CS.union` cs)
        P.Step m ms -> do
          l' <- combine (return ms) f csAcc
          return (P.Step m l')

    logBackjump :: ConflictSet QPN -> Explore (ConflictSetLog a)
    logBackjump cs = return (failWith (Failure cs Backjump) cs)

type ConflictSetLog = P.Progress Message (ConflictSet QPN)

type ConflictMap = Map (Var QPN) Int

getBestGoal :: ConflictMap -> P.PSQ (Goal QPN) a -> (Goal QPN, a)
getBestGoal cm =
  P.maximumBy
    ( flip (M.findWithDefault 0) cm
    . (\ (Goal v _) -> v)
    )

getFirstGoal :: P.PSQ (Goal QPN) a -> (Goal QPN, a)
getFirstGoal ts =
  P.casePSQ ts
    (error "getFirstGoal: empty goal choice") -- empty goal choice is an internal error
    (\ k v _xs -> (k, v))  -- commit to the first goal choice

updateCM :: ConflictSet QPN -> ConflictMap -> ConflictMap
updateCM cs cm =
  L.foldl' (\ cmc k -> M.alter inc k cmc) cm (CS.toList cs)
  where
    inc Nothing  = Just 1
    inc (Just n) = Just $! n + 1

-- | A tree traversal that simultaneously propagates conflict sets up
-- the tree from the leaves and creates a log.
exploreLog :: EnableBackjumping -> CountConflicts -> Tree QGoalReason
           -> (Assignment -> Explore (ConflictSetLog (Assignment, RevDepMap)))
exploreLog enableBj (CountConflicts countConflicts) = cata go
  where
    updateCM' :: ConflictSet QPN -> Explore a -> Explore a
    updateCM'
      | countConflicts = \ c k -> modify' (updateCM c) >> k
      | otherwise      = \ _ k -> k

    getBestGoal' :: P.PSQ (Goal QPN) a -> Explore (Goal QPN, a)
    getBestGoal'
      | countConflicts = \ ts -> get >>= \ cm -> return (getBestGoal cm ts)
      | otherwise      = return . getFirstGoal

    go :: TreeF QGoalReason (Assignment -> Explore (ConflictSetLog (Assignment, RevDepMap)))
                         -> (Assignment -> Explore (ConflictSetLog (Assignment, RevDepMap)))
    go (FailF c fr)             _            = updateCM' c (return (failWith (Failure c fr) c))
    go (DoneF rdm)              a            = return (succeedWith Success (a, rdm))
    go (PChoiceF qpn gr     ts) (A pa fa sa) =
      backjump enableBj (P qpn) (avoidSet (P qpn) gr) $ -- try children in order,
        P.mapWithKey                                -- when descending ...
          (\ i@(POption k _) r -> do
            l <- r (A (M.insert qpn k pa) fa sa)
            return (tryWith (TryP qpn i) l)
          )
        ts
    go (FChoiceF qfn gr _ _ ts) (A pa fa sa) =
      backjump enableBj (F qfn) (avoidSet (F qfn) gr) $ -- try children in order,
        P.mapWithKey                                -- when descending ...
          (\ k r -> do
            l <- r (A pa (M.insert qfn k fa) sa)
            return (tryWith (TryF qfn k) l)
          )
        ts
    go (SChoiceF qsn gr _   ts) (A pa fa sa) =
      backjump enableBj (S qsn) (avoidSet (S qsn) gr) $ -- try children in order,
        P.mapWithKey                                -- when descending ...
          (\ k r -> do
            l <- r (A pa fa (M.insert qsn k sa))
            return (tryWith (TryS qsn k) l)
          )
        ts
    go (GoalChoiceF         ts) a            = do
      (k, v) <- getBestGoal' ts
      l      <- v a
      return (continueWith (Next k) l)

-- | Build a conflict set corresponding to the (virtual) option not to
-- choose a solution for a goal at all.
--
-- In the solver, the set of goals is not statically determined, but depends
-- on the choices we make. Therefore, when dealing with conflict sets, we
-- always have to consider that we could perhaps make choices that would
-- avoid the existence of the goal completely.
--
-- Whenever we actual introduce a choice in the tree, we have already established
-- that the goal cannot be avoided. This is tracked in the "goal reason".
-- The choice to avoid the goal therefore is a conflict between the goal itself
-- and its goal reason. We build this set here, and pass it to the 'backjump'
-- function as the initial conflict set.
--
-- This has two effects:
--
-- - In a situation where there are no choices available at all (this happens
-- if an unknown package is requested), the initial conflict set becomes the
-- actual conflict set.
--
-- - In a situation where we backjump past the current node, the goal reason
-- of the current node will be added to the conflict set.
--
avoidSet :: Var QPN -> QGoalReason -> ConflictSet QPN
avoidSet var gr =
  CS.fromList (var : goalReasonToVars gr)

-- | Interface.
backjumpAndExplore :: EnableBackjumping
                   -> CountConflicts
                   -> Tree QGoalReason -> Log Message (Assignment, RevDepMap)
backjumpAndExplore enableBj countConflicts t =
    toLog $ fst $ runState (exploreLog enableBj countConflicts t (A M.empty M.empty M.empty)) M.empty
  where
    toLog :: P.Progress step fail done -> Log step done
    toLog = P.foldProgress P.Step (const (P.Fail ())) P.Done
