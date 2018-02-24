{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Solver.Modular.Explore
    ( backjump
    , backjumpAndExplore
    ) where

import qualified Distribution.Solver.Types.Progress as P

import Data.Foldable as F
import Data.List as L (foldl')
import Distribution.Compat.Map.Strict as M

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import qualified Distribution.Solver.Modular.PSQ as P
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.RetryLog
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.WeightedPSQ as W
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Settings (EnableBackjumping(..), CountConflicts(..))

-- | This function takes the variable we're currently considering, a
-- last conflict set and a list of children's logs. Each log yields
-- either a solution or a conflict set. The result is a combined log for
-- the parent node that has explored a prefix of the children.
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
-- The last conflict set corresponds to the justification that we
-- have to choose this goal at all. There is a reason why we have
-- introduced the goal in the first place, and this reason is in conflict
-- with the (virtual) option not to choose anything for the current
-- variable. See also the comments for 'avoidSet'.
--
backjump :: Maybe Int -> EnableBackjumping -> Var QPN
         -> ConflictSet -> W.WeightedPSQ w k (ExploreState -> ConflictSetLog a)
         -> ExploreState -> ConflictSetLog a
backjump mbj (EnableBackjumping enableBj) var lastCS xs =
    F.foldr combine avoidGoal xs CS.empty
  where
    combine :: forall a . (ExploreState -> ConflictSetLog a)
            -> (ConflictSet -> ExploreState -> ConflictSetLog a)
            ->  ConflictSet -> ExploreState -> ConflictSetLog a
    combine x f csAcc es = retry (x es) next
      where
        next :: IntermediateFailure -> ConflictSetLog a
        next BackjumpLimit = fromProgress (P.Fail BackjumpLimit)
        next (NoSolution !cs es')
          | enableBj && not (var `CS.member` cs) = skipLoggingBackjump cs es'
          | otherwise                            = f (csAcc `CS.union` cs) es'

    -- This function represents the option to not choose a value for this goal.
    avoidGoal :: ConflictSet -> ExploreState -> ConflictSetLog a
    avoidGoal cs !es =
        logBackjump (cs `CS.union` lastCS) $

        -- Use 'lastCS' below instead of 'cs' since we do not want to
        -- double-count the additionally accumulated conflicts.
        es { esConflictMap = updateCM lastCS (esConflictMap es) }

    logBackjump :: ConflictSet -> ExploreState -> ConflictSetLog a
    logBackjump cs es =
        failWith (Failure cs Backjump) $
            if reachedBjLimit (esBackjumps es)
            then BackjumpLimit
            else NoSolution cs es { esBackjumps = esBackjumps es + 1 }
      where
        reachedBjLimit = case mbj of
                           Nothing    -> const False
                           Just limit -> (== limit)

    -- The solver does not count or log backjumps at levels where the conflict
    -- set does not contain the current variable. Otherwise, there would be many
    -- consecutive log messages about backjumping with the same conflict set.
    skipLoggingBackjump :: ConflictSet -> ExploreState -> ConflictSetLog a
    skipLoggingBackjump cs es = fromProgress $ P.Fail (NoSolution cs es)

-- | The state that is read and written while exploring the search tree.
data ExploreState = ES {
    esConflictMap :: !ConflictMap
  , esBackjumps   :: !Int
  }

data IntermediateFailure =
    NoSolution ConflictSet ExploreState
  | BackjumpLimit

type ConflictSetLog = RetryLog Message IntermediateFailure

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

updateCM :: ConflictSet -> ConflictMap -> ConflictMap
updateCM cs cm =
  L.foldl' (\ cmc k -> M.insertWith (+) k 1 cmc) cm (CS.toList cs)

-- | Record complete assignments on 'Done' nodes.
assign :: Tree d c -> Tree Assignment c
assign tree = cata go tree $ A M.empty M.empty M.empty
  where
    go :: TreeF d c (Assignment -> Tree Assignment c)
                 -> (Assignment -> Tree Assignment c)
    go (FailF c fr)            _                  = Fail c fr
    go (DoneF rdm _)           a                  = Done rdm a
    go (PChoiceF qpn rdm y       ts) (A pa fa sa) = PChoice qpn rdm y       $ W.mapWithKey f ts
        where f (POption k _) r = r (A (M.insert qpn k pa) fa sa)
    go (FChoiceF qfn rdm y t m d ts) (A pa fa sa) = FChoice qfn rdm y t m d $ W.mapWithKey f ts
        where f k             r = r (A pa (M.insert qfn k fa) sa)
    go (SChoiceF qsn rdm y t     ts) (A pa fa sa) = SChoice qsn rdm y t     $ W.mapWithKey f ts
        where f k             r = r (A pa fa (M.insert qsn k sa))
    go (GoalChoiceF  rdm         ts) a            = GoalChoice  rdm         $ fmap ($ a) ts

-- | A tree traversal that simultaneously propagates conflict sets up
-- the tree from the leaves and creates a log.
exploreLog :: Maybe Int -> EnableBackjumping -> CountConflicts
           -> Tree Assignment QGoalReason
           -> ConflictSetLog (Assignment, RevDepMap)
exploreLog mbj enableBj (CountConflicts countConflicts) t = cata go t initES
  where
    getBestGoal' :: P.PSQ (Goal QPN) a -> ConflictMap -> (Goal QPN, a)
    getBestGoal'
      | countConflicts = \ ts cm -> getBestGoal cm ts
      | otherwise      = \ ts _  -> getFirstGoal ts

    go :: TreeF Assignment QGoalReason (ExploreState -> ConflictSetLog (Assignment, RevDepMap))
                                    -> (ExploreState -> ConflictSetLog (Assignment, RevDepMap))
    go (FailF c fr)                            = \ !es ->
        let es' = es { esConflictMap = updateCM c (esConflictMap es) }
        in failWith (Failure c fr) (NoSolution c es')
    go (DoneF rdm a)                           = \ _   -> succeedWith Success (a, rdm)
    go (PChoiceF qpn _ gr       ts)            =
      backjump mbj enableBj (P qpn) (avoidSet (P qpn) gr) $ -- try children in order,
        W.mapWithKey                                        -- when descending ...
          (\ k r es -> tryWith (TryP qpn k) (r es))
          ts
    go (FChoiceF qfn _ gr _ _ _ ts)            =
      backjump mbj enableBj (F qfn) (avoidSet (F qfn) gr) $ -- try children in order,
        W.mapWithKey                                        -- when descending ...
          (\ k r es -> tryWith (TryF qfn k) (r es))
          ts
    go (SChoiceF qsn _ gr _     ts)            =
      backjump mbj enableBj (S qsn) (avoidSet (S qsn) gr) $ -- try children in order,
        W.mapWithKey                                        -- when descending ...
          (\ k r es -> tryWith (TryS qsn k) (r es))
          ts
    go (GoalChoiceF _           ts)            = \ es ->
      let (k, v) = getBestGoal' ts (esConflictMap es)
      in continueWith (Next k) (v es)

    initES = ES {
        esConflictMap = M.empty
      , esBackjumps = 0
      }

-- | Build a conflict set corresponding to the (virtual) option not to
-- choose a solution for a goal at all.
--
-- In the solver, the set of goals is not statically determined, but depends
-- on the choices we make. Therefore, when dealing with conflict sets, we
-- always have to consider that we could perhaps make choices that would
-- avoid the existence of the goal completely.
--
-- Whenever we actually introduce a choice in the tree, we have already established
-- that the goal cannot be avoided. This is tracked in the "goal reason".
-- The choice to avoid the goal therefore is a conflict between the goal itself
-- and its goal reason. We build this set here, and pass it to the 'backjump'
-- function as the last conflict set.
--
-- This has two effects:
--
-- - In a situation where there are no choices available at all (this happens
-- if an unknown package is requested), the last conflict set becomes the
-- actual conflict set.
--
-- - In a situation where all of the children's conflict sets contain the
-- current variable, the goal reason of the current node will be added to the
-- conflict set.
--
avoidSet :: Var QPN -> QGoalReason -> ConflictSet
avoidSet var gr =
  CS.union (CS.singleton var) (goalReasonToCS gr)

-- | Interface.
--
-- Takes as an argument a limit on allowed backjumps. If the limit is 'Nothing',
-- then infinitely many backjumps are allowed. If the limit is 'Just 0',
-- backtracking is completely disabled.
backjumpAndExplore :: Maybe Int
                   -> EnableBackjumping
                   -> CountConflicts
                   -> Tree d QGoalReason -> Log Message (Assignment, RevDepMap)
backjumpAndExplore mbj enableBj countConflicts =
    toProgress . mapFailure convertFailure . exploreLog mbj enableBj countConflicts . assign
  where
    convertFailure (NoSolution cs es) = ExhaustiveSearch cs (esConflictMap es)
    convertFailure BackjumpLimit      = BackjumpLimitReached
