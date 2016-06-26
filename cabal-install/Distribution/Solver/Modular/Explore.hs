module Distribution.Solver.Modular.Explore
    ( backjump
    , backjumpAndExplore
    ) where

import Data.Foldable as F
import Data.Map as M

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import qualified Distribution.Solver.Modular.PSQ as P
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Settings (EnableBackjumping(..))
import qualified Distribution.Solver.Types.Progress as P

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
backjump :: F.Foldable t => EnableBackjumping -> Var QPN
         -> ConflictSet QPN -> t (ConflictSetLog a) -> ConflictSetLog a
backjump (EnableBackjumping enableBj) var initial xs =
    F.foldr combine logBackjump xs initial
  where
    combine :: ConflictSetLog a
            -> (ConflictSet QPN -> ConflictSetLog a)
            ->  ConflictSet QPN -> ConflictSetLog a
    combine (P.Done x)    _ _               = P.Done x
    combine (P.Fail cs)   f csAcc
      | enableBj && not (var `CS.member` cs) = logBackjump cs
      | otherwise                = f (csAcc `CS.union` cs)
    combine (P.Step m ms) f cs   = P.Step m (combine ms f cs)

    logBackjump :: ConflictSet QPN -> ConflictSetLog a
    logBackjump cs = failWith (Failure cs Backjump) cs

type ConflictSetLog = P.Progress Message (ConflictSet QPN)

-- | A tree traversal that simultaneously propagates conflict sets up
-- the tree from the leaves and creates a log.
exploreLog :: EnableBackjumping -> Tree QGoalReason
           -> (Assignment -> ConflictSetLog (Assignment, RevDepMap))
exploreLog enableBj = cata go
  where
    go :: TreeF QGoalReason (Assignment -> ConflictSetLog (Assignment, RevDepMap))
                         -> (Assignment -> ConflictSetLog (Assignment, RevDepMap))
    go (FailF c fr)          _           = failWith (Failure c fr) c
    go (DoneF rdm)           a           = succeedWith Success (a, rdm)
    go (PChoiceF qpn gr     ts) (A pa fa sa)   =
      backjump enableBj (P qpn) (avoidSet (P qpn) gr) $ -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ i@(POption k _) r -> tryWith (TryP qpn i) $ -- log and ...
                    r (A (M.insert qpn k pa) fa sa)) -- record the pkg choice
      ts
    go (FChoiceF qfn gr _ _ ts) (A pa fa sa)   =
      backjump enableBj (F qfn) (avoidSet (F qfn) gr) $ -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryF qfn k) $          -- log and ...
                    r (A pa (M.insert qfn k fa) sa)) -- record the pkg choice
      ts
    go (SChoiceF qsn gr _   ts) (A pa fa sa)   =
      backjump enableBj (S qsn) (avoidSet (S qsn) gr) $ -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryS qsn k) $          -- log and ...
                    r (A pa fa (M.insert qsn k sa))) -- record the pkg choice
      ts
    go (GoalChoiceF        ts) a           =
      P.casePSQ ts
        (failWith (Failure CS.empty EmptyGoalChoice) CS.empty) -- empty goal choice is an internal error
        (\ k v _xs -> continueWith (Next k) (v a))             -- commit to the first goal choice

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
                   -> Tree QGoalReason -> Log Message (Assignment, RevDepMap)
backjumpAndExplore enableBj t =
    toLog $ exploreLog enableBj t (A M.empty M.empty M.empty)
  where
    toLog :: P.Progress step fail done -> Log step done
    toLog = P.foldProgress P.Step (const (P.Fail ())) P.Done
