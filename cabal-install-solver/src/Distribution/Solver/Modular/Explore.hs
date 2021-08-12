{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Solver.Modular.Explore (backjumpAndExplore) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Distribution.Solver.Types.Progress as P

import qualified Data.List as L (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Distribution.Simple.Setup (asBool)

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.PSQ as P
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.RetryLog
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Version
import qualified Distribution.Solver.Modular.WeightedPSQ as W
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Settings
         (CountConflicts(..), EnableBackjumping(..), FineGrainedConflicts(..))
import Distribution.Types.VersionRange (anyVersion)

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
-- We can also skip a child if it does not resolve any of the conflicts paired
-- with the current variable in the previous child's conflict set. 'backjump'
-- takes a function to determine whether a child can be skipped. If the child
-- can be skipped, the function returns a new conflict set to be merged with the
-- previous conflict set.
--
backjump :: forall w k a . Maybe Int
         -> EnableBackjumping
         -> FineGrainedConflicts

         -> (k -> S.Set CS.Conflict -> Maybe ConflictSet)
            -- ^ Function that determines whether the given choice could resolve
            --   the given conflict. It indicates false by returning 'Just',
            --   with the new conflicts to be added to the conflict set.

         -> (k -> ConflictSet -> ExploreState -> ConflictSetLog a)
            -- ^ Function that logs the given choice that was skipped.

         -> Var QPN -- ^ The current variable.

         -> ConflictSet -- ^ Conflict set representing the reason that the goal
                        --   was introduced.

         -> W.WeightedPSQ w k (ExploreState -> ConflictSetLog a)
            -- ^ List of children's logs.

         -> ExploreState -> ConflictSetLog a
backjump mbj enableBj fineGrainedConflicts couldResolveConflicts
         logSkippedChoice var lastCS xs =
    foldr combine avoidGoal [(k, v) | (_, k, v) <- W.toList xs] CS.empty Nothing
  where
    combine :: (k, ExploreState -> ConflictSetLog a)
            -> (ConflictSet -> Maybe ConflictSet -> ExploreState -> ConflictSetLog a)
            ->  ConflictSet -> Maybe ConflictSet -> ExploreState -> ConflictSetLog a
    combine (k, x) f csAcc mPreviousCS es =
        case (asBool fineGrainedConflicts, mPreviousCS) of
          (True, Just previousCS) ->
              case CS.lookup var previousCS of
                Just conflicts ->
                  case couldResolveConflicts k conflicts of
                    Nothing           -> retryNoSolution (x es) next
                    Just newConflicts -> skipChoice (previousCS `CS.union` newConflicts)
                _              -> skipChoice previousCS
          _                       -> retryNoSolution (x es) next
      where
        next :: ConflictSet -> ExploreState -> ConflictSetLog a
        next !cs es' = if asBool enableBj && not (var `CS.member` cs)
                       then skipLoggingBackjump cs es'
                       else f (csAcc `CS.union` cs) (Just cs) es'

        -- This function is for skipping the choice when it cannot resolve any
        -- of the previous conflicts.
        skipChoice :: ConflictSet -> ConflictSetLog a
        skipChoice newCS =
            retryNoSolution (logSkippedChoice k newCS es) $ \cs' es' ->
                f (csAcc `CS.union` cs') (Just cs') $

                -- Update the conflict map with the conflict set, to make up for
                -- skipping the whole subtree.
                es' { esConflictMap = updateCM cs' (esConflictMap es') }

    -- This function represents the option to not choose a value for this goal.
    avoidGoal :: ConflictSet -> Maybe ConflictSet -> ExploreState -> ConflictSetLog a
    avoidGoal cs _mPreviousCS !es =
        logBackjump mbj (cs `CS.union` lastCS) $

        -- Use 'lastCS' below instead of 'cs' since we do not want to
        -- double-count the additionally accumulated conflicts.
        es { esConflictMap = updateCM lastCS (esConflictMap es) }

    -- The solver does not count or log backjumps at levels where the conflict
    -- set does not contain the current variable. Otherwise, there would be many
    -- consecutive log messages about backjumping with the same conflict set.
    skipLoggingBackjump :: ConflictSet -> ExploreState -> ConflictSetLog a
    skipLoggingBackjump cs es = fromProgress $ P.Fail (NoSolution cs es)

-- | Creates a failing ConflictSetLog representing a backjump. It inserts a
-- "backjumping" message, checks whether the backjump limit has been reached,
-- and increments the backjump count.
logBackjump :: Maybe Int -> ConflictSet -> ExploreState -> ConflictSetLog a
logBackjump mbj cs es =
    failWith (Failure cs Backjump) $
        if reachedBjLimit (esBackjumps es)
        then BackjumpLimit
        else NoSolution cs es { esBackjumps = esBackjumps es + 1 }
  where
    reachedBjLimit = case mbj of
                       Nothing    -> const False
                       Just limit -> (>= limit)

-- | Like 'retry', except that it only applies the input function when the
-- backjump limit has not been reached.
retryNoSolution :: ConflictSetLog a
                -> (ConflictSet -> ExploreState -> ConflictSetLog a)
                -> ConflictSetLog a
retryNoSolution lg f = retry lg $ \case
    BackjumpLimit    -> fromProgress (P.Fail BackjumpLimit)
    NoSolution cs es -> f cs es

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
assign tree = go tree (A M.empty M.empty M.empty)
  where
    go :: Tree d c -> Assignment -> Tree Assignment c
    go (Fail c fr)            _                  = Fail c fr
    go (Done rdm _)           a                  = Done rdm a
    go (PChoice qpn rdm y       ts) (A pa fa sa) = PChoice qpn rdm y       $ W.mapWithKey f (fmap go ts)
        where f (POption k _) r = r (A (M.insert qpn k pa) fa sa)
    go (FChoice qfn rdm y t m d ts) (A pa fa sa) = FChoice qfn rdm y t m d $ W.mapWithKey f (fmap go ts)
        where f k             r = r (A pa (M.insert qfn k fa) sa)
    go (SChoice qsn rdm y t     ts) (A pa fa sa) = SChoice qsn rdm y t     $ W.mapWithKey f (fmap go ts)
        where f k             r = r (A pa fa (M.insert qsn k sa))
    go (GoalChoice  rdm         ts) a            = GoalChoice  rdm         $ fmap ($ a) (fmap go ts)

-- | A tree traversal that simultaneously propagates conflict sets up
-- the tree from the leaves and creates a log.
exploreLog :: Maybe Int
           -> EnableBackjumping
           -> FineGrainedConflicts
           -> CountConflicts
           -> Index
           -> Tree Assignment QGoalReason
           -> ConflictSetLog (Assignment, RevDepMap)
exploreLog mbj enableBj fineGrainedConflicts (CountConflicts countConflicts) idx t =
    para go t initES
  where
    getBestGoal' :: P.PSQ (Goal QPN) a -> ConflictMap -> (Goal QPN, a)
    getBestGoal'
      | asBool countConflicts = \ ts cm -> getBestGoal cm ts
      | otherwise             = \ ts _  -> getFirstGoal ts

    go :: TreeF Assignment QGoalReason
                (ExploreState -> ConflictSetLog (Assignment, RevDepMap), Tree Assignment QGoalReason)
                                    -> (ExploreState -> ConflictSetLog (Assignment, RevDepMap))
    go (FailF c fr)                            = \ !es ->
        let es' = es { esConflictMap = updateCM c (esConflictMap es) }
        in failWith (Failure c fr) (NoSolution c es')
    go (DoneF rdm a)                           = \ _   -> succeedWith Success (a, rdm)
    go (PChoiceF qpn _ gr       ts)            =
      backjump mbj enableBj fineGrainedConflicts
               (couldResolveConflicts qpn)
               (logSkippedPackage qpn)
               (P qpn) (avoidSet (P qpn) gr) $ -- try children in order,
               W.mapWithKey                    -- when descending ...
                 (\ k r es -> tryWith (TryP qpn k) (r es))
                 (fmap fst ts)
    go (FChoiceF qfn _ gr _ _ _ ts)            =
      backjump mbj enableBj fineGrainedConflicts
               (\_ _ -> Nothing)
               (const logSkippedChoiceSimple)
               (F qfn) (avoidSet (F qfn) gr) $ -- try children in order,
               W.mapWithKey                    -- when descending ...
                 (\ k r es -> tryWith (TryF qfn k) (r es))
                 (fmap fst ts)
    go (SChoiceF qsn _ gr _     ts)            =
      backjump mbj enableBj fineGrainedConflicts
               (\_ _ -> Nothing)
               (const logSkippedChoiceSimple)
               (S qsn) (avoidSet (S qsn) gr) $ -- try children in order,
               W.mapWithKey                    -- when descending ...
                 (\ k r es -> tryWith (TryS qsn k) (r es))
                 (fmap fst ts)
    go (GoalChoiceF _           ts)            = \ es ->
      let (k, (v, tree)) = getBestGoal' ts (esConflictMap es)
      in continueWith (Next k) $
         -- Goal choice nodes are normally not counted as backjumps, since the
         -- solver always explores exactly one choice, which means that the
         -- backjump from the goal choice would be redundant with the backjump
         -- from the PChoice, FChoice, or SChoice below. The one case where the
         -- backjump is not redundant is when the chosen goal is a failure node,
         -- so we log a backjump in that case.
         case tree of
           Fail _ _ -> retryNoSolution (v es) $ logBackjump mbj
           _        -> v es

    initES = ES {
        esConflictMap = M.empty
      , esBackjumps = 0
      }

    -- Is it possible for this package instance (QPN and POption) to resolve any
    -- of the conflicts that were caused by the previous instance? The default
    -- is true, because it is always safe to explore a package instance.
    -- Skipping it is an optimization. If false, it returns a new conflict set
    -- to be merged with the previous one.
    couldResolveConflicts :: QPN -> POption -> S.Set CS.Conflict -> Maybe ConflictSet
    couldResolveConflicts currentQPN@(Q _ pn) (POption i@(I v _) _) conflicts =
      let (PInfo deps _ _ _) = idx M.! pn M.! i
          qdeps = qualifyDeps (defaultQualifyOptions idx) currentQPN deps

          couldBeResolved :: CS.Conflict -> Maybe ConflictSet
          couldBeResolved CS.OtherConflict = Nothing
          couldBeResolved (CS.GoalConflict conflictingDep) =
              -- Check whether this package instance also has 'conflictingDep'
              -- as a dependency (ignoring flag and stanza choices).
              if null [() | Simple (LDep _ (Dep (PkgComponent qpn _) _)) _ <- qdeps, qpn == conflictingDep]
              then Nothing
              else Just CS.empty
          couldBeResolved (CS.VersionConstraintConflict dep excludedVersion) =
              -- Check whether this package instance also excludes version
              -- 'excludedVersion' of 'dep' (ignoring flag and stanza choices).
              let vrs = [vr | Simple (LDep _ (Dep (PkgComponent qpn _) (Constrained vr))) _ <- qdeps, qpn == dep ]
                  vrIntersection = L.foldl' (.&&.) anyVersion vrs
              in if checkVR vrIntersection excludedVersion
                 then Nothing
                 else -- If we skip this package instance, we need to update the
                      -- conflict set to say that 'dep' was also excluded by
                      -- this package instance's constraint.
                      Just $ CS.singletonWithConflict (P dep) $
                      CS.VersionConflict currentQPN (CS.OrderedVersionRange vrIntersection)
          couldBeResolved (CS.VersionConflict reverseDep (CS.OrderedVersionRange excludingVR)) =
              -- Check whether this package instance's version is also excluded
              -- by 'excludingVR'.
              if checkVR excludingVR v
              then Nothing
              else -- If we skip this version, we need to update the conflict
                   -- set to say that the reverse dependency also excluded this
                   -- version.
                   Just $ CS.singletonWithConflict (P reverseDep) (CS.VersionConstraintConflict currentQPN v)
      in fmap CS.unions $ traverse couldBeResolved (S.toList conflicts)

    logSkippedPackage :: QPN -> POption -> ConflictSet -> ExploreState -> ConflictSetLog a
    logSkippedPackage qpn pOption cs es =
        tryWith (TryP qpn pOption) $
        failWith (Skip (fromMaybe S.empty $ CS.lookup (P qpn) cs)) $
        NoSolution cs es

    -- This function is used for flag and stanza choices, but it should not be
    -- called, because there is currently no way to skip a value for a flag or
    -- stanza.
    logSkippedChoiceSimple :: ConflictSet -> ExploreState -> ConflictSetLog a
    logSkippedChoiceSimple cs es = fromProgress $ P.Fail $ NoSolution cs es

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
avoidSet var@(P qpn) gr =
  CS.union (CS.singleton var) (goalReasonToConflictSetWithConflict qpn gr)
avoidSet var         gr =
  CS.union (CS.singleton var) (goalReasonToConflictSet gr)

-- | Interface.
--
-- Takes as an argument a limit on allowed backjumps. If the limit is 'Nothing',
-- then infinitely many backjumps are allowed. If the limit is 'Just 0',
-- backtracking is completely disabled.
backjumpAndExplore :: Maybe Int
                   -> EnableBackjumping
                   -> FineGrainedConflicts
                   -> CountConflicts
                   -> Index
                   -> Tree d QGoalReason
                   -> RetryLog Message SolverFailure (Assignment, RevDepMap)
backjumpAndExplore mbj enableBj fineGrainedConflicts countConflicts idx =
    mapFailure convertFailure
  . exploreLog mbj enableBj fineGrainedConflicts countConflicts idx
  . assign
  where
    convertFailure (NoSolution cs es) = ExhaustiveSearch cs (esConflictMap es)
    convertFailure BackjumpLimit      = BackjumpLimitReached
