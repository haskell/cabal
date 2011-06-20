module Distribution.Client.Dependency.Modular.Validate where

-- Validation of the tree.
--
-- The task here is to make sure all constraints hold. After validation, any
-- assignment returned by exploration of the tree should be a complete valid
-- assignment, i.e., actually constitute a solution.

import Control.Applicative
import Control.Monad.Reader hiding (sequence)
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Traversable
import Prelude hiding (sequence)

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree

-- In practice, most constraints are implication constraints (IF we have made
-- a number of choices, THEN we also have to ensure that). We call constraints
-- that for which the precondiditions are fulfilled ACTIVE. We maintain a set
-- of currently active constraints that we pass down the node.
--
-- We aim at detecting inconsistent states as early as possible.
--
-- Whenever we make a choice, there are two things that need to happen:
--
--   (1) We must check that the choice is consistent with the currently
--       active constraints.
--
--   (2) The choice increases the set of active constraints. For the new
--       active constraints, we must check that they are consistent with
--       the current state.
--
-- We can actually merge (1) and (2) by saying the the current choice is
-- a new active constraint, fixing the choice.
--
-- If a test fails, we have detected an inconsistent state. We can
-- disable the current subtree and do not have to traverse it any further.
--
-- We need a good way to represent the current state, i.e., the current
-- set of active constraints. Since the main situation where we have to
-- search in it is (1), it seems best to store the state by package: for
-- every package, we store which versions are still allowed. If for any
-- package, we have inconsistent active constraints, we can also stop.
-- This is a particular way to read task (2):
--
--   (2, weak) We only check if the new constraints are consistent with
--       the choices we've already made, and add them to the active set.
--
--   (2, strong) We check if the new constraints are consistent with the
--       choices we've already made, and the constraints we already have.
--
-- It currently seems as if we're implementing the weak variant. However,
-- when used together with 'preferEasyGoalChoices', we will find an
-- inconsistent state in the very next step.
--
-- What do we do about flags?
--
-- Like for packages, we store the flag choices we have already made.
-- Now, regarding (1), we only have to test whether we've decided the
-- current flag before. Regarding (2), the interesting bit is in discovering
-- the new active constraints. To this end, we look up the constraints for
-- the package the flag belongs to, and traverse its flagged dependencies.
-- Wherever we find the flag in question, we start recording dependencies
-- underneath as new active dependencies. If we encounter other flags, we
-- check if we've chosen them already and either proceed or stop.

-- | The state needed during validation.
data ValidateState = VS {
  index :: Index,
  saved :: Map QPN (FlaggedDeps QPN), -- saved, scoped, dependencies
  pa    :: PreAssignment
}

type Validate = Reader ValidateState

validate :: Tree (GoalReason, Scope) -> Validate (Tree GoalReason)
validate = cata go
  where
    go :: TreeF (GoalReason, Scope) (Validate (Tree GoalReason)) -> Validate (Tree GoalReason)

    go (PChoiceF qpn (gr,  sc)   ts) = PChoice qpn gr   <$> sequence (P.mapWithKey (goP qpn sc) ts)
    go (FChoiceF qfn (gr, _sc) b ts) = FChoice qfn gr b <$> sequence (P.mapWithKey (goF qfn gr) ts)

    -- We don't need to do anything for goal choices or failure nodes.
    go (GoalChoiceF              ts) = GoalChoice       <$> sequence ts
    go (DoneF    rdm               ) = pure (Done rdm)
    go (FailF    c fr              ) = pure (Fail c fr)

    -- What to do for package nodes ...
    goP :: QPN -> Scope -> I -> Validate (Tree GoalReason) -> Validate (Tree GoalReason)
    goP qpn@(Q _pp pn) sc i r = do
      PA ppa pfa <- asks pa    -- obtain current preassignment
      idx        <- asks index -- obtain the index
      svd        <- asks saved -- obtain saved dependencies
      let (PInfo deps _ _) = idx ! pn ! i -- obtain dependencies introduced by the choice
      let qdeps = L.map (fmap (qualify sc)) deps -- qualify the deps in the current scope
      -- the new active constraints are given by the instance we have chosen,
      -- plus the dependency information we have for that instance
      let newactives = Dep qpn (Fixed i qpn) : extractDeps pfa qdeps
      -- We now try to extend the partial assignment with the new active constraints.
      let mnppa = extend (P qpn) ppa newactives
      -- In case we continue, we save the scoped dependencies
      let nsvd = M.insert qpn qdeps svd
      case mnppa of
        Left (c, d) -> -- We have an inconsistency. We can stop.
                       return (Fail c (Conflicting d))
        Right nppa  -> -- We have an updated partial assignment for the recursive validation.
                       local (\ s -> s { pa = PA nppa pfa, saved = nsvd }) r

    -- What to do for flag nodes ...
    goF :: QFN -> GoalReason -> Bool -> Validate (Tree GoalReason) -> Validate (Tree GoalReason)
    goF qfn@(FN (PI qpn _i) _f) gr b r = do
      PA ppa pfa <- asks pa -- obtain current preassignment
      svd <- asks saved     -- obtain saved dependencies
      -- Note that there should be saved dependencies for the package in question,
      -- because while building, we do not choose flags before we see the packages
      -- that define them.
      let qdeps = svd ! qpn
      -- We take the *saved* dependencies, because these have been qualified in the
      -- correct scope.
      --
      -- First, we should check that our flag choice itself is consistent. Unlike for
      -- package nodes, we do not guarantee that a flag choice occurs exactly once.
      case M.lookup qfn pfa of
        Just rb | rb /= b -> return (Fail (F qfn `S.insert` goalReasonToVars gr) ConflictingFlag)
        _                 -> do
          -- Extend the flag assignment
          let npfa = M.insert qfn b pfa
          -- We now try to get the new active dependencies we might learn about because
          -- we have chosen a new flag.
          let newactives = extractNewFlagDeps qfn b npfa qdeps
          -- As in the package case, we try to extend the partial assignment.
          case extend (F qfn) ppa newactives of
            Left (c, d) -> return (Fail (c `S.union` goalReasonToVars gr) (Conflicting d)) -- inconsistency found
            Right nppa  -> local (\ s -> s { pa = PA nppa npfa }) r

-- | We try to extract as many concrete dependencies from the given flagged
-- dependencies as possible. We make use of all the flag knowledge we have
-- already acquired.
extractDeps :: FAssignment -> FlaggedDeps QPN -> [Dep QPN]
extractDeps fa deps = do
  d <- deps
  case d of
    Simple sd           -> return sd
    Flagged qfn _ td fd -> case M.lookup qfn fa of
                             Nothing    -> mzero
                             Just True  -> extractDeps fa td
                             Just False -> extractDeps fa fd

-- | We try to find new dependencies that become available due to the given
-- flag choice. We therefore look for the flag in question, and then call
-- 'extractDeps' for everything underneath.
extractNewFlagDeps :: QFN -> Bool -> FAssignment -> FlaggedDeps QPN -> [Dep QPN]
extractNewFlagDeps qfn b fa deps = do
  d <- deps
  case d of
    Simple _             -> mzero
    Flagged qfn' _ td fd
      | qfn == qfn'      -> L.map (resetVar (F qfn)) $
                            if b then extractDeps fa td else extractDeps fa fd
      | otherwise        -> case M.lookup qfn' fa of
                              Nothing    -> mzero
                              Just True  -> extractNewFlagDeps qfn b fa td
                              Just False -> extractNewFlagDeps qfn b fa fd

-- | Interface.
validateTree :: Index -> Tree (GoalReason, Scope) -> Tree GoalReason
validateTree idx t = runReader (validate t) (VS idx M.empty (PA M.empty M.empty))
