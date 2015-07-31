{-# LANGUAGE CPP #-}
module Distribution.Client.Dependency.Modular.Preference where

-- Reordering or pruning the tree in order to prefer or make certain choices.

import qualified Data.List as L
import qualified Data.Map as M
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Control.Applicative
#endif
import qualified Data.Set as S
import Prelude hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Data.Ord
import Data.Map (Map)
import Data.Traversable (sequence)

import Distribution.Client.Dependency.Types
  ( PackageConstraint(..), LabeledPackageConstraint(..)
  , PackagePreferences(..), InstalledPreference(..) )
import Distribution.Client.Types
  ( OptionalStanza(..) )

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.Version

-- | Generic abstraction for strategies that just rearrange the package order.
-- Only packages that match the given predicate are reordered.
packageOrderFor :: (PN -> Bool) -> (PN -> I -> I -> Ordering) -> Tree a -> Tree a
packageOrderFor p cmp' = trav go
  where
    go (PChoiceF v@(Q _ pn) r cs)
      | p pn                        = PChoiceF v r (P.sortByKeys (flip (cmp pn)) cs)
      | otherwise                   = PChoiceF v r                               cs
    go x                            = x

    cmp :: PN -> POption -> POption -> Ordering
    cmp pn (POption i _) (POption i' _) = cmp' pn i i'

-- | Prefer to link packages whenever possible
preferLinked :: Tree a -> Tree a
preferLinked = trav go
  where
    go (PChoiceF qn a  cs) = PChoiceF qn a (P.sortByKeys cmp cs)
    go x                   = x

    cmp (POption _ linkedTo) (POption _ linkedTo') = cmpL linkedTo linkedTo'

    cmpL Nothing  Nothing  = EQ
    cmpL Nothing  (Just _) = GT
    cmpL (Just _) Nothing  = LT
    cmpL (Just _) (Just _) = EQ


-- | Ordering that treats preferred versions as greater than non-preferred
-- versions.
preferredVersionsOrdering :: VR -> Ver -> Ver -> Ordering
preferredVersionsOrdering vr v1 v2 =
  compare (checkVR vr v1) (checkVR vr v2)

-- | Traversal that tries to establish package preferences (not constraints).
-- Works by reordering choice nodes.
preferPackagePreferences :: (PN -> PackagePreferences) -> Tree a -> Tree a
preferPackagePreferences pcs = packageOrderFor (const True) preference
  where
    preference pn i1@(I v1 _) i2@(I v2 _) =
      let PackagePreferences vr ipref = pcs pn
      in  preferredVersionsOrdering vr v1 v2 `mappend` -- combines lexically
          locationsOrdering ipref i1 i2

    -- Note that we always rank installed before uninstalled, and later
    -- versions before earlier, but we can change the priority of the
    -- two orderings.
    locationsOrdering PreferInstalled v1 v2 =
      preferInstalledOrdering v1 v2 `mappend` preferLatestOrdering v1 v2
    locationsOrdering PreferLatest v1 v2 =
      preferLatestOrdering v1 v2 `mappend` preferInstalledOrdering v1 v2

-- | Ordering that treats installed instances as greater than uninstalled ones.
preferInstalledOrdering :: I -> I -> Ordering
preferInstalledOrdering (I _ (Inst _)) (I _ (Inst _)) = EQ
preferInstalledOrdering (I _ (Inst _)) _              = GT
preferInstalledOrdering _              (I _ (Inst _)) = LT
preferInstalledOrdering _              _              = EQ

-- | Compare instances by their version numbers.
preferLatestOrdering :: I -> I -> Ordering
preferLatestOrdering (I v1 _) (I v2 _) = compare v1 v2

-- | Helper function that tries to enforce a single package constraint on a
-- given instance for a P-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintP :: ConflictSet QPN
                          -> I
                          -> LabeledPackageConstraint
                          -> Tree a
                          -> Tree a
processPackageConstraintP c i (LabeledPackageConstraint pc src) r = go i pc
  where
    go (I v _) (PackageConstraintVersion _ vr)
        | checkVR vr v  = r
        | otherwise     = Fail c (GlobalConstraintVersion vr src)
    go _       (PackageConstraintInstalled _)
        | instI i       = r
        | otherwise     = Fail c (GlobalConstraintInstalled src)
    go _       (PackageConstraintSource    _)
        | not (instI i) = r
        | otherwise     = Fail c (GlobalConstraintSource src)
    go _       _ = r

-- | Helper function that tries to enforce a single package constraint on a
-- given flag setting for an F-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintF :: Flag
                          -> ConflictSet QPN
                          -> Bool
                          -> LabeledPackageConstraint
                          -> Tree a
                          -> Tree a
processPackageConstraintF f c b' (LabeledPackageConstraint pc src) r = go pc
  where
    go (PackageConstraintFlags _ fa) =
        case L.lookup f fa of
          Nothing            -> r
          Just b | b == b'   -> r
                 | otherwise -> Fail c (GlobalConstraintFlag src)
    go _                             = r

-- | Helper function that tries to enforce a single package constraint on a
-- given flag setting for an F-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintS :: OptionalStanza
                          -> ConflictSet QPN
                          -> Bool
                          -> LabeledPackageConstraint
                          -> Tree a
                          -> Tree a
processPackageConstraintS s c b' (LabeledPackageConstraint pc src) r = go pc
  where
    go (PackageConstraintStanzas _ ss) =
        if not b' && s `elem` ss then Fail c (GlobalConstraintFlag src)
                                 else r
    go _                               = r

-- | Traversal that tries to establish various kinds of user constraints. Works
-- by selectively disabling choices that have been ruled out by global user
-- constraints.
enforcePackageConstraints :: M.Map PN [LabeledPackageConstraint]
                          -> Tree QGoalReasonChain
                          -> Tree QGoalReasonChain
enforcePackageConstraints pcs = trav go
  where
    go (PChoiceF qpn@(Q _ pn)               gr      ts) =
      let c = toConflictSet (Goal (P qpn) gr)
          -- compose the transformation functions for each of the relevant constraint
          g = \ (POption i _) -> foldl (\ h pc -> h . processPackageConstraintP   c i pc) id
                           (M.findWithDefault [] pn pcs)
      in PChoiceF qpn gr      (P.mapWithKey g ts)
    go (FChoiceF qfn@(FN (PI (Q _ pn) _) f) gr tr m ts) =
      let c = toConflictSet (Goal (F qfn) gr)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintF f c b pc) id
                           (M.findWithDefault [] pn pcs)
      in FChoiceF qfn gr tr m (P.mapWithKey g ts)
    go (SChoiceF qsn@(SN (PI (Q _ pn) _) f) gr tr   ts) =
      let c = toConflictSet (Goal (S qsn) gr)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintS f c b pc) id
                           (M.findWithDefault [] pn pcs)
      in SChoiceF qsn gr tr   (P.mapWithKey g ts)
    go x = x

-- | Transformation that tries to enforce manual flags. Manual flags
-- can only be re-set explicitly by the user. This transformation should
-- be run after user preferences have been enforced. For manual flags,
-- it checks if a user choice has been made. If not, it disables all but
-- the first choice.
enforceManualFlags :: Tree QGoalReasonChain -> Tree QGoalReasonChain
enforceManualFlags = trav go
  where
    go (FChoiceF qfn gr tr True ts) = FChoiceF qfn gr tr True $
      let c = toConflictSet (Goal (F qfn) gr)
      in  case span isDisabled (P.toList ts) of
            ([], y : ys) -> P.fromList (y : L.map (\ (b, _) -> (b, Fail c ManualFlag)) ys)
            _            -> ts -- something has been manually selected, leave things alone
      where
        isDisabled (_, Fail _ (GlobalConstraintFlag _)) = True
        isDisabled _                                    = False
    go x                                                   = x

-- | Prefer installed packages over non-installed packages, generally.
-- All installed packages or non-installed packages are treated as
-- equivalent.
preferInstalled :: Tree a -> Tree a
preferInstalled = packageOrderFor (const True) (const preferInstalledOrdering)

-- | Prefer packages with higher version numbers over packages with
-- lower version numbers, for certain packages.
preferLatestFor :: (PN -> Bool) -> Tree a -> Tree a
preferLatestFor p = packageOrderFor p (const preferLatestOrdering)

-- | Prefer packages with higher version numbers over packages with
-- lower version numbers, for all packages.
preferLatest :: Tree a -> Tree a
preferLatest = preferLatestFor (const True)

-- | Require installed packages.
requireInstalled :: (PN -> Bool) -> Tree QGoalReasonChain -> Tree QGoalReasonChain
requireInstalled p = trav go
  where
    go (PChoiceF v@(Q _ pn) gr cs)
      | p pn      = PChoiceF v gr (P.mapWithKey installed cs)
      | otherwise = PChoiceF v gr                         cs
      where
        installed (POption (I _ (Inst _)) _) x = x
        installed _ _ = Fail (toConflictSet (Goal (P v) gr)) CannotInstall
    go x          = x

-- | Avoid reinstalls.
--
-- This is a tricky strategy. If a package version is installed already and the
-- same version is available from a repo, the repo version will never be chosen.
-- This would result in a reinstall (either destructively, or potentially,
-- shadowing). The old instance won't be visible or even present anymore, but
-- other packages might have depended on it.
--
-- TODO: It would be better to actually check the reverse dependencies of installed
-- packages. If they're not depended on, then reinstalling should be fine. Even if
-- they are, perhaps this should just result in trying to reinstall those other
-- packages as well. However, doing this all neatly in one pass would require to
-- change the builder, or at least to change the goal set after building.
avoidReinstalls :: (PN -> Bool) -> Tree QGoalReasonChain -> Tree QGoalReasonChain
avoidReinstalls p = trav go
  where
    go (PChoiceF qpn@(Q _ pn) gr cs)
      | p pn      = PChoiceF qpn gr disableReinstalls
      | otherwise = PChoiceF qpn gr cs
      where
        disableReinstalls =
          let installed = [ v | (POption (I v (Inst _)) _, _) <- toList cs ]
          in  P.mapWithKey (notReinstall installed) cs

        notReinstall vs (POption (I v InRepo) _) _ | v `elem` vs =
          Fail (toConflictSet (Goal (P qpn) gr)) CannotReinstall
        notReinstall _ _ x =
          x
    go x          = x

-- | Always choose the first goal in the list next, abandoning all
-- other choices.
--
-- This is unnecessary for the default search strategy, because
-- it descends only into the first goal choice anyway,
-- but may still make sense to just reduce the tree size a bit.
firstGoal :: Tree a -> Tree a
firstGoal = trav go
  where
    go (GoalChoiceF xs) = -- casePSQ xs (GoalChoiceF xs) (\ _ t _ -> out t) -- more space efficient, but removes valuable debug info
                          casePSQ xs (GoalChoiceF (fromList [])) (\ g t _ -> GoalChoiceF (fromList [(g, t)]))
    go x                = x
    -- Note that we keep empty choice nodes, because they mean success.

-- | Transformation that tries to make a decision on base as early as
-- possible. In nearly all cases, there's a single choice for the base
-- package. Also, fixing base early should lead to better error messages.
preferBaseGoalChoice :: Tree a -> Tree a
preferBaseGoalChoice = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortByKeys preferBase xs)
    go x                = x

    preferBase :: OpenGoal comp -> OpenGoal comp -> Ordering
    preferBase (OpenGoal (Simple (Dep (Q _pp pn) _) _) _) _ | unPN pn == "base" = LT
    preferBase _ (OpenGoal (Simple (Dep (Q _pp pn) _) _) _) | unPN pn == "base" = GT
    preferBase _ _                                                              = EQ

-- | Deal with setup dependencies after regular dependencies, so that we can
-- will link setup depencencies against package dependencies when possible
deferSetupChoices :: Tree a -> Tree a
deferSetupChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortByKeys deferSetup xs)
    go x                = x

    deferSetup :: OpenGoal comp -> OpenGoal comp -> Ordering
    deferSetup (OpenGoal (Simple (Dep (Q (Setup _ _) _) _) _) _) _ = GT
    deferSetup _ (OpenGoal (Simple (Dep (Q (Setup _ _) _) _) _) _) = LT
    deferSetup _ _                                                 = EQ

-- | Transformation that sorts choice nodes so that
-- child nodes with a small branching degree are preferred. As a
-- special case, choices with 0 branches will be preferred (as they
-- are immediately considered inconsistent), and choices with 1
-- branch will also be preferred (as they don't involve choice).
preferEasyGoalChoices :: Tree a -> Tree a
preferEasyGoalChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortBy (comparing choices) xs)
    go x                = x

-- | Transformation that tries to avoid making weak flag choices early.
-- Weak flags are trivial flags (not influencing dependencies) or such
-- flags that are explicitly declared to be weak in the index.
deferWeakFlagChoices :: Tree a -> Tree a
deferWeakFlagChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortBy defer xs)
    go x                = x

    defer :: Tree a -> Tree a -> Ordering
    defer (FChoice _ _ True _ _) _ = GT
    defer _ (FChoice _ _ True _ _) = LT
    defer _ _                      = EQ

-- | Variant of 'preferEasyGoalChoices'.
--
-- Only approximates the number of choices in the branches. Less accurate,
-- more efficient.
lpreferEasyGoalChoices :: Tree a -> Tree a
lpreferEasyGoalChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortBy (comparing lchoices) xs)
    go x                = x

-- | Variant of 'preferEasyGoalChoices'.
--
-- I first thought that using a paramorphism might be faster here,
-- but it doesn't seem to make any difference.
preferEasyGoalChoices' :: Tree a -> Tree a
preferEasyGoalChoices' = para (inn . go)
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.map fst (P.sortBy (comparing (choices . snd)) xs))
    go x                = fmap fst x

-- | Monad used internally in enforceSingleInstanceRestriction
type EnforceSIR = Reader (Map (PI PN) QPN)

-- | Enforce ghc's single instance restriction
--
-- From the solver's perspective, this means that for any package instance
-- (that is, package name + package version) there can be at most one qualified
-- goal resolving to that instance (there may be other goals _linking_ to that
-- instance however).
enforceSingleInstanceRestriction :: Tree QGoalReasonChain -> Tree QGoalReasonChain
enforceSingleInstanceRestriction = (`runReader` M.empty) . cata go
  where
    go :: TreeF QGoalReasonChain (EnforceSIR (Tree QGoalReasonChain)) -> EnforceSIR (Tree QGoalReasonChain)

    -- We just verify package choices.
    go (PChoiceF qpn gr cs) =
      PChoice qpn gr <$> sequence (P.mapWithKey (goP qpn) cs)
    go _otherwise =
      innM _otherwise

    -- The check proper
    goP :: QPN -> POption -> EnforceSIR (Tree QGoalReasonChain) -> EnforceSIR (Tree QGoalReasonChain)
    goP qpn@(Q _ pn) (POption i linkedTo) r = do
      let inst = PI pn i
      env <- ask
      case (linkedTo, M.lookup inst env) of
        (Just _, _) ->
          -- For linked nodes we don't check anything
          r
        (Nothing, Nothing) ->
          -- Not linked, not already used
          local (M.insert inst qpn) r
        (Nothing, Just qpn') -> do
          -- Not linked, already used. This is an error
          return $ Fail (S.fromList [P qpn, P qpn']) MultipleInstances
