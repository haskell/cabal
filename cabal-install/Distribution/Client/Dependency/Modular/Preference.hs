{-# LANGUAGE CPP #-}
module Distribution.Client.Dependency.Modular.Preference
    ( avoidReinstalls
    , deferSetupChoices
    , deferWeakFlagChoices
    , enforceManualFlags
    , enforcePackageConstraints
    , enforceSingleInstanceRestriction
    , firstGoal
    , preferBaseGoalChoice
    , preferEasyGoalChoices
    , preferLinked
    , preferPackagePreferences
    , preferReallyEasyGoalChoices
    , requireInstalled
    ) where

-- Reordering or pruning the tree in order to prefer or make certain choices.

import qualified Data.List as L
import qualified Data.Map as M
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Control.Applicative
#endif
import Prelude hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Data.Map (Map)
import Data.Traversable (sequence)

import Distribution.Client.Dependency.Types
  ( PackageConstraint(..), LabeledPackageConstraint(..), ConstraintSource(..)
  , PackagePreferences(..), InstalledPreference(..) )
import Distribution.Client.Types
  ( OptionalStanza(..) )

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import qualified Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.Version
import qualified Distribution.Client.Dependency.Modular.ConflictSet as CS

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

-- | Ordering that treats versions satisfying more preferred ranges as greater
--   than versions satisfying less preferred ranges.
preferredVersionsOrdering :: [VR] -> Ver -> Ver -> Ordering
preferredVersionsOrdering vrs v1 v2 = compare (check v1) (check v2)
  where
     check v = Prelude.length . Prelude.filter (==True) .
               Prelude.map (flip checkVR v) $ vrs

-- | Traversal that tries to establish package preferences (not constraints).
-- Works by reordering choice nodes. Also applies stanza preferences.
preferPackagePreferences :: (PN -> PackagePreferences) -> Tree a -> Tree a
preferPackagePreferences pcs = preferPackageStanzaPreferences pcs
                             . packageOrderFor (const True) preference
  where
    preference pn i1@(I v1 _) i2@(I v2 _) =
      let PackagePreferences vrs ipref _ = pcs pn
      in  preferredVersionsOrdering vrs v1 v2 `mappend` -- combines lexically
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

-- | Traversal that tries to establish package stanza enable\/disable
-- preferences. Works by reordering the branches of stanza choices.
preferPackageStanzaPreferences :: (PN -> PackagePreferences) -> Tree a -> Tree a
preferPackageStanzaPreferences pcs = trav go
  where
    go (SChoiceF qsn@(SN (PI (Q pp pn) _) s) gr _tr ts) | primaryPP pp =
        let PackagePreferences _ _ spref = pcs pn
            enableStanzaPref = s `elem` spref
                  -- move True case first to try enabling the stanza
            ts' | enableStanzaPref = P.sortByKeys (flip compare) ts
                | otherwise        = ts
         in SChoiceF qsn gr True ts'   -- True: now weak choice
    go x = x

-- | Helper function that tries to enforce a single package constraint on a
-- given instance for a P-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintP :: PP
                          -> ConflictSet QPN
                          -> I
                          -> LabeledPackageConstraint
                          -> Tree a
                          -> Tree a
processPackageConstraintP pp _ _ (LabeledPackageConstraint _ src) r
  | src == ConstraintSourceUserTarget && not (primaryPP pp)         = r
    -- the constraints arising from targets, like "foo-1.0" only apply to
    -- the main packages in the solution, they don't constrain setup deps

processPackageConstraintP _ c i (LabeledPackageConstraint pc src) r = go i pc
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
                          -> Tree QGoalReason
                          -> Tree QGoalReason
enforcePackageConstraints pcs = trav go
  where
    go (PChoiceF qpn@(Q pp pn)              gr      ts) =
      let c = varToConflictSet (P qpn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ (POption i _) -> foldl (\ h pc -> h . processPackageConstraintP pp c i pc) id
                           (M.findWithDefault [] pn pcs)
      in PChoiceF qpn gr      (P.mapWithKey g ts)
    go (FChoiceF qfn@(FN (PI (Q _ pn) _) f) gr tr m ts) =
      let c = varToConflictSet (F qfn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintF f c b pc) id
                           (M.findWithDefault [] pn pcs)
      in FChoiceF qfn gr tr m (P.mapWithKey g ts)
    go (SChoiceF qsn@(SN (PI (Q _ pn) _) f) gr tr   ts) =
      let c = varToConflictSet (S qsn)
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
enforceManualFlags :: Tree QGoalReason -> Tree QGoalReason
enforceManualFlags = trav go
  where
    go (FChoiceF qfn gr tr True ts) = FChoiceF qfn gr tr True $
      let c = varToConflictSet (F qfn)
      in  case span isDisabled (P.toList ts) of
            ([], y : ys) -> P.fromList (y : L.map (\ (b, _) -> (b, Fail c ManualFlag)) ys)
            _            -> ts -- something has been manually selected, leave things alone
      where
        isDisabled (_, Fail _ (GlobalConstraintFlag _)) = True
        isDisabled _                                    = False
    go x                                                   = x

-- | Require installed packages.
requireInstalled :: (PN -> Bool) -> Tree QGoalReason -> Tree QGoalReason
requireInstalled p = trav go
  where
    go (PChoiceF v@(Q _ pn) gr cs)
      | p pn      = PChoiceF v gr (P.mapWithKey installed cs)
      | otherwise = PChoiceF v gr                         cs
      where
        installed (POption (I _ (Inst _)) _) x = x
        installed _ _ = Fail (varToConflictSet (P v)) CannotInstall
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
avoidReinstalls :: (PN -> Bool) -> Tree QGoalReason -> Tree QGoalReason
avoidReinstalls p = trav go
  where
    go (PChoiceF qpn@(Q _ pn) gr cs)
      | p pn      = PChoiceF qpn gr disableReinstalls
      | otherwise = PChoiceF qpn gr cs
      where
        disableReinstalls =
          let installed = [ v | (POption (I v (Inst _)) _, _) <- P.toList cs ]
          in  P.mapWithKey (notReinstall installed) cs

        notReinstall vs (POption (I v InRepo) _) _ | v `elem` vs =
          Fail (varToConflictSet (P qpn)) CannotReinstall
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
    go (GoalChoiceF xs) = GoalChoiceF (P.firstOnly xs)
    go x                = x
    -- Note that we keep empty choice nodes, because they mean success.

-- | Transformation that tries to make a decision on base as early as
-- possible. In nearly all cases, there's a single choice for the base
-- package. Also, fixing base early should lead to better error messages.
preferBaseGoalChoice :: Tree a -> Tree a
preferBaseGoalChoice = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.preferByKeys isBase xs)
    go x                = x

    isBase :: OpenGoal comp -> Bool
    isBase (OpenGoal (Simple (Dep (Q _pp pn) _) _) _) | unPN pn == "base" = True
    isBase _                                                              = False

-- | Deal with setup dependencies after regular dependencies, so that we can
-- will link setup depencencies against package dependencies when possible
deferSetupChoices :: Tree a -> Tree a
deferSetupChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.preferByKeys noSetup xs)
    go x                = x

    noSetup :: OpenGoal comp -> Bool
    noSetup (OpenGoal (Simple (Dep (Q (PP _ns (Setup _)) _) _) _) _) = False
    noSetup _                                                        = True

-- | Transformation that tries to avoid making weak flag choices early.
-- Weak flags are trivial flags (not influencing dependencies) or such
-- flags that are explicitly declared to be weak in the index.
deferWeakFlagChoices :: Tree a -> Tree a
deferWeakFlagChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.prefer noWeakStanza (P.prefer noWeakFlag xs))
    go x                = x

    noWeakStanza :: Tree a -> Bool
    noWeakStanza (SChoice _ _ True _) = False
    noWeakStanza _                    = True

    noWeakFlag :: Tree a -> Bool
    noWeakFlag (FChoice _ _ True _ _) = False
    noWeakFlag _                      = True

-- | Transformation that sorts choice nodes so that
-- child nodes with a small branching degree are preferred.
--
-- Only approximates the number of choices in the branches.
-- In particular, we try to take any goal immediately if it has
-- a branching degree of 0 (guaranteed failure) or 1 (no other
-- choice possible).
--
-- Returns at most one choice.
--
preferEasyGoalChoices :: Tree a -> Tree a
preferEasyGoalChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.dminimumBy dchoices xs)
      -- (a different implementation that seems slower):
      -- GoalChoiceF (P.firstOnly (P.preferOrElse zeroOrOneChoices (P.minimumBy choices) xs))
    go x                = x

-- | A variant of 'preferEasyGoalChoices' that just keeps the
-- ones with a branching degree of 0 or 1. Note that unlike
-- 'preferEasyGoalChoices', this may return more than one
-- choice.
--
preferReallyEasyGoalChoices :: Tree a -> Tree a
preferReallyEasyGoalChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.prefer zeroOrOneChoices xs)
    go x                = x

-- | Monad used internally in enforceSingleInstanceRestriction
--
-- For each package instance we record the goal for which we picked a concrete
-- instance. The SIR means that for any package instance there can only be one.
type EnforceSIR = Reader (Map (PI PN) QPN)

-- | Enforce ghc's single instance restriction
--
-- From the solver's perspective, this means that for any package instance
-- (that is, package name + package version) there can be at most one qualified
-- goal resolving to that instance (there may be other goals _linking_ to that
-- instance however).
enforceSingleInstanceRestriction :: Tree QGoalReason -> Tree QGoalReason
enforceSingleInstanceRestriction = (`runReader` M.empty) . cata go
  where
    go :: TreeF QGoalReason (EnforceSIR (Tree QGoalReason)) -> EnforceSIR (Tree QGoalReason)

    -- We just verify package choices.
    go (PChoiceF qpn gr cs) =
      PChoice qpn gr <$> sequence (P.mapWithKey (goP qpn) cs)
    go _otherwise =
      innM _otherwise

    -- The check proper
    goP :: QPN -> POption -> EnforceSIR (Tree QGoalReason) -> EnforceSIR (Tree QGoalReason)
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
          return $ Fail (CS.union (varToConflictSet (P qpn)) (varToConflictSet (P qpn'))) MultipleInstances
