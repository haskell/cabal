module Distribution.Client.Dependency.Modular.Preference where

-- Reordering or pruning the tree in order to prefer or make certain choices.

import Control.Applicative
import qualified Data.List as L
import qualified Data.Map as M
import Data.Monoid
import Data.Ord

import Distribution.Client.Dependency.Types
  ( PackageConstraint(..), PackagePreferences(..), InstalledPreference(..) )

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.Version

-- | Generic abstraction for strategies that just rearrange the package order.
-- Only packages that match the given predicate are reordered.
packageOrderFor :: (PN -> Bool) -> (PN -> I -> I -> Ordering) -> Tree a -> Tree a
packageOrderFor p cmp = trav go
  where
    go (PChoiceF v@(Q _ pn) r cs)
      | p pn                        = PChoiceF v r (P.sortByKeys (flip (cmp pn)) cs)
      | otherwise                   = PChoiceF v r                               cs
    go x                            = x

-- | Reorder according to global preferences.
preferPreferences :: Preferences -> Tree a -> Tree a
preferPreferences prefs = packageOrderFor (`M.member` prefs) preference
  where
    preference pn (I v1 _) (I v2 _) = preferredVersionsOrdering preferred v1 v2
      where preferred   = M.findWithDefault anyVR pn prefs

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


-- | Traversal that tries to establish various kinds of user constraints. Works
-- by selectively disabling choices that have been rules out by global user
-- constraints.
enforcePackageConstraints :: M.Map PN PackageConstraint -> Tree QGoalReasons -> Tree QGoalReasons
enforcePackageConstraints pcs = trav go
  where
    go x@(PChoiceF qpn@(Q _ pn)               gr   ts) =
      let c = toConflictSet (Goal (P qpn) gr) in
      case M.lookup pn pcs of
        Just (PackageConstraintVersion _ vr) ->
          PChoiceF qpn gr
            (P.mapWithKey (\ (I v _) r -> if checkVR vr v
                                            then r
                                            else Fail c (GlobalConstraintVersion vr))
                          ts)
        Just (PackageConstraintInstalled _) ->
          PChoiceF qpn gr
            (P.mapWithKey (\ i r -> if instI i
                                      then r
                                      else Fail c GlobalConstraintInstalled)
                          ts)
        Just (PackageConstraintSource    _) ->
          PChoiceF qpn gr
            (P.mapWithKey (\ i r -> if not (instI i)
                                      then r
                                      else Fail c GlobalConstraintSource)
                          ts)
        _ -> x
    go x@(FChoiceF qfn@(FN (PI (Q _ pn) _) f) gr tr ts) =
      let c = toConflictSet (Goal (F qfn) gr) in
      case M.lookup pn pcs of
        Just (PackageConstraintFlags _ fa) ->
          case L.lookup f fa of
            Nothing -> x
            Just b  ->
              FChoiceF qfn gr tr
                (P.mapWithKey (\ b' r -> if b == b'
                                            then r
                                            else Fail c GlobalConstraintFlag)
                              ts)
        _ -> x
    go x = x

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
requireInstalled :: (PN -> Bool) -> Tree (QGoalReasons, a) -> Tree (QGoalReasons, a)
requireInstalled p = trav go
  where
    go (PChoiceF v@(Q _ pn) i@(gr, _) cs)
      | p pn      = PChoiceF v i (P.mapWithKey installed cs)
      | otherwise = PChoiceF v i                         cs
      where
        installed (I _ (Inst _)) x = x
        installed _              _ = Fail (toConflictSet (Goal (P v) gr)) CannotInstall
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
avoidReinstalls :: (PN -> Bool) -> Tree (QGoalReasons, a) -> Tree (QGoalReasons, a)
avoidReinstalls p = trav go
  where
    go (PChoiceF qpn@(Q _ pn) i@(gr, _) cs)
      | p pn      = PChoiceF qpn i disableReinstalls
      | otherwise = PChoiceF qpn i cs
      where
        disableReinstalls =
          let installed = [ v | (I v (Inst _), _) <- toList cs ]
          in  P.mapWithKey (notReinstall installed) cs

        notReinstall vs (I v InRepo) _
          | v `elem` vs                = Fail (toConflictSet (Goal (P qpn) gr)) CannotReinstall
        notReinstall _  _            x = x
    go x          = x

type GlobalFlags = M.Map Flag    Bool
type LocalFlags  = M.Map (FN PN) Bool

-- | Enforce flag choices explicitly given by some outer context,
-- for example by the user on the command line. For maximum
-- flexibility we allow both global and local choices, where local
-- choices override the global ones.
enforceFlagChoices :: GlobalFlags -> LocalFlags -> Tree a -> Tree a
enforceFlagChoices gfs lfs = trav go
  where
    go (FChoiceF qfn@(FN _ f) r tr cs) =
      case M.lookup (fmap unQualify qfn) lfs <|> M.lookup f gfs of -- find flag in either map
         Nothing -> FChoiceF qfn r tr            cs  -- if nothing specified, use old node
         Just b  -> FChoiceF qfn r tr (enforce b cs) -- keep only the chosen variant
    go x = x

    enforce :: Bool -> PSQ Bool (Tree a) -> PSQ Bool (Tree a)
    enforce b = P.filterKeys (== b)

-- | Always choose the first goal in the list next, abandoning all
-- other choices.
--
-- This is unnecessary for the default search strategy, because
-- it descends only into the first goal choice anyway,
-- but may still make sense to just reduce the tree size a bit.
firstGoal :: Tree a -> Tree a
firstGoal = trav go
  where
    go (GoalChoiceF xs) = casePSQ xs (GoalChoiceF xs) (\ _ t _ -> out t)
    go x                = x
    -- Note that we keep empty choice nodes, because they mean success.

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

-- | Transformation that tries to avoid making inconsequential
-- flag choices early.
deferDefaultFlagChoices :: Tree a -> Tree a
deferDefaultFlagChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortBy defer xs)
    go x                = x

    defer :: Tree a -> Tree a -> Ordering
    defer (FChoice _ _ True _) _ = GT
    defer _ (FChoice _ _ True _) = LT
    defer _ _                    = EQ

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

