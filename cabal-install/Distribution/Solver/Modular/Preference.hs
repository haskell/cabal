{-# LANGUAGE CPP #-}
module Distribution.Solver.Modular.Preference
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
    , sortGoals
    ) where

-- Reordering or pruning the tree in order to prefer or make certain choices.

import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Prelude hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.InstalledPreference
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PackagePreferences
import Distribution.Solver.Types.Variable

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.PSQ as P
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Version
import qualified Distribution.Solver.Modular.ConflictSet as CS
import qualified Distribution.Solver.Modular.WeightedPSQ as W

-- | Update the weights of children under 'PChoice' nodes. 'addWeights' takes a
-- list of weight-calculating functions in order to avoid sorting the package
-- choices multiple times. Each function takes the package name, sorted list of
-- siblings' versions, and package option. 'addWeights' prepends the new
-- weights to the existing weights, which gives precedence to preferences that
-- are applied later.
addWeights :: [PN -> [Ver] -> POption -> Weight] -> Tree a -> Tree a
addWeights fs = trav go
  where
    go (PChoiceF qpn@(Q _ pn) x cs) =
      let sortedVersions = L.sortBy (flip compare) $ L.map version (W.keys cs)
          weights k = [f pn sortedVersions k | f <- fs]
      in  PChoiceF qpn x $
          W.mapWeightsWithKey (\k w -> weights k ++ w) cs
    go x                            = x

addWeight :: (PN -> [Ver] -> POption -> Weight) -> Tree a -> Tree a
addWeight f = addWeights [f]

version :: POption -> Ver
version (POption (I v _) _) = v

-- | Prefer to link packages whenever possible.
preferLinked :: Tree a -> Tree a
preferLinked = addWeight (const (const linked))
  where
    linked (POption _ Nothing)  = 1
    linked (POption _ (Just _)) = 0

-- Works by setting weights on choice nodes. Also applies stanza preferences.
preferPackagePreferences :: (PN -> PackagePreferences) -> Tree a -> Tree a
preferPackagePreferences pcs =
    preferPackageStanzaPreferences pcs .
    addWeights [
          \pn _  opt -> preferred pn opt

        -- Note that we always rank installed before uninstalled, and later
        -- versions before earlier, but we can change the priority of the
        -- two orderings.
        , \pn vs opt -> case preference pn of
                          PreferInstalled -> installed opt
                          PreferLatest    -> latest vs opt
        , \pn vs opt -> case preference pn of
                          PreferInstalled -> latest vs opt
                          PreferLatest    -> installed opt
        ]
  where
    -- Prefer packages with higher version numbers over packages with
    -- lower version numbers.
    latest :: [Ver] -> POption -> Weight
    latest sortedVersions opt =
      let l = length sortedVersions
          index = fromMaybe l $ L.findIndex (<= version opt) sortedVersions
      in  fromIntegral index / fromIntegral l

    preference :: PN -> InstalledPreference
    preference pn =
      let PackagePreferences _ ipref _ = pcs pn
      in  ipref

    -- | Prefer versions satisfying more preferred version ranges.
    preferred :: PN -> POption -> Weight
    preferred pn opt =
      let PackagePreferences vrs _ _ = pcs pn
      in fromIntegral . negate . L.length $
         L.filter (flip checkVR (version opt)) vrs

    -- Prefer installed packages over non-installed packages.
    installed :: POption -> Weight
    installed (POption (I _ (Inst _)) _) = 0
    installed _                          = 1

-- | Traversal that tries to establish package stanza enable\/disable
-- preferences. Works by reordering the branches of stanza choices.
preferPackageStanzaPreferences :: (PN -> PackagePreferences) -> Tree a -> Tree a
preferPackageStanzaPreferences pcs = trav go
  where
    go (SChoiceF qsn@(SN (PI (Q pp pn) _) s) gr _tr ts)
      | primaryPP pp && enableStanzaPref pn s =
          -- move True case first to try enabling the stanza
          let ts' = W.mapWeightsWithKey (\k w -> weight k : w) ts
              weight k = if k then 0 else 1
          -- defer the choice by setting it to weak
          in  SChoiceF qsn gr (WeakOrTrivial True) ts'
    go x = x

    enableStanzaPref :: PN -> OptionalStanza -> Bool
    enableStanzaPref pn s =
      let PackagePreferences _ _ spref = pcs pn
      in  s `elem` spref

-- | Helper function that tries to enforce a single package constraint on a
-- given instance for a P-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintP :: PackagePath
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
                          -> Tree a
                          -> Tree a
enforcePackageConstraints pcs = trav go
  where
    go (PChoiceF qpn@(Q pp pn)              gr      ts) =
      let c = varToConflictSet (P qpn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ (POption i _) -> foldl (\ h pc -> h . processPackageConstraintP pp c i pc) id
                           (M.findWithDefault [] pn pcs)
      in PChoiceF qpn gr      (W.mapWithKey g ts)
    go (FChoiceF qfn@(FN (PI (Q _ pn) _) f) gr tr m ts) =
      let c = varToConflictSet (F qfn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintF f c b pc) id
                           (M.findWithDefault [] pn pcs)
      in FChoiceF qfn gr tr m (W.mapWithKey g ts)
    go (SChoiceF qsn@(SN (PI (Q _ pn) _) f) gr tr   ts) =
      let c = varToConflictSet (S qsn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintS f c b pc) id
                           (M.findWithDefault [] pn pcs)
      in SChoiceF qsn gr tr   (W.mapWithKey g ts)
    go x = x

-- | Transformation that tries to enforce manual flags. Manual flags
-- can only be re-set explicitly by the user. This transformation should
-- be run after user preferences have been enforced. For manual flags,
-- it checks if a user choice has been made. If not, it disables all but
-- the first choice.
enforceManualFlags :: Tree a -> Tree a
enforceManualFlags = trav go
  where
    go (FChoiceF qfn gr tr True ts) = FChoiceF qfn gr tr True $
      let c = varToConflictSet (F qfn)
      in  case span isDisabled (W.toList ts) of
            ([], y : ys) -> W.fromList (y : L.map (\ (w, b, _) -> (w, b, Fail c ManualFlag)) ys)
            _            -> ts -- something has been manually selected, leave things alone
      where
        isDisabled (_, _, Fail _ (GlobalConstraintFlag _)) = True
        isDisabled _                                       = False
    go x                                                   = x

-- | Require installed packages.
requireInstalled :: (PN -> Bool) -> Tree a -> Tree a
requireInstalled p = trav go
  where
    go (PChoiceF v@(Q _ pn) gr cs)
      | p pn      = PChoiceF v gr (W.mapWithKey installed cs)
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
avoidReinstalls :: (PN -> Bool) -> Tree a -> Tree a
avoidReinstalls p = trav go
  where
    go (PChoiceF qpn@(Q _ pn) gr cs)
      | p pn      = PChoiceF qpn gr disableReinstalls
      | otherwise = PChoiceF qpn gr cs
      where
        disableReinstalls =
          let installed = [ v | (_, POption (I v (Inst _)) _, _) <- W.toList cs ]
          in  W.mapWithKey (notReinstall installed) cs

        notReinstall vs (POption (I v InRepo) _) _ | v `elem` vs =
          Fail (varToConflictSet (P qpn)) CannotReinstall
        notReinstall _ _ x =
          x
    go x          = x

-- | Sort all goals using the provided function.
sortGoals :: (Variable QPN -> Variable QPN -> Ordering) -> Tree a -> Tree a
sortGoals variableOrder = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortByKeys goalOrder xs)
    go x                = x

    goalOrder :: Goal QPN -> Goal QPN -> Ordering
    goalOrder = variableOrder `on` (varToVariable . goalToVar)

    varToVariable :: Var QPN -> Variable QPN
    varToVariable (P qpn)                    = PackageVar qpn
    varToVariable (F (FN (PI qpn _) fn))     = FlagVar qpn fn
    varToVariable (S (SN (PI qpn _) stanza)) = StanzaVar qpn stanza

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

    isBase :: Goal QPN -> Bool
    isBase (Goal (P (Q _pp pn)) _) = unPN pn == "base"
    isBase _                       = False

-- | Deal with setup dependencies after regular dependencies, so that we can
-- will link setup depencencies against package dependencies when possible
deferSetupChoices :: Tree a -> Tree a
deferSetupChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.preferByKeys noSetup xs)
    go x                = x

    noSetup :: Goal QPN -> Bool
    noSetup (Goal (P (Q (PackagePath _ns (Setup _)) _)) _) = False
    noSetup _                                              = True

-- | Transformation that tries to avoid making weak flag choices early.
-- Weak flags are trivial flags (not influencing dependencies) or such
-- flags that are explicitly declared to be weak in the index.
deferWeakFlagChoices :: Tree a -> Tree a
deferWeakFlagChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.prefer noWeakStanza (P.prefer noWeakFlag xs))
    go x                = x

    noWeakStanza :: Tree a -> Bool
    noWeakStanza (SChoice _ _ (WeakOrTrivial True) _) = False
    noWeakStanza _                                    = True

    noWeakFlag :: Tree a -> Bool
    noWeakFlag (FChoice _ _ (WeakOrTrivial True) _ _) = False
    noWeakFlag _                                      = True

-- | Transformation that sorts choice nodes so that
-- child nodes with a small branching degree are preferred.
--
-- Only approximates the number of choices in the branches
-- using dchoices which classifies every goal by the number
-- of active choices:
--
-- - 0 (guaranteed failure) or 1 (no other option) active choice
-- - 2 active choices
-- - 3 or more active choices
--
-- We pick the minimum goal according to this approximation.
-- In particular, if we encounter any goal in the first class
-- (0 or 1 option), we do not look any further and choose it
-- immediately.
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
enforceSingleInstanceRestriction :: Tree a -> Tree a
enforceSingleInstanceRestriction = (`runReader` M.empty) . cata go
  where
    go :: TreeF a (EnforceSIR (Tree a)) -> EnforceSIR (Tree a)

    -- We just verify package choices.
    go (PChoiceF qpn gr cs) =
      PChoice qpn gr <$> sequence (W.mapWithKey (goP qpn) cs)
    go _otherwise =
      innM _otherwise

    -- The check proper
    goP :: QPN -> POption -> EnforceSIR (Tree a) -> EnforceSIR (Tree a)
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
