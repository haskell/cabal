{-# LANGUAGE DeriveFunctor #-}
module Distribution.Client.Dependency.Modular.Dependency (
    -- * Variables
    Var(..)
  , simplifyVar
  , showVar
    -- * Conflict sets
  , ConflictSet
  , showCS
    -- * Constrained instances
  , CI(..)
  , showCI
  , merge
    -- * Flagged dependencies
  , FlaggedDeps
  , FlaggedDep(..)
  , TrueFlaggedDeps
  , FalseFlaggedDeps
  , Dep(..)
  , showDep
    -- ** Setting/forgetting components
  , forgetCompOpenGoal
  , setCompFlaggedDeps
    -- ** Selecting subsets
  , nonSetupDeps
  , setupDeps
  , select
    -- * Reverse dependency map
  , RevDepMap
    -- * Goals
  , Goal(..)
  , GoalReason(..)
  , GoalReasonChain
  , QGoalReasonChain
  , ResetGoal(..)
  , toConflictSet
  , goalReasonToVars
  , goalReasonChainToVars
  , goalReasonChainsToVars
    -- * Open goals
  , OpenGoal(..)
  , close
    -- * Version ranges pairsed with origins (goals)
  , VROrigin
  , collapse
  ) where

import Prelude hiding (pi)

import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Set  as S

import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Version

import Distribution.Client.ComponentDeps (Component(..))

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | The type of variables that play a role in the solver.
-- Note that the tree currently does not use this type directly,
-- and rather has separate tree nodes for the different types of
-- variables. This fits better with the fact that in most cases,
-- these have to be treated differently.
--
-- TODO: This isn't the ideal location to declare the type,
-- but we need them for constrained instances.
data Var qpn = P qpn | F (FN qpn) | S (SN qpn)
  deriving (Eq, Ord, Show, Functor)

-- | For computing conflict sets, we map flag choice vars to a
-- single flag choice. This means that all flag choices are treated
-- as interdependent. So if one flag of a package ends up in a
-- conflict set, then all flags are being treated as being part of
-- the conflict set.
simplifyVar :: Var qpn -> Var qpn
simplifyVar (P qpn)       = P qpn
simplifyVar (F (FN pi _)) = F (FN pi (mkFlag "flag"))
simplifyVar (S qsn)       = S qsn

showVar :: Var QPN -> String
showVar (P qpn) = showQPN qpn
showVar (F qfn) = showQFN qfn
showVar (S qsn) = showQSN qsn

{-------------------------------------------------------------------------------
  Conflict sets
-------------------------------------------------------------------------------}

type ConflictSet qpn = Set (Var qpn)

showCS :: ConflictSet QPN -> String
showCS = intercalate ", " . L.map showVar . S.toList

{-------------------------------------------------------------------------------
  Constrained instances
-------------------------------------------------------------------------------}

-- | Constrained instance. If the choice has already been made, this is
-- a fixed instance, and we record the package name for which the choice
-- is for convenience. Otherwise, it is a list of version ranges paired with
-- the goals / variables that introduced them.
data CI qpn = Fixed I (Goal qpn) | Constrained [VROrigin qpn]
  deriving (Eq, Show, Functor)

showCI :: CI QPN -> String
showCI (Fixed i _)      = "==" ++ showI i
showCI (Constrained vr) = showVR (collapse vr)

-- | Merge constrained instances. We currently adopt a lazy strategy for
-- merging, i.e., we only perform actual checking if one of the two choices
-- is fixed. If the merge fails, we return a conflict set indicating the
-- variables responsible for the failure, as well as the two conflicting
-- fragments.
--
-- Note that while there may be more than one conflicting pair of version
-- ranges, we only return the first we find.
--
-- TODO: Different pairs might have different conflict sets. We're
-- obviously interested to return a conflict that has a "better" conflict
-- set in the sense the it contains variables that allow us to backjump
-- further. We might apply some heuristics here, such as to change the
-- order in which we check the constraints.
merge :: Ord qpn => CI qpn -> CI qpn -> Either (ConflictSet qpn, (CI qpn, CI qpn)) (CI qpn)
merge c@(Fixed i g1)       d@(Fixed j g2)
  | i == j                                    = Right c
  | otherwise                                 = Left (S.union (toConflictSet g1) (toConflictSet g2), (c, d))
merge c@(Fixed (I v _) g1)   (Constrained rs) = go rs -- I tried "reverse rs" here, but it seems to slow things down ...
  where
    go []              = Right c
    go (d@(vr, g2) : vrs)
      | checkVR vr v   = go vrs
      | otherwise      = Left (S.union (toConflictSet g1) (toConflictSet g2), (c, Constrained [d]))
merge c@(Constrained _)    d@(Fixed _ _)      = merge d c
merge   (Constrained rs)     (Constrained ss) = Right (Constrained (rs ++ ss))

{-------------------------------------------------------------------------------
  Flagged dependencies
-------------------------------------------------------------------------------}

-- | Flagged dependencies
--
-- 'FlaggedDeps' is the modular solver's view of a packages dependencies:
-- rather than having the dependencies indexed by component, each dependency
-- defines what component it is in.
--
-- However, top-level goals are also modelled as dependencies, but of course
-- these don't actually belong in any component of any package. Therefore, we
-- parameterize 'FlaggedDeps' and derived datatypes with a type argument that
-- specifies whether or not we have a component: we only ever instantiate this
-- type argument with @()@ for top-level goals, or 'Component' for everything
-- else (we could express this as a kind at the type-level, but that would
-- require a very recent GHC).
--
-- Note however, crucially, that independent of the type parameters, the list
-- of dependencies underneath a flag choice or stanza choices _always_ uses
-- Component as the type argument. This is important: when we pick a value for
-- a flag, we _must_ know what component the new dependencies belong to, or
-- else we don't be able to construct fine-grained reverse dependencies.
type FlaggedDeps comp qpn = [FlaggedDep comp qpn]

-- | Flagged dependencies can either be plain dependency constraints,
-- or flag-dependent dependency trees.
data FlaggedDep comp qpn =
    Flagged (FN qpn) FInfo (TrueFlaggedDeps qpn) (FalseFlaggedDeps qpn)
  | Stanza  (SN qpn)       (TrueFlaggedDeps qpn)
  | Simple (Dep qpn) comp
  deriving (Eq, Show, Functor)

type TrueFlaggedDeps  qpn = FlaggedDeps Component qpn
type FalseFlaggedDeps qpn = FlaggedDeps Component qpn

-- | A dependency (constraint) associates a package name with a
-- constrained instance.
data Dep qpn = Dep qpn (CI qpn)
  deriving (Eq, Show, Functor)

showDep :: Dep QPN -> String
showDep (Dep qpn (Fixed i (Goal v _))          ) =
  (if P qpn /= v then showVar v ++ " => " else "") ++
  showQPN qpn ++ "==" ++ showI i
showDep (Dep qpn (Constrained [(vr, Goal v _)])) =
  showVar v ++ " => " ++ showQPN qpn ++ showVR vr
showDep (Dep qpn ci                            ) =
  showQPN qpn ++ showCI ci

{-------------------------------------------------------------------------------
  Setting/forgetting the Component
-------------------------------------------------------------------------------}

forgetCompOpenGoal :: OpenGoal Component -> OpenGoal ()
forgetCompOpenGoal = mapCompOpenGoal $ const ()

setCompFlaggedDeps :: Component -> FlaggedDeps () qpn -> FlaggedDeps Component qpn
setCompFlaggedDeps = mapCompFlaggedDeps . const

{-------------------------------------------------------------------------------
  Auxiliary: Mapping over the Component goal

  We don't export these, because the only type instantiations for 'a' and 'b'
  here should be () or Component. (We could express this at the type level
  if we relied on newer versions of GHC.)
-------------------------------------------------------------------------------}

mapCompOpenGoal :: (a -> b) -> OpenGoal a -> OpenGoal b
mapCompOpenGoal g (OpenGoal d gr) = OpenGoal (mapCompFlaggedDep g d) gr

mapCompFlaggedDeps :: (a -> b) -> FlaggedDeps a qpn -> FlaggedDeps b qpn
mapCompFlaggedDeps = L.map . mapCompFlaggedDep

mapCompFlaggedDep :: (a -> b) -> FlaggedDep a qpn -> FlaggedDep b qpn
mapCompFlaggedDep _ (Flagged fn nfo t f) = Flagged fn nfo   t f
mapCompFlaggedDep _ (Stanza  sn     t  ) = Stanza  sn       t
mapCompFlaggedDep g (Simple  pn a      ) = Simple  pn (g a)

{-------------------------------------------------------------------------------
  Selecting FlaggedDeps subsets

  (Correspond to the functions with the same names in ComponentDeps).
-------------------------------------------------------------------------------}

nonSetupDeps :: FlaggedDeps Component a -> FlaggedDeps Component a
nonSetupDeps = select (/= ComponentSetup)

setupDeps :: FlaggedDeps Component a -> FlaggedDeps Component a
setupDeps = select (== ComponentSetup)

-- | Select the dependencies of a given component
--
-- The modular solver kind of flattens the dependency trees from the .cabal
-- file, putting the component of each dependency at the leaves, rather than
-- indexing per component. For instance, package C might have flagged deps that
-- look something like
--
-- > Flagged <flagName> ..
-- >   [Simple <package A> ComponentLib]
-- >   [Simple <package B> ComponentLib]
--
-- indicating that the library component of C relies on either A or B, depending
-- on the flag. This makes it somewhat awkward however to extract certain kinds
-- of dependencies. In particular, extracting, say, the setup dependencies from
-- the above set of dependencies could either return the empty list, or else
--
-- > Flagged <flagName> ..
-- >   []
-- >   []
--
-- Both answers are reasonable; we opt to return the empty list in this
-- case, as it results in simpler search trees in the builder.
--
-- (Note that the builder already introduces separate goals for all flags of a
-- package, independently of whether or not they are used in any component, so
-- we don't have to worry about preserving flags here.)
select :: (Component -> Bool) -> FlaggedDeps Component a -> FlaggedDeps Component a
select p = mapMaybe go
  where
    go :: FlaggedDep Component a -> Maybe (FlaggedDep Component a)
    go (Flagged fn nfo  t f) = let t' = mapMaybe go t
                                   f' = mapMaybe go f
                               in if null t' && null f'
                                     then Nothing
                                     else Just $ Flagged fn nfo t' f'
    go (Stanza  sn      t  ) = let t' = mapMaybe go t
                               in if null t'
                                     then Nothing
                                     else Just $ Stanza  sn     t'
    go (Simple  pn comp    ) = if p comp then Just $ Simple pn comp
                                         else Nothing

{-------------------------------------------------------------------------------
  Reverse dependency map
-------------------------------------------------------------------------------}

-- | A map containing reverse dependencies between qualified
-- package names.
type RevDepMap = Map QPN [(Component, QPN)]

{-------------------------------------------------------------------------------
  Goals
-------------------------------------------------------------------------------}

-- | Goals are solver variables paired with information about
-- why they have been introduced.
data Goal qpn = Goal (Var qpn) (GoalReasonChain qpn)
  deriving (Eq, Show, Functor)

-- | Reasons why a goal can be added to a goal set.
data GoalReason qpn =
    UserGoal
  | PDependency (PI qpn)
  | FDependency (FN qpn) Bool
  | SDependency (SN qpn)
  deriving (Eq, Show, Functor)

-- | The first element is the immediate reason. The rest are the reasons
-- for the reasons ...
type GoalReasonChain qpn = [GoalReason qpn]

type QGoalReasonChain = GoalReasonChain QPN

class ResetGoal f where
  resetGoal :: Goal qpn -> f qpn -> f qpn

instance ResetGoal CI where
  resetGoal g (Fixed i _)       = Fixed i g
  resetGoal g (Constrained vrs) = Constrained (L.map (\ (x, y) -> (x, resetGoal g y)) vrs)

instance ResetGoal Dep where
  resetGoal g (Dep qpn ci) = Dep qpn (resetGoal g ci)

instance ResetGoal Goal where
  resetGoal = const

-- | Compute a conflic set from a goal. The conflict set contains the
-- closure of goal reasons as well as the variable of the goal itself.
toConflictSet :: Ord qpn => Goal qpn -> ConflictSet qpn
toConflictSet (Goal g grs) = S.insert (simplifyVar g) (goalReasonChainToVars grs)

goalReasonToVars :: GoalReason qpn -> ConflictSet qpn
goalReasonToVars UserGoal                 = S.empty
goalReasonToVars (PDependency (PI qpn _)) = S.singleton (P qpn)
goalReasonToVars (FDependency qfn _)      = S.singleton (simplifyVar (F qfn))
goalReasonToVars (SDependency qsn)        = S.singleton (S qsn)

goalReasonChainToVars :: Ord qpn => GoalReasonChain qpn -> ConflictSet qpn
goalReasonChainToVars = S.unions . L.map goalReasonToVars

goalReasonChainsToVars :: Ord qpn => [GoalReasonChain qpn] -> ConflictSet qpn
goalReasonChainsToVars = S.unions . L.map goalReasonChainToVars

{-------------------------------------------------------------------------------
  Open goals
-------------------------------------------------------------------------------}

-- | For open goals as they occur during the build phase, we need to store
-- additional information about flags.
data OpenGoal comp = OpenGoal (FlaggedDep comp QPN) QGoalReasonChain
  deriving (Eq, Show)

-- | Closes a goal, i.e., removes all the extraneous information that we
-- need only during the build phase.
close :: OpenGoal comp -> Goal QPN
close (OpenGoal (Simple (Dep qpn _) _) gr) = Goal (P qpn) gr
close (OpenGoal (Flagged qfn _ _ _ )   gr) = Goal (F qfn) gr
close (OpenGoal (Stanza  qsn _)        gr) = Goal (S qsn) gr

{-------------------------------------------------------------------------------
  Version ranges paired with origins
-------------------------------------------------------------------------------}

type VROrigin qpn = (VR, Goal qpn)

-- | Helper function to collapse a list of version ranges with origins into
-- a single, simplified, version range.
collapse :: [VROrigin qpn] -> VR
collapse = simplifyVR . L.foldr (.&&.) anyVR . L.map fst
