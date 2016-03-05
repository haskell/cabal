{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Dependency.Modular.Dependency (
    -- * Variables
    Var(..)
  , simplifyVar
  , varPI
    -- * Conflict sets
  , ConflictSet
  , showCS
    -- * Constrained instances
  , CI(..)
  , merge
    -- * Flagged dependencies
  , FlaggedDeps
  , FlaggedDep(..)
  , Dep(..)
  , showDep
  , flattenFlaggedDeps
  , QualifyOptions(..)
  , qualifyDeps
    -- ** Setting/forgetting components
  , forgetCompOpenGoal
  , setCompFlaggedDeps
    -- * Reverse dependency map
  , RevDepMap
    -- * Goals
  , Goal(..)
  , GoalReason(..)
  , QGoalReasonChain
  , ResetGoal(..)
  , toConflictSet
  , extendConflictSet
    -- * Open goals
  , OpenGoal(..)
  , close
  ) where

import Prelude hiding (pi)

import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Set  as S

import Language.Haskell.Extension (Extension(..), Language(..))

import Distribution.Text

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

-- | Extract the package instance from a Var
varPI :: Var QPN -> (QPN, Maybe I)
varPI (P qpn)               = (qpn, Nothing)
varPI (F (FN (PI qpn i) _)) = (qpn, Just i)
varPI (S (SN (PI qpn i) _)) = (qpn, Just i)

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
  deriving (Eq, Show)

-- | Conversatively flatten out flagged dependencies
--
-- NOTE: We do not filter out duplicates.
flattenFlaggedDeps :: FlaggedDeps Component qpn -> [(Dep qpn, Component)]
flattenFlaggedDeps = concatMap aux
  where
    aux :: FlaggedDep Component qpn -> [(Dep qpn, Component)]
    aux (Flagged _ _ t f) = flattenFlaggedDeps t ++ flattenFlaggedDeps f
    aux (Stanza  _   t)   = flattenFlaggedDeps t
    aux (Simple d c)      = [(d, c)]

type TrueFlaggedDeps  qpn = FlaggedDeps Component qpn
type FalseFlaggedDeps qpn = FlaggedDeps Component qpn

-- | A dependency (constraint) associates a package name with a
-- constrained instance.
--
-- 'Dep' intentionally has no 'Functor' instance because the type variable
-- is used both to record the dependencies as well as who's doing the
-- depending; having a 'Functor' instance makes bugs where we don't distinguish
-- these two far too likely. (By rights 'Dep' ought to have two type variables.)
data Dep qpn = Dep  qpn (CI qpn)  -- dependency on a package
             | Ext  Extension     -- dependency on a language extension
             | Lang Language      -- dependency on a language version
             | Pkg  PN VR         -- dependency on a pkg-config package
  deriving (Eq, Show)

showDep :: Dep QPN -> String
showDep (Dep qpn (Fixed i (Goal v _))          ) =
  (if P qpn /= v then showVar v ++ " => " else "") ++
  showQPN qpn ++ "==" ++ showI i
showDep (Dep qpn (Constrained [(vr, Goal v _)])) =
  showVar v ++ " => " ++ showQPN qpn ++ showVR vr
showDep (Dep qpn ci                            ) =
  showQPN qpn ++ showCI ci
showDep (Ext ext)   = "requires " ++ display ext
showDep (Lang lang) = "requires " ++ display lang
showDep (Pkg pn vr) = "requires pkg-config package "
                      ++ display pn ++ display vr
                      ++ ", not found in the pkg-config database"

-- | Options for goal qualification (used in 'qualifyDeps')
--
-- See also 'defaultQualifyOptions'
data QualifyOptions = QO {
    -- | Do we have a version of base relying on another version of base?
    qoBaseShim :: Bool

    -- Should dependencies of the setup script be treated as independent?
  , qoSetupIndependent :: Bool
  }
  deriving Show

-- | Apply built-in rules for package qualifiers
--
-- NOTE: It's the _dependencies_ of a package that may or may not be independent
-- from the package itself. Package flag choices must of course be consistent.
qualifyDeps :: QualifyOptions -> QPN -> FlaggedDeps Component PN -> FlaggedDeps Component QPN
qualifyDeps QO{..} (Q pp' pn) = go
  where
    -- The Base qualifier does not get inherited
    pp :: PP
    pp = (if qoBaseShim then stripBase else id) pp'

    go :: FlaggedDeps Component PN -> FlaggedDeps Component QPN
    go = map go1

    go1 :: FlaggedDep Component PN -> FlaggedDep Component QPN
    go1 (Flagged fn nfo t f) = Flagged (fmap (Q pp) fn) nfo (go t) (go f)
    go1 (Stanza  sn     t)   = Stanza  (fmap (Q pp) sn)     (go t)
    go1 (Simple dep comp)    = Simple (goD dep comp) comp

    -- Suppose package B has a setup dependency on package A.
    -- This will be recorded as something like
    --
    -- > Dep "A" (Constrained [(AnyVersion, Goal (P "B") reason])
    --
    -- Observe that when we qualify this dependency, we need to turn that
    -- @"A"@ into @"B-setup.A"@, but we should not apply that same qualifier
    -- to the goal or the goal reason chain.
    goD :: Dep PN -> Component -> Dep QPN
    goD (Ext  ext)    _    = Ext  ext
    goD (Lang lang)   _    = Lang lang
    goD (Pkg pkn vr)  _    = Pkg pkn vr
    goD (Dep  dep ci) comp
      | qBase  dep  = Dep (Q (Base  pn pp) dep) (fmap (Q pp) ci)
      | qSetup comp = Dep (Q (Setup pn pp) dep) (fmap (Q pp) ci)
      | otherwise   = Dep (Q           pp  dep) (fmap (Q pp) ci)

    -- Should we qualify this goal with the 'Base' package path?
    qBase :: PN -> Bool
    qBase dep = qoBaseShim && unPackageName dep == "base"

    -- Should we qualify this goal with the 'Setup' packaeg path?
    qSetup :: Component -> Bool
    qSetup comp = qoSetupIndependent && comp == ComponentSetup

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
  resetGoal _ (Ext ext)    = Ext ext
  resetGoal _ (Lang lang)  = Lang lang
  resetGoal _ (Pkg pn vr)  = Pkg pn vr

instance ResetGoal Goal where
  resetGoal = const

-- | Compute a conflict set from a goal. The conflict set contains the closure
-- of goal reasons as well as the variable of the goal itself.
toConflictSet :: Ord qpn => Goal qpn -> ConflictSet qpn
toConflictSet (Goal g grs) = S.insert (simplifyVar g) (goalReasonChainToVars grs)

-- | Add another variable into a conflict set
extendConflictSet :: Ord qpn => Var qpn -> ConflictSet qpn -> ConflictSet qpn
extendConflictSet = S.insert . simplifyVar

goalReasonToVars :: GoalReason qpn -> ConflictSet qpn
goalReasonToVars UserGoal                 = S.empty
goalReasonToVars (PDependency (PI qpn _)) = S.singleton (P qpn)
goalReasonToVars (FDependency qfn _)      = S.singleton (simplifyVar (F qfn))
goalReasonToVars (SDependency qsn)        = S.singleton (S qsn)

goalReasonChainToVars :: Ord qpn => GoalReasonChain qpn -> ConflictSet qpn
goalReasonChainToVars = S.unions . L.map goalReasonToVars

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
close (OpenGoal (Simple (Ext     _) _) _ ) =
  error "Distribution.Client.Dependency.Modular.Dependency.close: called on Ext goal"
close (OpenGoal (Simple (Lang    _) _) _ ) =
  error "Distribution.Client.Dependency.Modular.Dependency.close: called on Lang goal"
close (OpenGoal (Simple (Pkg   _ _) _) _ ) =
  error "Distribution.Client.Dependency.Modular.Dependency.close: called on Pkg goal"
close (OpenGoal (Flagged qfn _ _ _ )   gr) = Goal (F qfn) gr
close (OpenGoal (Stanza  qsn _)        gr) = Goal (S qsn) gr

{-------------------------------------------------------------------------------
  Version ranges paired with origins
-------------------------------------------------------------------------------}

type VROrigin qpn = (VR, Goal qpn)

-- | Helper function to collapse a list of version ranges with origins into
-- a single, simplified, version range.
collapse :: [VROrigin qpn] -> VR
collapse = simplifyVR . L.foldr ((.&&.) . fst) anyVR
