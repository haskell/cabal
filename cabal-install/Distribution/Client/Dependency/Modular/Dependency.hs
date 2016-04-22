{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Dependency.Modular.Dependency (
    -- * Variables
    Var(..)
  , simplifyVar
  , varPI
    -- * Conflict sets
  , ConflictSet
  , CS.showCS
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
  , unqualifyDeps
    -- ** Setting/forgetting components
  , forgetCompOpenGoal
  , setCompFlaggedDeps
    -- * Reverse dependency map
  , RevDepMap
    -- * Goals
  , Goal(..)
  , GoalReason(..)
  , QGoalReason
  , ResetVar(..)
  , goalVarToConflictSet
  , varToConflictSet
  , goalReasonToVars
    -- * Open goals
  , OpenGoal(..)
  , close
  ) where

import Prelude hiding (pi)

import Data.Map (Map)
import qualified Data.List as L

import Language.Haskell.Extension (Extension(..), Language(..))

import Distribution.Text

import Distribution.Client.Dependency.Modular.ConflictSet (ConflictSet)
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Var
import Distribution.Client.Dependency.Modular.Version
import qualified Distribution.Client.Dependency.Modular.ConflictSet as CS

import Distribution.Client.ComponentDeps (Component(..))

{-------------------------------------------------------------------------------
  Constrained instances
-------------------------------------------------------------------------------}

-- | Constrained instance. If the choice has already been made, this is
-- a fixed instance, and we record the package name for which the choice
-- is for convenience. Otherwise, it is a list of version ranges paired with
-- the goals / variables that introduced them.
data CI qpn = Fixed I (Var qpn) | Constrained [VROrigin qpn]
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
  | otherwise                                 = Left (CS.union (varToConflictSet g1) (varToConflictSet g2), (c, d))
merge c@(Fixed (I v _) g1)   (Constrained rs) = go rs -- I tried "reverse rs" here, but it seems to slow things down ...
  where
    go []              = Right c
    go (d@(vr, g2) : vrs)
      | checkVR vr v   = go vrs
      | otherwise      = Left (CS.union (varToConflictSet g1) (varToConflictSet g2), (c, Constrained [d]))
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
showDep (Dep qpn (Fixed i v)            ) =
  (if P qpn /= v then showVar v ++ " => " else "") ++
  showQPN qpn ++ "==" ++ showI i
showDep (Dep qpn (Constrained [(vr, v)])) =
  showVar v ++ " => " ++ showQPN qpn ++ showVR vr
showDep (Dep qpn ci                     ) =
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
-- Although the behaviour of 'qualifyDeps' depends on the 'QualifyOptions',
-- it is important that these 'QualifyOptions' are _static_. Qualification
-- does NOT depend on flag assignment; in other words, it behaves the same no
-- matter which choices the solver makes (modulo the global 'QualifyOptions');
-- we rely on this in 'linkDeps' (see comment there).
--
-- NOTE: It's the _dependencies_ of a package that may or may not be independent
-- from the package itself. Package flag choices must of course be consistent.
qualifyDeps :: QualifyOptions -> QPN -> FlaggedDeps Component PN -> FlaggedDeps Component QPN
qualifyDeps QO{..} (Q pp@(PP ns q) pn) = go
  where
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
      | qBase  dep  = Dep (Q (PP ns (Base  pn)) dep) (fmap (Q pp) ci)
      | qSetup comp = Dep (Q (PP ns (Setup pn)) dep) (fmap (Q pp) ci)
      | otherwise   = Dep (Q (PP ns inheritedQ) dep) (fmap (Q pp) ci)

    -- If P has a setup dependency on Q, and Q has a regular dependency on R, then
    -- we say that the 'Setup' qualifier is inherited: P has an (indirect) setup
    -- dependency on R. We do not do this for the base qualifier however.
    --
    -- The inherited qualifier is only used for regular dependencies; for setup
    -- and base deppendencies we override the existing qualifier. See #3160 for
    -- a detailed discussion.
    inheritedQ :: Qualifier
    inheritedQ = case q of
                   Setup _     -> q
                   Unqualified -> q
                   Base _      -> Unqualified

    -- Should we qualify this goal with the 'Base' package path?
    qBase :: PN -> Bool
    qBase dep = qoBaseShim && unPackageName dep == "base"

    -- Should we qualify this goal with the 'Setup' packaeg path?
    qSetup :: Component -> Bool
    qSetup comp = qoSetupIndependent && comp == ComponentSetup

-- | Remove qualifiers from set of dependencies
--
-- This is used during link validation: when we link package @Q.A@ to @Q'.A@,
-- then all dependencies @Q.B@ need to be linked to @Q'.B@. In order to compute
-- what to link these dependencies to, we need to requalify @Q.B@ to become
-- @Q'.B@; we do this by first removing all qualifiers and then calling
-- 'qualifyDeps' again.
unqualifyDeps :: FlaggedDeps comp QPN -> FlaggedDeps comp PN
unqualifyDeps = go
  where
    go :: FlaggedDeps comp QPN -> FlaggedDeps comp PN
    go = map go1

    go1 :: FlaggedDep comp QPN -> FlaggedDep comp PN
    go1 (Flagged fn nfo t f) = Flagged (fmap unq fn) nfo (go t) (go f)
    go1 (Stanza  sn     t)   = Stanza  (fmap unq sn)     (go t)
    go1 (Simple dep comp)    = Simple (goD dep) comp

    goD :: Dep QPN -> Dep PN
    goD (Dep qpn ci) = Dep (unq qpn) (fmap unq ci)
    goD (Ext  ext)   = Ext ext
    goD (Lang lang)  = Lang lang
    goD (Pkg pn vr)  = Pkg pn vr

    unq :: QPN -> PN
    unq (Q _ pn) = pn

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

-- | A goal is just a solver variable paired with a reason.
-- The reason is only used for tracing.
data Goal qpn = Goal (Var qpn) (GoalReason qpn)
  deriving (Eq, Show, Functor)

-- | Reason why a goal is being added to a goal set.
data GoalReason qpn =
    UserGoal
  | PDependency (PI qpn)
  | FDependency (FN qpn) Bool
  | SDependency (SN qpn)
  deriving (Eq, Show, Functor)

type QGoalReason = GoalReason QPN

class ResetVar f where
  resetVar :: Var qpn -> f qpn -> f qpn

instance ResetVar CI where
  resetVar v (Fixed i _)       = Fixed i v
  resetVar v (Constrained vrs) = Constrained (L.map (\ (x, y) -> (x, resetVar v y)) vrs)

instance ResetVar Dep where
  resetVar v (Dep qpn ci) = Dep qpn (resetVar v ci)
  resetVar _ (Ext ext)    = Ext ext
  resetVar _ (Lang lang)  = Lang lang
  resetVar _ (Pkg pn vr)  = Pkg pn vr

instance ResetVar Var where
  resetVar = const

-- | Compute a singleton conflict set from a goal, containing just
-- the goal variable.
--
-- NOTE: This is just a call to 'varToConflictSet' under the hood;
-- the 'GoalReason' is ignored.
goalVarToConflictSet :: Goal qpn -> ConflictSet qpn
goalVarToConflictSet (Goal g _gr) = varToConflictSet g

-- | Compute a singleton conflict set from a 'Var'
varToConflictSet :: Var qpn -> ConflictSet qpn
varToConflictSet = CS.singleton

-- | A goal reason is mostly just a variable paired with the
-- decision we made for that variable (except for user goals,
-- where we cannot really point to a solver variable). This
-- function drops the decision and recovers the list of
-- variables (which will be empty or contain one element).
--
goalReasonToVars :: GoalReason qpn -> [Var qpn]
goalReasonToVars UserGoal                 = []
goalReasonToVars (PDependency (PI qpn _)) = [P qpn]
goalReasonToVars (FDependency qfn _)      = [F qfn]
goalReasonToVars (SDependency qsn)        = [S qsn]

{-------------------------------------------------------------------------------
  Open goals
-------------------------------------------------------------------------------}

-- | For open goals as they occur during the build phase, we need to store
-- additional information about flags.
data OpenGoal comp = OpenGoal (FlaggedDep comp QPN) QGoalReason
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

type VROrigin qpn = (VR, Var qpn)

-- | Helper function to collapse a list of version ranges with origins into
-- a single, simplified, version range.
collapse :: [VROrigin qpn] -> VR
collapse = simplifyVR . L.foldr ((.&&.) . fst) anyVR
