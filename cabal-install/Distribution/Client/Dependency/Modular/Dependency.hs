module Distribution.Client.Dependency.Modular.Dependency where

import Prelude hiding (pi)

import Data.List as L
import Data.Map as M
import Data.Set as S

import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Version

-- | The type of variables that play a role in the solver.
-- Note that the tree currently does not use this type directly,
-- and rather has separate tree nodes for the different types of
-- variables. This fits better with the fact that in most cases,
-- these have to be treated differently.
--
-- TODO: This isn't the ideal location to declare the type,
-- but we need them for constrained instances.
data Var qpn = P qpn | F (FN qpn) | S (SN qpn)
  deriving (Eq, Ord, Show)

showVar :: Var QPN -> String
showVar (P qpn) = showQPN qpn
showVar (F qfn) = showQFN qfn
showVar (S qsn) = showQSN qsn

instance Functor Var where
  fmap f (P n)  = P (f n)
  fmap f (F fn) = F (fmap f fn)
  fmap f (S sn) = S (fmap f sn)

type ConflictSet qpn = Set (Var qpn)

showCS :: ConflictSet QPN -> String
showCS = intercalate ", " . L.map showVar . S.toList

-- | Constrained instance. If the choice has already been made, this is
-- a fixed instance, and we record the package name for which the choice
-- is for convenience. Otherwise, it is a list of version ranges paired with
-- the goals / variables that introduced them.
data CI qpn = Fixed I (Goal qpn) | Constrained [VROrigin qpn]
  deriving (Eq, Show)

instance Functor CI where
  fmap f (Fixed i g)       = Fixed i (fmap f g)
  fmap f (Constrained vrs) = Constrained (L.map (\ (x, y) -> (x, fmap f y)) vrs)

instance ResetGoal CI where
  resetGoal g (Fixed i _)       = Fixed i g
  resetGoal g (Constrained vrs) = Constrained (L.map (\ (x, y) -> (x, resetGoal g y)) vrs)

type VROrigin qpn = (VR, Goal qpn)

-- | Helper function to collapse a list of version ranges with origins into
-- a single, simplified, version range.
collapse :: [VROrigin qpn] -> VR
collapse = simplifyVR . L.foldr (.&&.) anyVR . L.map fst

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


type FlaggedDeps qpn = [FlaggedDep qpn]

-- | Flagged dependencies can either be plain dependency constraints,
-- or flag-dependent dependency trees.
data FlaggedDep qpn =
    Flagged (FN qpn) FInfo (TrueFlaggedDeps qpn) (FalseFlaggedDeps qpn)
  | Stanza  (SN qpn)       (TrueFlaggedDeps qpn)
  | Simple (Dep qpn)
  deriving (Eq, Show)

instance Functor FlaggedDep where
  fmap f (Flagged x y tt ff) = Flagged (fmap f x) y
                                       (fmap (fmap f) tt) (fmap (fmap f) ff)
  fmap f (Stanza x tt)       = Stanza (fmap f x) (fmap (fmap f) tt)
  fmap f (Simple d)          = Simple (fmap f d)

type TrueFlaggedDeps  qpn = FlaggedDeps qpn
type FalseFlaggedDeps qpn = FlaggedDeps qpn

-- | A dependency (constraint) associates a package name with a
-- constrained instance.
data Dep qpn = Dep qpn (CI qpn)
  deriving (Eq, Show)

showDep :: Dep QPN -> String
showDep (Dep qpn (Fixed i (Goal v _))          ) =
  (if P qpn /= v then showVar v ++ " => " else "") ++
  showQPN qpn ++ "==" ++ showI i
showDep (Dep qpn (Constrained [(vr, Goal v _)])) =
  showVar v ++ " => " ++ showQPN qpn ++ showVR vr
showDep (Dep qpn ci                            ) =
  showQPN qpn ++ showCI ci

instance Functor Dep where
  fmap f (Dep x y) = Dep (f x) (fmap f y)

instance ResetGoal Dep where
  resetGoal g (Dep qpn ci) = Dep qpn (resetGoal g ci)

-- | A map containing reverse dependencies between qualified
-- package names.
type RevDepMap = Map QPN [QPN]

-- | Goals are solver variables paired with information about
-- why they have been introduced.
data Goal qpn = Goal (Var qpn) (GoalReasonChain qpn)
  deriving (Eq, Show)

instance Functor Goal where
  fmap f (Goal v grs) = Goal (fmap f v) (fmap (fmap f) grs)

class ResetGoal f where
  resetGoal :: Goal qpn -> f qpn -> f qpn

instance ResetGoal Goal where
  resetGoal = const

-- | For open goals as they occur during the build phase, we need to store
-- additional information about flags.
data OpenGoal = OpenGoal (FlaggedDep QPN) QGoalReasonChain
  deriving (Eq, Show)

-- | Reasons why a goal can be added to a goal set.
data GoalReason qpn =
    UserGoal
  | PDependency (PI qpn)
  | FDependency (FN qpn) Bool
  | SDependency (SN qpn)
  deriving (Eq, Show)

instance Functor GoalReason where
  fmap _ UserGoal           = UserGoal
  fmap f (PDependency pi)   = PDependency (fmap f pi)
  fmap f (FDependency fn b) = FDependency (fmap f fn) b
  fmap f (SDependency sn)   = SDependency (fmap f sn)

-- | The first element is the immediate reason. The rest are the reasons
-- for the reasons ...
type GoalReasonChain qpn = [GoalReason qpn]

type QGoalReasonChain = GoalReasonChain QPN

goalReasonToVars :: GoalReason qpn -> ConflictSet qpn
goalReasonToVars UserGoal                 = S.empty
goalReasonToVars (PDependency (PI qpn _)) = S.singleton (P qpn)
goalReasonToVars (FDependency qfn _)      = S.singleton (F qfn)
goalReasonToVars (SDependency qsn)        = S.singleton (S qsn)

goalReasonChainToVars :: Ord qpn => GoalReasonChain qpn -> ConflictSet qpn
goalReasonChainToVars = S.unions . L.map goalReasonToVars

goalReasonChainsToVars :: Ord qpn => [GoalReasonChain qpn] -> ConflictSet qpn
goalReasonChainsToVars = S.unions . L.map goalReasonChainToVars

-- | Closes a goal, i.e., removes all the extraneous information that we
-- need only during the build phase.
close :: OpenGoal -> Goal QPN
close (OpenGoal (Simple (Dep qpn _)) gr) = Goal (P qpn) gr
close (OpenGoal (Flagged qfn _ _ _ ) gr) = Goal (F qfn) gr
close (OpenGoal (Stanza  qsn _)      gr) = Goal (S qsn) gr

-- | Compute a conflic set from a goal. The conflict set contains the
-- closure of goal reasons as well as the variable of the goal itself.
toConflictSet :: Ord qpn => Goal qpn -> ConflictSet qpn
toConflictSet (Goal g grs) = S.insert g (goalReasonChainToVars grs)
