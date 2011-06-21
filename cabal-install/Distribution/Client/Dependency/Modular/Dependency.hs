module Distribution.Client.Dependency.Modular.Dependency where

import Data.List as L
import Data.Map as M
import Data.Set as S

import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Version

-- | The type of variables that play a role in the solver.
-- Note that the tree currently does not use this type directly,
-- and rather has to separate tree nodes for the two types of
-- variables. This fits better with the fact that in most cases,
-- these have to be treated differently.
--
-- TODO: This isn't the ideal location to declare the type,
-- but we need them for constrained instances.
data Var qpn = P qpn | F (FN qpn)
  deriving (Eq, Ord, Show)

showVar :: Var QPN -> String
showVar (P qpn) = showQPN qpn
showVar (F qfn) = showQFN qfn

instance Functor Var where
  fmap f (P n)  = P (f n)
  fmap f (F fn) = F (fmap f fn)

class ResetVar f where
  resetVar :: Var qpn -> f qpn -> f qpn

instance ResetVar Var where
  resetVar = const

type ConflictSet qpn = Set (Var qpn)

showCS :: ConflictSet QPN -> String
showCS = intercalate ", " . L.map showVar . S.toList

-- | Constrained instance. If the choice has already been made, this is
-- a fixed instance, and we record the package name for which the choice
-- is for convenience. Otherwise, it is a list of version ranges paired with
-- the goals / variables that introduced them.
data CI qpn = Fixed I qpn | Constrained [VROrigin qpn]
  deriving (Eq, Show)

instance Functor CI where
  fmap f (Fixed i n)       = Fixed i (f n)
  fmap f (Constrained vrs) = Constrained (L.map (\ (x, y) -> (x, fmap f y)) vrs)

instance ResetVar CI where
  resetVar v (Constrained vrs) = Constrained (L.map (\ (x, y) -> (x, resetVar v y)) vrs)
  resetVar _ x                 = x

type VROrigin qpn = (VR, Var qpn)

-- | Helper function to collapse a list of version ranges with origins into
-- a single, simplified, version range.
collapse :: [VROrigin qpn] -> VR
collapse = simplifyVR . foldr (.&&.) anyVR . L.map fst

showCI :: CI QPN -> String
showCI (Fixed i _)      = "==" ++ showI i
showCI (Constrained vr) = showVR (collapse vr)

-- | Merge constrained instances. We currently adopt a lazy strategy for
-- merging, i.e., we only perform actual checking if one of the two choices
-- is fixed. We return the two inconsistent fragment if the merge fails.
--
-- TODO: In fact, we only return the first pair of inconsistent fragments
-- now. It might be better to return them all, but I don't know.
merge :: Ord qpn => CI qpn -> CI qpn -> Either (ConflictSet qpn, (CI qpn, CI qpn)) (CI qpn)
merge c@(Fixed i p1)       d@(Fixed j p2)
  | i == j                                   = Right c
  | otherwise                                = Left (S.fromList [P p1, P p2], (c, d))
merge c@(Fixed (I v _) p)   (Constrained rs) = go rs
  where
    go []              = Right c
    go ((vr, o) : vrs)
      | checkVR vr v   = go vrs
      | otherwise      = Left (S.fromList [P p, o], (c, Constrained [(vr, o)]))
merge c@(Constrained _)   d@(Fixed _ _)      = merge d c
merge   (Constrained rs)    (Constrained ss) = Right (Constrained (rs ++ ss))


type FlaggedDeps qpn = [FlaggedDep qpn]

-- | Flagged dependencies can either be plain dependency constraints,
-- or flag-dependent dependency trees.
data FlaggedDep qpn =
    Flagged (FN qpn) FDefault (TrueFlaggedDeps qpn) (FalseFlaggedDeps qpn)
  | Simple (Dep qpn)
  deriving (Eq, Show)

instance Functor FlaggedDep where
  fmap f (Flagged x y tt ff) = Flagged (fmap f x) y
                                       (fmap (fmap f) tt) (fmap (fmap f) ff)
  fmap f (Simple d)          = Simple (fmap f d)

type TrueFlaggedDeps  qpn = FlaggedDeps qpn
type FalseFlaggedDeps qpn = FlaggedDeps qpn

-- | A dependency (constraint) associates a package name with a
-- constrained instance.
data Dep qpn = Dep qpn (CI qpn)
  deriving (Eq, Show)

showDep :: Dep QPN -> String
showDep (Dep qpn (Constrained [(vr, o)])) = showQPN qpn ++ showVR vr ++ " introduced by " ++ showVar o
showDep (Dep qpn ci                     ) = showQPN qpn ++ showCI ci

instance Functor Dep where
  fmap f (Dep x y) = Dep (f x) (fmap f y)

instance ResetVar Dep where
  resetVar v (Dep qpn ci) = Dep qpn (resetVar v ci)

-- | A map containing reverse dependencies between qualified
-- package names.
type RevDepMap = Map QPN [QPN]
