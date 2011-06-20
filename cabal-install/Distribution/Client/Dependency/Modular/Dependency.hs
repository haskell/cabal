module Distribution.Client.Dependency.Modular.Dependency where

import Data.List as L
import Data.Map as M

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
  deriving (Eq, Show)

instance Functor Var where
  fmap f (P n)  = P (f n)
  fmap f (F fn) = F (fmap f fn)

-- | Constrained instance. If the choice has already been made, this is
-- a fixed instance, and we record the package name for which the choice
-- is for convenience. Otherwise, it is a list of version ranges paired with
-- the goals / variables that introduced them.
data CI qpn = Fixed I qpn | Constrained [VROrigin qpn]
  deriving (Eq, Show)

instance Functor CI where
  fmap f (Fixed i n)       = Fixed i (f n)
  fmap f (Constrained vrs) = Constrained (L.map (\ (x, y) -> (x, fmap f y)) vrs)

type VROrigin qpn = (VR, Var qpn)

-- | Helper function to collapse a list of version ranges with origins into
-- a single, simplified, version range.
collapse :: [VROrigin qpn] -> VR
collapse = simplifyVR . foldr (.&&.) anyVR . L.map fst

showCI :: CI qpn -> String
showCI (Fixed i _)      = "==" ++ showI i
showCI (Constrained vr) = showVR (collapse vr)

-- | Merge constrained instances. We currently adopt a lazy strategy for
-- merging, i.e., we only perform actual checking if one of the two choices
-- is fixed. We return the failing version range and origin if the merge
-- fails.
merge :: CI qpn -> CI qpn -> Maybe (CI qpn)
merge c@(Fixed i _) (Fixed j _)
  | i == j                              = Just c
  | otherwise                           = Nothing
merge c@(Fixed (I v _) _)(Constrained rs)
  | checkVR (collapse rs) v             = Just c
  | otherwise                           = Nothing
merge c@(Constrained _) d@(Fixed _ _)   = merge d c
merge (Constrained rs) (Constrained ss) = Just (Constrained (rs ++ ss))


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
showDep (Dep qpn ci) = showQPN qpn ++ showCI ci

instance Functor Dep where
  fmap f (Dep x y) = Dep (f x) (fmap f y)

-- | A map containing reverse dependencies between qualified
-- package names.
type RevDepMap = Map QPN [QPN]
