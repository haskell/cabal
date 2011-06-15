module Distribution.Client.Dependency.Modular.Dependency where

import Data.Map as M

import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package

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
data Dep qpn = Dep qpn CI
  deriving (Eq, Show)

showDep :: Dep QPN -> String
showDep (Dep qpn ci) = showQPN qpn ++ showCI ci

instance Functor Dep where
  fmap f (Dep x y) = Dep (f x) y

-- | A map containing reverse dependencies between qualified
-- package names.
type RevDepMap = Map QPN [QPN]
