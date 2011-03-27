-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency.Types
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Common types for dependency resolution.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency.Types (
    DependencyResolver,

    PackageConstraint(..),
    PackagePreferences(..),
    InstalledPreference(..),

    Progress(..),
    foldProgress,
  ) where

import Distribution.Client.Types
         ( SourcePackage(..), InstalledPackage )
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.PackageDescription
         ( FlagAssignment )
import Distribution.Client.PackageIndex
         ( PackageIndex )
import Distribution.Package
         ( PackageName )
import Distribution.Version
         ( VersionRange )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( Platform )

import Prelude hiding (fail)

-- | A dependency resolver is a function that works out an installation plan
-- given the set of installed and available packages and a set of deps to
-- solve for.
--
-- The reason for this interface is because there are dozens of approaches to
-- solving the package dependency problem and we want to make it easy to swap
-- in alternatives.
--
type DependencyResolver = Platform
                       -> CompilerId
                       -> PackageIndex InstalledPackage
                       -> PackageIndex SourcePackage
                       -> (PackageName -> PackagePreferences)
                       -> [PackageConstraint]
                       -> [PackageName]
                       -> Progress String String [InstallPlan.PlanPackage]

-- | Per-package constraints. Package constraints must be respected by the
-- solver. Multiple constraints for each package can be given, though obviously
-- it is possible to construct conflicting constraints (eg impossible version
-- range or inconsistent flag assignment).
--
data PackageConstraint
   = PackageConstraintVersion   PackageName VersionRange
   | PackageConstraintInstalled PackageName
   | PackageConstraintFlags     PackageName FlagAssignment
  deriving (Show,Eq)

-- | A per-package preference on the version. It is a soft constraint that the
-- 'DependencyResolver' should try to respect where possible. It consists of
-- a 'InstalledPreference' which says if we prefer versions of packages
-- that are already installed. It also hase a 'PackageVersionPreference' which
-- is a suggested constraint on the version number. The resolver should try to
-- use package versions that satisfy the suggested version constraint.
--
-- It is not specified if preferences on some packages are more important than
-- others.
--
data PackagePreferences = PackagePreferences VersionRange InstalledPreference

-- | Wether we prefer an installed version of a package or simply the latest
-- version.
--
data InstalledPreference = PreferInstalled | PreferLatest

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- retult which may be used to indicate progress and\/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done

-- | Consume a 'Progres' calculation. Much like 'foldr' for lists but with
-- two base cases, one for a final result and one for failure.
--
-- Eg to convert into a simple 'Either' result use:
--
-- > foldProgress (flip const) Left Right
--
foldProgress :: (step -> a -> a) -> (fail -> a) -> (done -> a)
             -> Progress step fail done -> a
foldProgress step fail done = fold
  where fold (Step s p) = step s (fold p)
        fold (Fail f)   = fail f
        fold (Done r)   = done r

instance Functor (Progress step fail) where
  fmap f = foldProgress Step Fail (Done . f)

instance Monad (Progress step fail) where
  return a = Done a
  p >>= f  = foldProgress Step Fail f p
