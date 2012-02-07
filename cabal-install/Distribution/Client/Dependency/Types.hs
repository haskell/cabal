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
    ExtDependency(..),

    Solver(..),
    DependencyResolver,

    PackageConstraint(..),
    PackagePreferences(..),
    InstalledPreference(..),
    PackagesPreferenceDefault(..),

    Progress(..),
    foldProgress,
  ) where

import Control.Applicative
         ( Applicative(..), Alternative(..) )

import Data.Char
         ( isAlpha, toLower )
import Data.Monoid
         ( Monoid(..) )

import Distribution.Client.Types
         ( OptionalStanza, SourcePackage(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Compat.ReadP
         ( (<++) )

import qualified Distribution.Compat.ReadP as Parse
         ( pfail, munch1 )
import Distribution.PackageDescription
         ( FlagAssignment )
import qualified Distribution.Client.PackageIndex as PackageIndex
         ( PackageIndex )
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
         ( PackageIndex )
import Distribution.Package
         ( Dependency, PackageName, InstalledPackageId )
import Distribution.Version
         ( VersionRange )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( Platform )
import Distribution.Text
         ( Text(..) )

import Text.PrettyPrint
         ( text )

import Prelude hiding (fail)

-- | Covers source dependencies and installed dependencies in
-- one type.
data ExtDependency = SourceDependency Dependency
                   | InstalledDependency InstalledPackageId

instance Text ExtDependency where
  disp (SourceDependency    dep) = disp dep
  disp (InstalledDependency dep) = disp dep

  parse = (SourceDependency `fmap` parse) <++ (InstalledDependency `fmap` parse)

-- | All the solvers that can be selected.
data Solver = TopDown | Modular
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Text Solver where
  disp TopDown = text "topdown"
  disp Modular = text "modular"
  parse = do
    name <- Parse.munch1 isAlpha
    case map toLower name of
      "topdown" -> return TopDown
      "modular" -> return Modular
      _         -> Parse.pfail

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
                       -> InstalledPackageIndex.PackageIndex
                       ->          PackageIndex.PackageIndex SourcePackage
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
   | PackageConstraintSource    PackageName
   | PackageConstraintFlags     PackageName FlagAssignment
   | PackageConstraintStanzas   PackageName [OptionalStanza]
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

-- | Global policy for all packages to say if we prefer package versions that
-- are already installed locally or if we just prefer the latest available.
--
data PackagesPreferenceDefault =

     -- | Always prefer the latest version irrespective of any existing
     -- installed version.
     --
     -- * This is the standard policy for upgrade.
     --
     PreferAllLatest

     -- | Always prefer the installed versions over ones that would need to be
     -- installed. Secondarily, prefer latest versions (eg the latest installed
     -- version or if there are none then the latest source version).
   | PreferAllInstalled

     -- | Prefer the latest version for packages that are explicitly requested
     -- but prefers the installed version for any other packages.
     --
     -- * This is the standard policy for install.
     --
   | PreferLatestForSelected

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

instance Applicative (Progress step fail) where
  pure a  = Done a
  p <*> x = foldProgress Step Fail (flip fmap x) p

instance Monoid fail => Alternative (Progress step fail) where
  empty   = Fail mempty
  p <|> q = foldProgress Step (const q) Done p
