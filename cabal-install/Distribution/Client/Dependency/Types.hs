{-# LANGUAGE DeriveFunctor #-}
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

    PreSolver(..),
    Solver(..),
    DependencyResolver,

    AllowNewer(..), isAllowNewer,
    PackageConstraint(..),
    debugPackageConstraint,
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
         ( OptionalStanza(..), SourcePackage(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Compat.ReadP
         ( (<++) )

import qualified Distribution.Compat.ReadP as Parse
         ( pfail, munch1 )
import Distribution.PackageDescription
         ( FlagAssignment, FlagName(..) )
import qualified Distribution.Client.PackageIndex as PackageIndex
         ( PackageIndex )
import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import Distribution.Package
         ( Dependency, PackageName, InstalledPackageId )
import Distribution.Version
         ( VersionRange, simplifyVersionRange )
import Distribution.Compiler
         ( CompilerInfo )
import Distribution.System
         ( Platform )
import Distribution.Text
         ( Text(..), display )

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
data PreSolver = AlwaysTopDown | AlwaysModular | Choose
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | All the solvers that can be used.
data Solver = TopDown | Modular
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Text PreSolver where
  disp AlwaysTopDown = text "topdown"
  disp AlwaysModular = text "modular"
  disp Choose        = text "choose"
  parse = do
    name <- Parse.munch1 isAlpha
    case map toLower name of
      "topdown" -> return AlwaysTopDown
      "modular" -> return AlwaysModular
      "choose"  -> return Choose
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
                       -> CompilerInfo
                       -> InstalledPackageIndex
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

-- | Provide a textual representation of a package constraint
-- for debugging purposes.
--
debugPackageConstraint :: PackageConstraint -> String
debugPackageConstraint (PackageConstraintVersion pn vr) =
  display pn ++ " " ++ display (simplifyVersionRange vr)
debugPackageConstraint (PackageConstraintInstalled pn) =
  display pn ++ " installed"
debugPackageConstraint (PackageConstraintSource pn) =
  display pn ++ " source"
debugPackageConstraint (PackageConstraintFlags pn fs) =
  "flags " ++ display pn ++ " " ++ unwords (map (uncurry showFlag) fs)
  where
    showFlag (FlagName f) True  = "+" ++ f
    showFlag (FlagName f) False = "-" ++ f
debugPackageConstraint (PackageConstraintStanzas pn ss) =
  "stanzas " ++ display pn ++ " " ++ unwords (map showStanza ss)
  where
    showStanza TestStanzas  = "test"
    showStanza BenchStanzas = "bench"

-- | A per-package preference on the version. It is a soft constraint that the
-- 'DependencyResolver' should try to respect where possible. It consists of
-- a 'InstalledPreference' which says if we prefer versions of packages
-- that are already installed. It also has a 'PackageVersionPreference' which
-- is a suggested constraint on the version number. The resolver should try to
-- use package versions that satisfy the suggested version constraint.
--
-- It is not specified if preferences on some packages are more important than
-- others.
--
data PackagePreferences = PackagePreferences VersionRange InstalledPreference

-- | Whether we prefer an installed version of a package or simply the latest
-- version.
--
data InstalledPreference = PreferInstalled | PreferLatest
  deriving Show

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
  deriving Show

-- | Policy for relaxing upper bounds in dependencies. For example, given
-- 'build-depends: array >= 0.3 && < 0.5', are we allowed to relax the upper
-- bound and choose a version of 'array' that is greater or equal to 0.5? By
-- default the upper bounds are always strictly honored.
data AllowNewer =

  -- | Default: honor the upper bounds in all dependencies, never choose
  -- versions newer than allowed.
  AllowNewerNone

  -- | Ignore upper bounds in dependencies on the given packages.
  | AllowNewerSome [PackageName]

  -- | Ignore upper bounds in dependencies on all packages.
  | AllowNewerAll

-- | Convert 'AllowNewer' to a boolean.
isAllowNewer :: AllowNewer -> Bool
isAllowNewer AllowNewerNone     = False
isAllowNewer (AllowNewerSome _) = True
isAllowNewer AllowNewerAll      = True

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- result which may be used to indicate progress and\/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done
  deriving Functor

-- | Consume a 'Progress' calculation. Much like 'foldr' for lists but with two
-- base cases, one for a final result and one for failure.
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

instance Monad (Progress step fail) where
  return a = Done a
  p >>= f  = foldProgress Step Fail f p

instance Applicative (Progress step fail) where
  pure a  = Done a
  p <*> x = foldProgress Step Fail (flip fmap x) p

instance Monoid fail => Alternative (Progress step fail) where
  empty   = Fail mempty
  p <|> q = foldProgress Step (const q) Done p
