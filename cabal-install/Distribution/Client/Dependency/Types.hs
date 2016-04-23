{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
    PreSolver(..),
    Solver(..),
    DependencyResolver,
    ResolverPackage(..),

    PackageConstraint(..),
    showPackageConstraint,
    PackagePreferences(..),
    InstalledPreference(..),
    PackagesPreferenceDefault(..),

    Progress(..),
    foldProgress,

    LabeledPackageConstraint(..),
    ConstraintSource(..),
    unlabelPackageConstraint,
    showConstraintSource

  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
         ( Applicative(..) )
#endif
import Control.Applicative
         ( Alternative(..) )

import Data.Char
         ( isAlpha, toLower )
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
         ( Monoid(..) )
#endif

import Distribution.Client.PkgConfigDb
         ( PkgConfigDb )
import Distribution.Client.Types
         ( OptionalStanza(..), SourcePackage(..), ConfiguredPackage )

import qualified Distribution.Compat.ReadP as Parse
         ( pfail, munch1 )
import Distribution.PackageDescription
         ( FlagAssignment, FlagName(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import qualified Distribution.Client.PackageIndex as PackageIndex
         ( PackageIndex )
import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import Distribution.Package
         ( PackageName )
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
import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary(..))

import Prelude hiding (fail)


-- | All the solvers that can be selected.
data PreSolver = AlwaysTopDown | AlwaysModular | Choose
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

-- | All the solvers that can be used.
data Solver = TopDown | Modular
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance Binary PreSolver
instance Binary Solver

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
                       -> PkgConfigDb
                       -> (PackageName -> PackagePreferences)
                       -> [LabeledPackageConstraint]
                       -> [PackageName]
                       -> Progress String String [ResolverPackage]

-- | The dependency resolver picks either pre-existing installed packages
-- or it picks source packages along with package configuration.
--
-- This is like the 'InstallPlan.PlanPackage' but with fewer cases.
--
data ResolverPackage = PreExisting InstalledPackageInfo
                     | Configured  ConfiguredPackage

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
  deriving (Eq,Show,Generic)

instance Binary PackageConstraint

-- | Provide a textual representation of a package constraint
-- for debugging purposes.
--
showPackageConstraint :: PackageConstraint -> String
showPackageConstraint (PackageConstraintVersion pn vr) =
  display pn ++ " " ++ display (simplifyVersionRange vr)
showPackageConstraint (PackageConstraintInstalled pn) =
  display pn ++ " installed"
showPackageConstraint (PackageConstraintSource pn) =
  display pn ++ " source"
showPackageConstraint (PackageConstraintFlags pn fs) =
  "flags " ++ display pn ++ " " ++ unwords (map (uncurry showFlag) fs)
  where
    showFlag (FlagName f) True  = "+" ++ f
    showFlag (FlagName f) False = "-" ++ f
showPackageConstraint (PackageConstraintStanzas pn ss) =
  "stanzas " ++ display pn ++ " " ++ unwords (map showStanza ss)
  where
    showStanza TestStanzas  = "test"
    showStanza BenchStanzas = "bench"

-- | Per-package preferences on the version. It is a soft constraint that the
-- 'DependencyResolver' should try to respect where possible. It consists of
-- an 'InstalledPreference' which says if we prefer versions of packages
-- that are already installed. It also has (possibly multiple)
-- 'PackageVersionPreference's which are suggested constraints on the version
-- number. The resolver should try to use package versions that satisfy
-- the maximum number of the suggested version constraints.
--
-- It is not specified if preferences on some packages are more important than
-- others.
--
data PackagePreferences = PackagePreferences [VersionRange]
                                             InstalledPreference
                                             [OptionalStanza]

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

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- result which may be used to indicate progress and\/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done
  deriving (Functor)

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
  return   = pure
  p >>= f  = foldProgress Step Fail f p

instance Applicative (Progress step fail) where
  pure a  = Done a
  p <*> x = foldProgress Step Fail (flip fmap x) p

instance Monoid fail => Alternative (Progress step fail) where
  empty   = Fail mempty
  p <|> q = foldProgress Step (const q) Done p

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint
   = LabeledPackageConstraint PackageConstraint ConstraintSource

unlabelPackageConstraint :: LabeledPackageConstraint -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc

-- | Source of a 'PackageConstraint'.
data ConstraintSource =

  -- | Main config file, which is ~/.cabal/config by default.
  ConstraintSourceMainConfig FilePath

  -- | Local cabal.project file
  | ConstraintSourceProjectConfig FilePath

  -- | Sandbox config file, which is ./cabal.sandbox.config by default.
  | ConstraintSourceSandboxConfig FilePath

  -- | User config file, which is ./cabal.config by default.
  | ConstraintSourceUserConfig FilePath

  -- | Flag specified on the command line.
  | ConstraintSourceCommandlineFlag

  -- | Target specified by the user, e.g., @cabal install package-0.1.0.0@
  -- implies @package==0.1.0.0@.
  | ConstraintSourceUserTarget

  -- | Internal requirement to use installed versions of packages like ghc-prim.
  | ConstraintSourceNonUpgradeablePackage

  -- | Internal requirement to use the add-source version of a package when that
  -- version is installed and the source is modified.
  | ConstraintSourceModifiedAddSourceDep

  -- | Internal constraint used by @cabal freeze@.
  | ConstraintSourceFreeze

  -- | Constraint specified by a config file, a command line flag, or a user
  -- target, when a more specific source is not known.
  | ConstraintSourceConfigFlagOrTarget

  -- | The source of the constraint is not specified.
  | ConstraintSourceUnknown
  deriving (Eq, Show, Generic)

instance Binary ConstraintSource

-- | Description of a 'ConstraintSource'.
showConstraintSource :: ConstraintSource -> String
showConstraintSource (ConstraintSourceMainConfig path) =
    "main config " ++ path
showConstraintSource (ConstraintSourceProjectConfig path) =
    "project config " ++ path
showConstraintSource (ConstraintSourceSandboxConfig path) =
    "sandbox config " ++ path
showConstraintSource (ConstraintSourceUserConfig path)= "user config " ++ path
showConstraintSource ConstraintSourceCommandlineFlag = "command line flag"
showConstraintSource ConstraintSourceUserTarget = "user target"
showConstraintSource ConstraintSourceNonUpgradeablePackage =
    "non-upgradeable package"
showConstraintSource ConstraintSourceModifiedAddSourceDep =
    "modified add-source dependency"
showConstraintSource ConstraintSourceFreeze = "cabal freeze"
showConstraintSource ConstraintSourceConfigFlagOrTarget =
    "config file, command line flag, or user target"
showConstraintSource ConstraintSourceUnknown = "unknown source"
