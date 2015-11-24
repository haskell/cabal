{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Solver.Types
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Common types for dependency resolution.
-----------------------------------------------------------------------------
module Distribution.Solver.Types(
    ConfiguredPackage(..),
    ConfiguredId(..),
    fakeComponentId,
    enableStanzas,
    OptionalStanza(..),
    DependencyResolver,
    ResolverPackage(..),
    SourcePackage(..),

    PackageDescriptionOverride,
    PackageConstraint(..),
    showPackageConstraint,
    PackagePreferences(..),
    InstalledPreference(..),

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

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
         ( Monoid(..) )
#endif

import Data.ByteString.Lazy (ByteString)

import Distribution.Solver.ComponentDeps

import Distribution.Package (Package(..), PackageId, ComponentId(..), HasComponentId(..))
import Distribution.PackageDescription
         ( FlagAssignment, FlagName(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import qualified Distribution.Solver.PackageIndex as PackageIndex
         ( PackageIndex )
import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import Distribution.Package
         ( PackageName )
import Distribution.PackageDescription
         ( GenericPackageDescription(..)
         , condBenchmarks
         , condTestSuites
         , benchmarkEnabled
         , testEnabled )
import Distribution.PackageDescription.Configuration
         ( mapTreeData )
import Distribution.Version
         ( VersionRange, simplifyVersionRange )
import Distribution.Compiler
         ( CompilerInfo )
import Distribution.System
         ( Platform )
import Distribution.Text
         ( display )

import Prelude hiding (fail)

-- | A dependency resolver is a function that works out an installation plan
-- given the set of installed and available packages and a set of deps to
-- solve for.
--
-- The reason for this interface is because there are dozens of approaches to
-- solving the package dependency problem and we want to make it easy to swap
-- in alternatives.
--
type DependencyResolver loc = Platform
                            -> CompilerInfo
                            -> InstalledPackageIndex
                            -> PackageIndex.PackageIndex (SourcePackage loc)
                            -> (PackageName -> PackagePreferences)
                            -> [LabeledPackageConstraint]
                            -> [PackageName]
                            -> Progress String String [ResolverPackage loc]

-- | The dependency resolver picks either pre-existing installed packages
-- or it picks source packages along with package configuration.
--
-- This is like the 'InstallPlan.PlanPackage' but with fewer cases.
--
data ResolverPackage loc = PreExisting InstalledPackageInfo
                         | Configured (ConfiguredPackage loc)

-- | A 'ConfiguredPackage' is a not-yet-installed package along with the
-- total configuration information. The configuration information is total in
-- the sense that it provides all the configuration information and so the
-- final configure process will be independent of the environment.
--
data ConfiguredPackage loc = ConfiguredPackage
       (SourcePackage loc) -- package info, including repo
       FlagAssignment      -- complete flag assignment for the package
       [OptionalStanza]    -- list of enabled optional stanzas for the package
       (ComponentDeps [ConfiguredId])
                           -- set of exact dependencies (installed or source).
                           -- These must be consistent with the 'buildDepends'
                           -- in the 'PackageDescription' that you'd get by
                           -- applying the flag assignment and optional stanzas.
  deriving Show

instance Package (ConfiguredPackage loc) where
  packageId (ConfiguredPackage pkg _ _ _) = packageId pkg

instance HasComponentId (ConfiguredPackage loc) where
  installedComponentId = fakeComponentId . packageId

-- | A ConfiguredId is a package ID for a configured package.
--
-- Once we configure a source package we know it's ComponentId
-- (at least, in principle, even if we have to fake it currently). It is still
-- however useful in lots of places to also know the source ID for the package.
-- We therefore bundle the two.
--
-- An already installed package of course is also "configured" (all it's
-- configuration parameters and dependencies have been specified).
--
-- TODO: I wonder if it would make sense to promote this datatype to Cabal
-- and use it consistently instead of ComponentIds?
data ConfiguredId = ConfiguredId {
    confSrcId  :: PackageId
  , confInstId :: ComponentId
  }

instance Show ConfiguredId where
  show = show . confSrcId

-- | A package description along with the location of the package sources.
--
data SourcePackage pkgSrc = SourcePackage {
    packageInfoId        :: PackageId,
    packageDescription   :: GenericPackageDescription,
    packageSource        :: pkgSrc,
    packageDescrOverride :: PackageDescriptionOverride
  }
  deriving Show

-- | We sometimes need to override the .cabal file in the tarball with
-- the newer one from the package index.
type PackageDescriptionOverride = Maybe ByteString

instance Package (SourcePackage a) where packageId = packageInfoId

data OptionalStanza
    = TestStanzas
    | BenchStanzas
  deriving (Eq, Ord, Show)

enableStanzas
    :: [OptionalStanza]
    -> GenericPackageDescription
    -> GenericPackageDescription
enableStanzas stanzas gpkg = gpkg
    { condBenchmarks = flagBenchmarks $ condBenchmarks gpkg
    , condTestSuites = flagTests $ condTestSuites gpkg
    }
  where
    enableTest t = t { testEnabled = TestStanzas `elem` stanzas }
    enableBenchmark bm = bm { benchmarkEnabled = BenchStanzas `elem` stanzas }
    flagBenchmarks = map (\(n, bm) -> (n, mapTreeData enableBenchmark bm))
    flagTests = map (\(n, t) -> (n, mapTreeData enableTest t))

-- | In order to reuse the implementation of PackageIndex which relies on
-- 'ComponentId', we need to be able to synthesize these IDs prior
-- to installation.  Eventually, we'll move to a representation of
-- 'ComponentId' which can be properly computed before compilation
-- (of course, it's a bit of a misnomer since the packages are not actually
-- installed yet.)  In any case, we'll synthesize temporary installed package
-- IDs to use as keys during install planning.  These should never be written
-- out!  Additionally, they need to be guaranteed unique within the install
-- plan.
fakeComponentId :: PackageId -> ComponentId
fakeComponentId = ComponentId . (".fake."++) . display

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

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint
   = LabeledPackageConstraint PackageConstraint ConstraintSource

unlabelPackageConstraint :: LabeledPackageConstraint -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc

-- | Source of a 'PackageConstraint'.
data ConstraintSource =

  -- | Main config file, which is ~/.cabal/config by default.
  ConstraintSourceMainConfig FilePath

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
  deriving (Eq, Show)

-- | Description of a 'ConstraintSource'.
showConstraintSource :: ConstraintSource -> String
showConstraintSource (ConstraintSourceMainConfig path) =
    "main config " ++ path
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
