{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Distribution.Solver.Types.DependencyResolver
    ( ResolverParams (..)
    , DependencyResolver (..)
    , PackagePreference (..)
    , PackagesPreferenceSource
    , PackagesPreferenceDefault (..)
    , showPackagePreference
    , interpretPackagesPreference
    ) where

import Distribution.Solver.Compat.Prelude
    ( Binary, Generic, Structured, Maybe, String, Set, Eq
    , Show (show)
    , (++)
    , const
    )
import Prelude ()
import Data.Kind
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Distribution.Solver.Types.InstalledPreference
    ( InstalledPreference )
import qualified Distribution.Solver.Types.InstalledPreference as Preference
import Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint )
import Distribution.Solver.Types.OptionalStanza ( OptionalStanza )
import Distribution.Solver.Types.PkgConfigDb ( PkgConfigDb )
import Distribution.Solver.Types.PackageIndex ( PackageIndex )
import Distribution.Solver.Types.PackagePreferences ( PackagePreferences (..) )
import Distribution.Solver.Types.Progress
    ( Progress )
import Distribution.Solver.Types.ResolverPackage
    ( ResolverPackage )
import Distribution.Solver.Types.SourcePackage ( SourcePackage )
import Distribution.Solver.Types.SummarizedMessage
    ( SummarizedMessage(..) )
import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import Distribution.Package ( PackageName )
import Distribution.Compiler ( CompilerInfo )
import Distribution.System ( Platform )
import Distribution.Types.VersionRange ( VersionRange, anyVersion )
import Distribution.Pretty ( prettyShow )
import Distribution.Version ( simplifyVersionRange )
import Distribution.Utils.Generic ( ordNub )

data ResolverParams loc =
  ResolverParams
    { rpPlatform :: Platform
    , rpCompilerInfo :: CompilerInfo
    , rpInstalledPackages :: InstalledPackageIndex
    , rpSourcePackages :: PackageIndex (SourcePackage loc)
    , rpPkgConfigDb :: Maybe PkgConfigDb
    , rpPkgPrefs :: PackagesPreferenceSource
    , rpPkgConstraints :: [LabeledPackageConstraint]
    , rpPkgNames :: Set PackageName
    }
    deriving (Generic)

deriving instance Binary loc => Binary (ResolverParams loc)
deriving instance Structured loc => Structured (ResolverParams loc)
deriving instance Eq loc => Eq (ResolverParams loc)

type PackagesPreferenceSource =
    (Set PackageName, PackagesPreferenceDefault, [PackagePreference])

-- | Give an interpretation to the global 'PackagesPreference' as
--  specific per-package 'PackageVersionPreference'.
interpretPackagesPreference
  :: PackagesPreferenceSource
  -> (PackageName -> PackagePreferences)
interpretPackagesPreference (selected, defaultPref, prefs) =
  \pkgname ->
    PackagePreferences
      (versionPref pkgname)
      (installPref pkgname)
      (stanzasPref pkgname)
  where
    versionPref :: PackageName -> [VersionRange]
    versionPref pkgname =
      Map.findWithDefault [anyVersion] pkgname versionPrefs
    versionPrefs =
      Map.fromListWith
        (++)
        [ (pkgname, [pref])
        | PackageVersionPreference pkgname pref <- prefs
        ]

    installPref :: PackageName -> InstalledPreference
    installPref pkgname =
      Map.findWithDefault (installPrefDefault pkgname) pkgname installPrefs
    installPrefs =
      Map.fromList
        [ (pkgname, pref)
        | PackageInstalledPreference pkgname pref <- prefs
        ]
    installPrefDefault = case defaultPref of
      PreferAllLatest -> const Preference.PreferLatest
      PreferAllOldest -> const Preference.PreferOldest
      PreferAllInstalled -> const Preference.PreferInstalled
      PreferLatestForSelected -> \pkgname ->
        -- When you say cabal install foo, what you really mean is, prefer the
        -- latest version of foo, but the installed version of everything else
        if pkgname `Set.member` selected
          then Preference.PreferLatest
          else Preference.PreferInstalled

    stanzasPref :: PackageName -> [OptionalStanza]
    stanzasPref pkgname =
      Map.findWithDefault [] pkgname stanzasPrefs
    stanzasPrefs =
      Map.fromListWith
        (\a b -> ordNub (a ++ b))
        [ (pkgname, pref)
        | PackageStanzasPreference pkgname pref <- prefs
        ]

-- | Global policy for all packages to say if we prefer package versions that
-- are already installed locally or if we just prefer the latest available.
data PackagesPreferenceDefault
  = -- | Always prefer the latest version irrespective of any existing
    -- installed version.
    --
    -- * This is the standard policy for upgrade.
    -- * This is enabled by --prefer-version=latest for install
    PreferAllLatest
  | -- | Always prefer the oldest version irrespective of any existing
    -- installed version or packages explicitly requested.
    --
    -- * This is enabled by --prefer-version=oldest for install.
    PreferAllOldest
  | -- | Always prefer the installed versions over ones that would need to be
    -- installed. Secondarily, prefer latest versions (eg the latest installed
    -- version or if there are none then the latest source version).
    PreferAllInstalled
  | -- | Prefer the latest version for packages that are explicitly requested
    -- but prefers the installed version for any other packages.
    --
    -- * This is the standard policy for install.
    PreferLatestForSelected
  deriving (Show, Eq, Generic, Binary, Structured)

-- | A package selection preference for a particular package.
--
-- Preferences are soft constraints that the dependency resolver should try to
-- respect where possible. It is not specified if preferences on some packages
-- are more important than others.
data PackagePreference
  = -- | A suggested constraint on the version number.
    PackageVersionPreference PackageName VersionRange
  | -- | If we prefer versions of packages that are already installed.
    PackageInstalledPreference PackageName InstalledPreference
  | -- | If we would prefer to enable these optional stanzas
    -- (i.e. test suites and/or benchmarks)
    PackageStanzasPreference PackageName [OptionalStanza]
  deriving (Show, Eq, Generic, Binary, Structured)

-- | Provide a textual representation of a package preference
-- for debugging purposes.
showPackagePreference :: PackagePreference -> String
showPackagePreference (PackageVersionPreference pn vr) =
  prettyShow pn ++ " " ++ prettyShow (simplifyVersionRange vr)
showPackagePreference (PackageInstalledPreference pn ip) =
  prettyShow pn ++ " " ++ show ip
showPackagePreference (PackageStanzasPreference pn st) =
  prettyShow pn ++ " " ++ show st


-- | A dependency resolver consists of a function that works out an
-- installation plan given the set of installed and available packages and a
-- set of deps to solve for, together with a pair of resolver-specific input
-- and output types, and a pair of conversion that translate between these
-- resolver-specific types and the resolver-independent 'ResolverParams' type.
--
-- The reason for this interface is because there are dozens of approaches to
-- solving the package dependency problem and we want to make it easy to swap
-- in alternatives.
--
-- Both the input and the output type must be binary-serializable, because we
-- need to use them as keys for re-run checks.
class {- (Binary (ResolverInputs a), Binary (ResolverOutput a))
        => -} DependencyResolver a where
  type ResolverInputs a :: Type
  type ResolverOutput a :: Type
  runResolver :: a -> ResolverInputs a -> Progress SummarizedMessage String (ResolverOutput a)
  convInputs :: a -> ResolverParams loc -> ResolverInputs a
  convOutput :: a -> ResolverParams loc -> ResolverOutput a -> [ResolverPackage loc]

