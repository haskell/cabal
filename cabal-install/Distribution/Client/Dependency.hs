-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Top level interface to dependency resolution.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency (
    module Distribution.Client.Dependency.Types,
    resolveDependencies,
    resolveDependenciesWithProgress,

    dependencyConstraints,
    dependencyTargets,

    PackagesPreference(..),
    PackagesPreferenceDefault(..),
    PackagePreference(..),

    upgradableDependencies,
  ) where

import Distribution.Client.Dependency.Bogus (bogusResolver)
import Distribution.Client.Dependency.TopDown (topDownResolver)
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Client.PackageIndex (PackageIndex)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.Types
         ( UnresolvedDependency(..), AvailablePackage(..), InstalledPackage )
import Distribution.Client.Dependency.Types
         ( DependencyResolver, PackageConstraint(..)
         , PackagePreferences(..), InstalledPreference(..)
         , Progress(..), foldProgress )
import Distribution.Package
         ( PackageIdentifier(..), PackageName(..), packageVersion, packageName
         , Dependency(..), Package(..), PackageFixedDeps(..) )
import Distribution.Version
         ( VersionRange, anyVersion, orLaterVersion, isAnyVersion )
import Distribution.Compiler
         ( CompilerId(..) )
import Distribution.System
         ( Platform )
import Distribution.Simple.Utils (comparing)
import Distribution.Client.Utils (mergeBy, MergeResult(..))

import Data.List (maximumBy)
import Data.Monoid (Monoid(mempty))
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Exception (assert)

defaultResolver :: DependencyResolver
defaultResolver = topDownResolver

-- | Global policy for the versions of all packages.
--
data PackagesPreference = PackagesPreference
       PackagesPreferenceDefault
       [PackagePreference]

dependencyConstraints :: [UnresolvedDependency] -> [PackageConstraint]
dependencyConstraints deps =
     [ PackageVersionConstraint name versionRange
     | UnresolvedDependency (Dependency name versionRange) _ <- deps
     , not (isAnyVersion versionRange) ]

  ++ [ PackageFlagsConstraint name flags
     | UnresolvedDependency (Dependency name _) flags <- deps
     , not (null flags) ]

dependencyTargets :: [UnresolvedDependency] -> [PackageName]
dependencyTargets deps =
  [ name | UnresolvedDependency (Dependency name _) _ <- deps ]

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
     -- version or if there are none then the latest available version).
   | PreferAllInstalled

     -- | Prefer the latest version for packages that are explicitly requested
     -- but prefers the installed version for any other packages.
     --
     -- * This is the standard policy for install.
     --
   | PreferLatestForSelected

data PackagePreference
   = PackageVersionPreference   PackageName VersionRange
   | PackageInstalledPreference PackageName InstalledPreference

resolveDependencies :: Platform
                    -> CompilerId
                    -> Maybe (PackageIndex InstalledPackage)
                    -> PackageIndex AvailablePackage
                    -> PackagesPreference
                    -> [PackageConstraint]
                    -> [PackageName]
                    -> Either String InstallPlan
resolveDependencies platform comp installed available
                    preferences constraints targets =
  foldProgress (flip const) Left Right $
    resolveDependenciesWithProgress platform comp installed available
                                    preferences constraints targets

resolveDependenciesWithProgress :: Platform
                                -> CompilerId
                                -> Maybe (PackageIndex InstalledPackage)
                                -> PackageIndex AvailablePackage
                                -> PackagesPreference
                                -> [PackageConstraint]
                                -> [PackageName]
                                -> Progress String String InstallPlan
resolveDependenciesWithProgress platform comp (Just installed) =
  dependencyResolver defaultResolver platform comp installed

resolveDependenciesWithProgress platform comp Nothing =
  dependencyResolver bogusResolver platform comp mempty

hideBrokenPackages :: PackageFixedDeps p => PackageIndex p -> PackageIndex p
hideBrokenPackages index =
    check (null . PackageIndex.brokenPackages)
  . foldr (PackageIndex.deletePackageId . packageId) index
  . PackageIndex.reverseDependencyClosure index
  . map (packageId . fst)
  $ PackageIndex.brokenPackages index
  where
    check p x = assert (p x) x

dependencyResolver
  :: DependencyResolver
  -> Platform -> CompilerId
  -> PackageIndex InstalledPackage
  -> PackageIndex AvailablePackage
  -> PackagesPreference
  -> [PackageConstraint]
  -> [PackageName]
  -> Progress String String InstallPlan
dependencyResolver resolver platform comp installed available
                            pref constraints targets =
  let installed' = hideBrokenPackages installed
      -- If the user is not explicitly asking to upgrade base then lets
      -- prevent that from happening accidentally since it is usually not what
      -- you want and it probably does not work anyway. We do it by adding a
      -- constraint to only pick an installed version of base and ghc-prim.
      extraConstraints =
        [ PackageInstalledConstraint pkgname
        | all (/=PackageName "base") targets
        , pkgname <-  [ PackageName "base", PackageName "ghc-prim" ]
        , not (null (PackageIndex.lookupPackageName installed pkgname)) ]
      preferences = interpretPackagesPreference (Set.fromList targets) pref
   in fmap toPlan
    $ resolver platform comp installed' available
               preferences (extraConstraints ++ constraints) targets

  where
    toPlan pkgs =
      case InstallPlan.new platform comp (PackageIndex.fromList pkgs) of
        Right plan     -> plan
        Left  problems -> error $ unlines $
            "internal error: could not construct a valid install plan."
          : "The proposed (invalid) plan contained the following problems:"
          : map InstallPlan.showPlanProblem problems

-- | Give an interpretation to the global 'PackagesPreference' as
--  specific per-package 'PackageVersionPreference'.
--
interpretPackagesPreference :: Set PackageName
                            -> PackagesPreference
                            -> (PackageName -> PackagePreferences)
interpretPackagesPreference selected (PackagesPreference defaultPref prefs) =
  \pkgname -> PackagePreferences (versionPref pkgname) (installPref pkgname)

  where
    versionPref pkgname =
      fromMaybe anyVersion (Map.lookup pkgname versionPrefs)
    versionPrefs = Map.fromList
      [ (pkgname, pref)
      | PackageVersionPreference pkgname pref <- prefs ]

    installPref pkgname =
      fromMaybe (installPrefDefault pkgname) (Map.lookup pkgname installPrefs)
    installPrefs = Map.fromList
      [ (pkgname, pref)
      | PackageInstalledPreference pkgname pref <- prefs ]
    installPrefDefault = case defaultPref of
      PreferAllLatest         -> \_       -> PreferLatest
      PreferAllInstalled      -> \_       -> PreferInstalled
      PreferLatestForSelected -> \pkgname ->
        -- When you say cabal install foo, what you really mean is, prefer the
        -- latest version of foo, but the installed version of everything else
        if pkgname `Set.member` selected then PreferLatest
                                         else PreferInstalled

-- | Given the list of installed packages and available packages, figure
-- out which packages can be upgraded.
--
upgradableDependencies :: PackageIndex InstalledPackage
                       -> PackageIndex AvailablePackage
                       -> [Dependency]
upgradableDependencies installed available =
  [ Dependency name (orLaterVersion latestVersion)
    -- This is really quick (linear time). The trick is that we're doing a
    -- merge join of two tables. We can do it as a merge because they're in
    -- a comparable order because we're getting them from the package indexs.
  | InBoth latestInstalled allAvailable
      <- mergeBy (\a (b:_) -> packageName a `compare` packageName b)
                 [ maximumBy (comparing packageVersion) pkgs
                 | pkgs <- PackageIndex.allPackagesByName installed ]
                 (PackageIndex.allPackagesByName available)
  , let (PackageIdentifier name latestVersion) = packageId latestInstalled
  , any (\p -> packageVersion p > latestVersion) allAvailable ]
