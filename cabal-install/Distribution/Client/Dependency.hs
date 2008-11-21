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
    resolveDependencies,
    resolveDependenciesWithProgress,

    PackagesPreference(..),
    packagesPreference,
    PackagesVersionPreference,
    PackagesInstalledPreference(..),

    upgradableDependencies,
  ) where

import Distribution.Client.Dependency.Bogus (bogusResolver)
import Distribution.Client.Dependency.TopDown (topDownResolver)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.Types
         ( UnresolvedDependency(..), AvailablePackage(..) )
import Distribution.Client.Dependency.Types
         ( PackageName, DependencyResolver
         , PackagePreference(..), PackageInstalledPreference(..)
         , Progress(..), foldProgress )
import Distribution.Package
         ( PackageIdentifier(..), PackageName(..), packageVersion, packageName
         , Dependency(..), Package(..), PackageFixedDeps(..) )
import Distribution.Version
         ( VersionRange(AnyVersion), orLaterVersion )
import Distribution.Compiler
         ( CompilerId(..), CompilerFlavor(LHC) )
import Distribution.System
         ( OS, Arch )
import Distribution.Simple.Utils (comparing)
import Distribution.Client.Utils (mergeBy, MergeResult(..))

import Data.List (maximumBy)
import Data.Monoid (Monoid(mempty))
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Exception (assert)

defaultResolver :: DependencyResolver
defaultResolver = topDownResolver

-- | Global policy for the versions of all packages.
--
data PackagesPreference = PackagesPreference
       PackagesInstalledPreference
       PackagesVersionPreference

packagesPreference :: PackagesInstalledPreference
                   -> Map PackageName VersionRange
                   -> PackagesPreference
packagesPreference installedPref versionPrefs =
  PackagesPreference installedPref versionPrefs'
  where
    versionPrefs' :: PackageName -> VersionRange
    versionPrefs' pkgname =
      fromMaybe AnyVersion (Map.lookup pkgname versionPrefs)

-- | An optional suggested version for each package.
--
type PackagesVersionPreference = PackageName -> VersionRange

-- | Global policy for all packages to say if we prefer package versions that
-- are already installed locally or if we just prefer the latest available.
--
data PackagesInstalledPreference =

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

resolveDependencies :: OS
                    -> Arch
                    -> CompilerId
                    -> Maybe (PackageIndex InstalledPackageInfo)
                    -> PackageIndex AvailablePackage
                    -> PackagesPreference
                    -> [UnresolvedDependency]
                    -> Either String InstallPlan
resolveDependencies os arch comp installed available pref deps =
  foldProgress (flip const) Left Right $
    resolveDependenciesWithProgress os arch comp installed available pref deps

resolveDependenciesWithProgress :: OS
                                -> Arch
                                -> CompilerId
                                -> Maybe (PackageIndex InstalledPackageInfo)
                                -> PackageIndex AvailablePackage
                                -> PackagesPreference
                                -> [UnresolvedDependency]
                                -> Progress String String InstallPlan
resolveDependenciesWithProgress os arch comp (Just installed) =
  dependencyResolver defaultResolver os arch comp installed

resolveDependenciesWithProgress os arch comp Nothing =
  dependencyResolver bogusResolver os arch comp mempty

hideBrokenPackages :: PackageFixedDeps p => PackageIndex p -> PackageIndex p
hideBrokenPackages index =
    check (null . PackageIndex.brokenPackages)
  . foldr (PackageIndex.deletePackageId . packageId) index
  . PackageIndex.reverseDependencyClosure index
  . map (packageId . fst)
  $ PackageIndex.brokenPackages index
  where
    check p x = assert (p x) x

hideBasePackage :: Package p => PackageIndex p -> PackageIndex p
hideBasePackage = PackageIndex.deletePackageName (PackageName "base")
                . PackageIndex.deletePackageName (PackageName "ghc-prim")

dependencyResolver
  :: DependencyResolver
  -> OS -> Arch -> CompilerId
  -> PackageIndex InstalledPackageInfo
  -> PackageIndex AvailablePackage
  -> PackagesPreference
  -> [UnresolvedDependency]
  -> Progress String String InstallPlan
dependencyResolver resolver os arch comp installed available pref deps =
  let installed' = hideBrokenPackages installed
      available' = case comp of -- Ugly hack to support LHC.
                     CompilerId LHC _ -> available
                     _                -> hideBasePackage available
   in fmap toPlan
    $ resolver os arch comp installed' available' preference deps

  where
    toPlan pkgs =
      case InstallPlan.new os arch comp (PackageIndex.fromList pkgs) of
        Right plan     -> plan
        Left  problems -> error $ unlines $
            "internal error: could not construct a valid install plan."
          : "The proposed (invalid) plan contained the following problems:"
          : map InstallPlan.showPlanProblem problems

    preference = interpretPackagesPreference initialPkgNames pref
    initialPkgNames = Set.fromList
      [ name | UnresolvedDependency (Dependency name _) _ <- deps ]

-- | Give an interpretation to the global 'PackagesPreference' as
--  specific per-package 'PackageVersionPreference'.
--
interpretPackagesPreference :: Set PackageName
                            -> PackagesPreference
                            -> (PackageName -> PackagePreference)
interpretPackagesPreference selected
  (PackagesPreference installPref versionPref) = case installPref of
    PreferAllLatest    -> PackagePreference PreferLatest    . versionPref
    PreferAllInstalled -> PackagePreference PreferInstalled . versionPref
    PreferLatestForSelected -> \pkgname ->
      -- When you say cabal install foo, what you really mean is, prefer the
      -- latest version of foo, but the installed version of everything else:
      if pkgname `Set.member` selected
        then PackagePreference PreferLatest    (versionPref pkgname)
        else PackagePreference PreferInstalled (versionPref pkgname)

-- | Given the list of installed packages and available packages, figure
-- out which packages can be upgraded.
--
upgradableDependencies :: PackageIndex InstalledPackageInfo
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
