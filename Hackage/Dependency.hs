-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Dependency
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
module Hackage.Dependency (
    resolveDependencies,
    resolveDependenciesWithProgress,
    PackagesVersionPreference(..),
    upgradableDependencies,
  ) where

import Hackage.Dependency.Naive (naiveResolver)
import Hackage.Dependency.Bogus (bogusResolver)
import Hackage.Dependency.TopDown (topDownResolver)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.InstallPlan (InstallPlan)
import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..) )
import Hackage.Dependency.Types
         ( PackageName, DependencyResolver, PackageVersionPreference(..)
         , Progress(..), foldProgress )
import Distribution.Package
         ( PackageIdentifier(..), packageVersion, packageName
         , Dependency(..), Package(..), PackageFixedDeps(..) )
import Distribution.Version
         ( VersionRange(LaterVersion) )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( OS, Arch )
import Distribution.Simple.Utils (comparing)
import Hackage.Utils (mergeBy, MergeResult(..))

import Data.List (maximumBy)
import Data.Monoid (Monoid(mempty))
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Exception (assert)

defaultResolver :: DependencyResolver a
defaultResolver = naiveResolver
--for the brave: try the new topDownResolver, but only with --dry-run !!!

-- | Global policy for the versions of all packages.
--
data PackagesVersionPreference =

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
                    -> PackagesVersionPreference
                    -> [UnresolvedDependency]
                    -> Either String (InstallPlan a)
resolveDependencies os arch comp installed available pref deps =
  foldProgress (flip const) Left Right $
    resolveDependenciesWithProgress os arch comp installed available pref deps

resolveDependenciesWithProgress :: OS
                                -> Arch
                                -> CompilerId
                                -> Maybe (PackageIndex InstalledPackageInfo)
                                -> PackageIndex AvailablePackage
                                -> PackagesVersionPreference
                                -> [UnresolvedDependency]
                                -> Progress String String (InstallPlan a)
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
hideBasePackage = PackageIndex.deletePackageName "base"
                . PackageIndex.deletePackageName "ghc-prim"

dependencyResolver
  :: DependencyResolver a
  -> OS -> Arch -> CompilerId
  -> PackageIndex InstalledPackageInfo
  -> PackageIndex AvailablePackage
  -> PackagesVersionPreference
  -> [UnresolvedDependency]
  -> Progress String String (InstallPlan a)
dependencyResolver resolver os arch comp installed available pref deps =
  let installed' = hideBrokenPackages installed
      available' = hideBasePackage available
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

    preference = interpretPackagesVersionPreference initialPkgNames pref
    initialPkgNames = Set.fromList
      [ name | UnresolvedDependency (Dependency name _) _ <- deps ]

-- | Give an interpretation to the global 'PackagesVersionPreference' as
--  specific per-package 'PackageVersionPreference'.
--
interpretPackagesVersionPreference :: Set PackageName
                                   -> PackagesVersionPreference
                                   -> (PackageName -> PackageVersionPreference)
interpretPackagesVersionPreference selected pref = case pref of
  PreferAllLatest         -> const PreferLatest
  PreferAllInstalled      -> const PreferInstalled
  PreferLatestForSelected -> \pkgname ->
    -- When you say cabal install foo, what you really mean is, prefer the
    -- latest version of foo, but the installed version of everything else:
    if pkgname `Set.member` selected
      then PreferLatest
      else PreferInstalled

-- | Given the list of installed packages and available packages, figure
-- out which packages can be upgraded.
--
upgradableDependencies :: PackageIndex InstalledPackageInfo
                       -> PackageIndex AvailablePackage
                       -> [Dependency]
upgradableDependencies installed available =
  [ Dependency name (LaterVersion latestVersion)
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
