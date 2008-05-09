-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Dependency
-- Copyright   :  (c) David Himmelstrup 2005, Bjorn Bringert 2007
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Various kinds of dependency resolution and utilities.
-----------------------------------------------------------------------------
module Hackage.Dependency
    (
      resolveDependencies
    , getUpgradableDeps
    ) where

import Hackage.Dependency.Naive (naiveResolver)
import Hackage.Dependency.Bogus (bogusResolver)
import Distribution.InstalledPackageInfo (InstalledPackageInfo_(package))
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.InstallPlan (InstallPlan)
import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..) )
import Distribution.Package
         ( PackageIdentifier(..), Dependency(..)
         , Package(..), PackageFixedDeps(..) )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( OS, Arch )
import Distribution.Simple.Utils (comparing)

import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(mempty))
import Control.Exception (assert)

resolveDependencies :: OS
                    -> Arch
                    -> CompilerId
                    -> Maybe (PackageIndex InstalledPackageInfo)
                    -> PackageIndex AvailablePackage
                    -> [UnresolvedDependency]
                    -> Either [Dependency] (InstallPlan a)
resolveDependencies os arch comp (Just installed) available deps =
  dependencyResolver naiveResolver
    os arch comp installed available deps

resolveDependencies os arch comp Nothing available deps =
  dependencyResolver bogusResolver
    os arch comp mempty available deps

hideBrokenPackages :: PackageFixedDeps p => PackageIndex p -> PackageIndex p
hideBrokenPackages index =
    check (null . PackageIndex.brokenPackages)
  . foldr (PackageIndex.delete . packageId) index
  . PackageIndex.reverseDependencyClosure index
  . map (packageId . fst)
  $ PackageIndex.brokenPackages index
  where
    check p x = assert (p x) x

type DependencyResolver a = OS
                         -> Arch
                         -> CompilerId
                         -> PackageIndex InstalledPackageInfo
                         -> PackageIndex AvailablePackage
                         -> [UnresolvedDependency]
                         -> Either [Dependency] [InstallPlan.PlanPackage a]

dependencyResolver
  :: DependencyResolver a
  -> OS -> Arch -> CompilerId
  -> PackageIndex InstalledPackageInfo
  -> PackageIndex AvailablePackage
  -> [UnresolvedDependency]
  -> Either [Dependency] (InstallPlan a)
dependencyResolver resolver os arch comp installed available deps =
  case resolver os arch comp (hideBrokenPackages installed) available deps of
    Left unresolved -> Left unresolved
    Right pkgs ->
      case InstallPlan.new os arch comp (PackageIndex.fromList pkgs) of
        Right plan     -> Right plan
        Left  problems -> error $ unlines $
            "internal error: could not construct a valid install plan."
          : "The proposed (invalid) plan contained the following problems:"
          : map InstallPlan.showPlanProblem problems

-- | This is an example resolver that says that every package failed.
--
failingResolver :: DependencyResolver a
failingResolver _ _ _ _ _ deps = Left
  [ dep | UnresolvedDependency dep _ <- deps ]

-- |Given the list of installed packages and installable packages, figure
-- out which packages can be upgraded.

getUpgradableDeps :: PackageIndex InstalledPackageInfo
                  -> PackageIndex AvailablePackage
                  -> [AvailablePackage]
getUpgradableDeps installed available =
  let latestInstalled = getLatestPackageVersions installed
      mNeedingUpgrade = map (flip newerAvailable available) latestInstalled
   in catMaybes mNeedingUpgrade

  where newerAvailable :: PackageIdentifier
                       -> PackageIndex AvailablePackage -- ^installable packages
                       -> Maybe AvailablePackage -- ^greatest available
        newerAvailable pkgToUpdate index
            = foldl (newerThan pkgToUpdate) Nothing (PackageIndex.allPackages index)
        newerThan :: PackageIdentifier 
                  -> Maybe AvailablePackage
                  -> AvailablePackage
                  -> Maybe AvailablePackage
        newerThan pkgToUpdate mFound testPkg
            = case (pkgName pkgToUpdate == (pkgName $ packageId testPkg), mFound) of
               (False, _) -> mFound
               (True, Nothing) -- compare to given package
                   -> if ((packageId testPkg) `isNewer` pkgToUpdate)
                      then Just testPkg
                      else Nothing -- none found so far
               (True, Just lastNewestPkg) -- compare to latest package
                   -> if ((packageId testPkg) `isNewer` (packageId lastNewestPkg))
                      then Just testPkg
                      else mFound

        -- trim out the old versions of packages with multiple versions installed
        isNewer :: PackageIdentifier -> PackageIdentifier -> Bool
        isNewer p1 p2 = pkgVersion p1 > pkgVersion p2


-- | Given the index of installed packages, get the latest version of each
-- package. That is, if multiple versions of this package are installed, figure
-- out which is the lastest one.
--
getLatestPackageVersions :: PackageIndex InstalledPackageInfo -> [PackageIdentifier]
getLatestPackageVersions index =
  [ maximumBy (comparing pkgVersion) $ map package pkgs
  | pkgs <- PackageIndex.allPackagesByName index ]
