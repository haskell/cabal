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
    , upgradableDependencies
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
         ( DependencyResolver, Progress(..), foldProgress )
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
import Control.Exception (assert)

defaultResolver :: DependencyResolver a
defaultResolver = naiveResolver
--for the brave: try the new topDownResolver, but only with --dry-run !!!

resolveDependencies :: OS
                    -> Arch
                    -> CompilerId
                    -> Maybe (PackageIndex InstalledPackageInfo)
                    -> PackageIndex AvailablePackage
                    -> [UnresolvedDependency]
                    -> Either String (InstallPlan a)
resolveDependencies os arch comp (Just installed) available deps =
  foldProgress (flip const) Left Right $
  dependencyResolver defaultResolver
    os arch comp installed available deps

resolveDependencies os arch comp Nothing available deps =
  foldProgress (flip const) Left Right $
  dependencyResolver bogusResolver
    os arch comp mempty available deps

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
  -> [UnresolvedDependency]
  -> Progress String String (InstallPlan a)
dependencyResolver resolver os arch comp installed available =
  let installed' = hideBrokenPackages installed
      available' = hideBasePackage available
   in fmap toPlan . resolver os arch comp installed' available'

  where
    toPlan pkgs =
      case InstallPlan.new os arch comp (PackageIndex.fromList pkgs) of
        Right plan     -> plan
        Left  problems -> error $ unlines $
            "internal error: could not construct a valid install plan."
          : "The proposed (invalid) plan contained the following problems:"
          : map InstallPlan.showPlanProblem problems

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
