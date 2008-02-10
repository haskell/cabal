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
    , resolveDependenciesLocal
    , packagesToInstall
    , getUpgradableDeps
    ) where

import qualified Hackage.LocalIndex as LocalIndex
import Hackage.LocalIndex (LocalIndex)
import qualified Hackage.RepoIndex as RepoIndex
import Hackage.RepoIndex (RepoIndex)
import Hackage.Types (ResolvedDependency(..), UnresolvedDependency(..),
                      PkgInfo(..), FlagAssignment)
import Hackage.Utils (comparing, equating)
import Distribution.Version (Dependency(..))
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription 
    (PackageDescription(buildDepends)
    , GenericPackageDescription )
import Distribution.PackageDescription.Configuration
    ( finalizePackageDescription)
import Distribution.Simple.Compiler (Compiler, showCompilerId, compilerVersion)
import qualified Distribution.Simple.Setup as Cabal

import Control.Monad (mplus)
import Data.List (nub, nubBy, maximumBy, sortBy, groupBy)
import Data.Maybe (fromMaybe, catMaybes)
import qualified System.Info (arch,os)


resolveDependencies :: Compiler
                    -> LocalIndex
                    -> RepoIndex
                    -> [UnresolvedDependency]
                    -> [ResolvedDependency]
resolveDependencies comp installed available deps =
  [ resolveDependency comp installed available dep flags
  | UnresolvedDependency dep flags <- deps]

-- | Resolve dependencies of a local package description. This is used
-- when the top-level package does not come from hackage.
resolveDependenciesLocal :: Compiler
                         -> LocalIndex
                         -> RepoIndex
                         -> GenericPackageDescription
                         -> FlagAssignment
                         -> [ResolvedDependency]
resolveDependenciesLocal comp installed available desc flags =
  [ resolveDependency comp installed available dep flags
  | dep <- getDependencies comp installed available desc flags ]

resolveDependency :: Compiler
                  -> LocalIndex -- ^ Installed packages.
                  -> RepoIndex -- ^ Installable packages
                  -> Dependency
                  -> FlagAssignment
                  -> ResolvedDependency
resolveDependency comp installed available dep flags
    = fromMaybe (UnavailableDependency dep) $ resolveFromInstalled `mplus` resolveFromAvailable
  where
    resolveFromInstalled = fmap (InstalledDependency dep) $ latestInstalledSatisfying installed dep
    resolveFromAvailable = 
        do pkg <- latestAvailableSatisfying available dep
           let deps = getDependencies comp installed available (pkgDesc pkg) flags
               resolved = map (\d -> resolveDependency comp installed available d []) deps
           return $ AvailableDependency dep pkg flags resolved

-- | Gets the latest installed package satisfying a dependency.
latestInstalledSatisfying :: LocalIndex -> Dependency -> Maybe PackageIdentifier
latestInstalledSatisfying  index dep =
  case LocalIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing pkgVersion) pkgs)

-- | Gets the latest available package satisfying a dependency.
latestAvailableSatisfying :: RepoIndex -> Dependency -> Maybe PkgInfo
latestAvailableSatisfying index dep =
  case RepoIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing (pkgVersion . pkgInfoId)) pkgs)

-- | Gets the dependencies of an available package.
getDependencies :: Compiler 
                -> LocalIndex -- ^ Installed packages.
                -> RepoIndex -- ^ Available packages
                -> GenericPackageDescription
                -> FlagAssignment
                -> [Dependency] 
                   -- ^ If successful, this is the list of dependencies.
                   -- If flag assignment failed, this is the list of
                   -- missing dependencies.
getDependencies comp installed available pkg flags
    = case e of
        Left missing   -> missing
        Right (desc,_) -> buildDepends desc
    where 
      e = finalizePackageDescription 
                flags
                (Just $ nub $ LocalIndex.allPackages installed
                           ++ map pkgInfoId (RepoIndex.allPackages available))
                System.Info.os
                System.Info.arch
                (showCompilerId comp, compilerVersion comp)
                pkg

packagesToInstall :: [ResolvedDependency]
                  -> Either [Dependency] [(PkgInfo, FlagAssignment)]
                     -- ^ Either a list of missing dependencies, or a list
                     -- of packages to install, with their options.
packagesToInstall xs | null missing = Right toInstall
                     | otherwise = Left missing
  where 
    flattened = concatMap flatten xs
    missing = [d | Left d <- flattened]
    toInstall = nubBy samePackage [x | Right x <- flattened]
    samePackage a b = pkgInfoId (fst a) == pkgInfoId (fst b)
    flatten (InstalledDependency _ _) = []
    flatten (AvailableDependency _ p opts deps) = concatMap flatten deps ++ [Right (p,opts)]
    flatten (UnavailableDependency dep) = [Left dep]


-- |Given the list of installed packages and installable packages, figure
-- out which packages can be upgraded.

getUpgradableDeps :: LocalIndex
                  -> RepoIndex
                  -> [PkgInfo]
getUpgradableDeps installed available =
  let latestInstalled = getLatestPackageVersions installed
      mNeedingUpgrade = map (flip newerAvailable available) latestInstalled
   in catMaybes mNeedingUpgrade

  where newerAvailable :: PackageIdentifier
                       -> RepoIndex -- ^installable packages
                       -> Maybe PkgInfo -- ^greatest available
        newerAvailable pkgToUpdate index
            = foldl (newerThan pkgToUpdate) Nothing (RepoIndex.allPackages index)
        newerThan :: PackageIdentifier 
                  -> Maybe PkgInfo
                  -> PkgInfo
                  -> Maybe PkgInfo
        newerThan pkgToUpdate mFound testPkg
            = case (pkgName pkgToUpdate == (pkgName $ pkgInfoId testPkg), mFound) of
               (False, _) -> mFound
               (True, Nothing) -- compare to given package
                   -> if ((pkgInfoId testPkg) `isNewer` pkgToUpdate)
                      then Just testPkg
                      else Nothing -- none found so far
               (True, Just lastNewestPkg) -- compare to latest package
                   -> if ((pkgInfoId testPkg) `isNewer` (pkgInfoId lastNewestPkg))
                      then Just testPkg
                      else mFound

        -- trim out the old versions of packages with multiple versions installed
        isNewer :: PackageIdentifier -> PackageIdentifier -> Bool
        isNewer p1 p2 = pkgVersion p1 > pkgVersion p2


-- | Given the index of installed packages, get the latest version of each
-- package. That is, if multiple versions of this package are installed, figure
-- out which is the lastest one.
--
getLatestPackageVersions :: LocalIndex -> [PackageIdentifier]
getLatestPackageVersions index =
  [ maximumBy (comparing pkgVersion) pkgs
  | pkgGroup <- LocalIndex.allPackageGroups index
  -- Each pkgGroup may contain packages with names that are not equal
  -- case-insensitively, so we split into group by name case-sensitively:
  , pkgs <- (groupBy (equating pkgName) . sortBy (comparing pkgName)) pkgGroup ]
