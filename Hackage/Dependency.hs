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

import Hackage.Config (listInstalledPackages)
import qualified Hackage.Index as RepoIndex
import Hackage.Index (RepoIndex)
import Hackage.Types (ResolvedPackage(..), UnresolvedDependency(..),
                      PkgInfo(..), FlagAssignment, Repo)
import Hackage.Utils (comparing)
import Distribution.Version (Dependency(..), withinRange)
import Distribution.Verbosity (Verbosity)
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription 
    (PackageDescription(buildDepends)
    , GenericPackageDescription
    , finalizePackageDescription)
import Distribution.Simple.Compiler (Compiler, showCompilerId, compilerVersion,
                                     PackageDB)
import Distribution.Simple.Program (ProgramConfiguration)
import qualified Distribution.Simple.Setup as Cabal

import Control.Monad (mplus)
import Data.Monoid (Monoid(mconcat))
import Data.List (nub, nubBy, maximumBy, sort)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import qualified System.Info (arch,os)


resolveDependencies :: Verbosity
                    -> PackageDB -> [Repo]
                    -> Compiler
                    -> ProgramConfiguration
                    -> [UnresolvedDependency]
                    -> IO [ResolvedPackage]
resolveDependencies verbosity packageDB repos comp conf deps 
    = do installed <- listInstalledPackages verbosity packageDB comp conf
         available <- mconcat `fmap` mapM (RepoIndex.read verbosity) repos
         return [resolveDependency comp installed available dep flags
                     | UnresolvedDependency dep flags <- deps]

-- | Resolve dependencies of a local package description. This is used
-- when the top-level package does not come from hackage.
resolveDependenciesLocal :: Verbosity
                         -> PackageDB -> [Repo]
                         -> Compiler
                         -> ProgramConfiguration
                         -> GenericPackageDescription
                         -> FlagAssignment
                         -> IO [ResolvedPackage]
resolveDependenciesLocal verbosity packageDB repos comp conf desc flags
    =  do installed <- listInstalledPackages verbosity packageDB comp conf
          available <- mconcat `fmap` mapM (RepoIndex.read verbosity) repos
          return [resolveDependency comp installed available dep []
                     | dep <- getDependencies comp installed available desc flags]

resolveDependency :: Compiler
                  -> [PackageIdentifier] -- ^ Installed packages.
                  -> RepoIndex -- ^ Installable packages
                  -> Dependency
                  -> FlagAssignment
                  -> ResolvedPackage
resolveDependency comp installed available dep flags
    = fromMaybe (Unavailable dep) $ resolveFromInstalled `mplus` resolveFromAvailable
  where
    resolveFromInstalled = fmap (Installed dep) $ latestInstalledSatisfying installed dep
    resolveFromAvailable = 
        do pkg <- latestAvailableSatisfying available dep
           let deps = getDependencies comp installed available (pkgDesc pkg) flags
               resolved = map (\d -> resolveDependency comp installed available d []) deps
           return $ Available dep pkg flags resolved

-- | Gets the latest installed package satisfying a dependency.
latestInstalledSatisfying :: [PackageIdentifier] 
                          -> Dependency -> Maybe PackageIdentifier
latestInstalledSatisfying installed dep =
  case filter (`satisfies` dep) installed of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing pkgVersion) pkgs)

  where
    satisfies :: PackageIdentifier -> Dependency -> Bool
    satisfies pkg (Dependency depName vrange)
      = pkgName pkg == depName && pkgVersion pkg `withinRange` vrange


-- | Gets the latest available package satisfying a dependency.
latestAvailableSatisfying :: RepoIndex -> Dependency -> Maybe PkgInfo
latestAvailableSatisfying index dep =
  case RepoIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing (pkgVersion . pkgInfoId)) pkgs)

-- | Gets the dependencies of an available package.
getDependencies :: Compiler 
                -> [PackageIdentifier] -- ^ Installed packages.
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
                (Just $ nub $ installed
                           ++ map pkgInfoId (RepoIndex.allPackages available))
                System.Info.os
                System.Info.arch
                (showCompilerId comp, compilerVersion comp)
                pkg

packagesToInstall :: [ResolvedPackage] 
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
    flatten (Installed _ _) = []
    flatten (Available _ p opts deps) = concatMap flatten deps ++ [Right (p,opts)]
    flatten (Unavailable dep) = [Left dep]


-- |Given the list of installed packages and installable packages, figure
-- out which packages can be upgraded.

getUpgradableDeps :: Verbosity
                  -> PackageDB -> [Repo]
                  -> Compiler
                  -> ProgramConfiguration
                  -> IO [PkgInfo]
getUpgradableDeps verbosity packageDB repos comp conf
    = do allInstalled <- listInstalledPackages verbosity packageDB comp conf
         -- we should only consider the latest version of each package:
         let latestInstalled = getLatest allInstalled
         available <- mconcat `fmap` mapM (RepoIndex.read verbosity) repos
         let mNeedingUpgrade = map (\x -> newerAvailable x available)
                                   latestInstalled
         return $ catMaybes mNeedingUpgrade

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
        getLatest :: [PackageIdentifier]
                  -> [PackageIdentifier]
        getLatest pkgs = catMaybes [latestPkgVersion (pkgName pkg) pkgs | pkg <- pkgs]

        isNewer :: PackageIdentifier -> PackageIdentifier -> Bool
        isNewer p1 p2 = pkgVersion p1 > pkgVersion p2


-- |Given a package and the list of installed packages, get the latest
-- version of the given package.  That is, if multiple versions of
-- this package are installed, figure out which is the lastest one.
latestPkgVersion :: String -- ^Package name
                 -> [PackageIdentifier] -- installed packages
                 -> Maybe PackageIdentifier
latestPkgVersion name pkgs =
   let matches = [pkg | pkg <- pkgs, (pkgName pkg == name)] in
   listToMaybe $ reverse $ sort matches
