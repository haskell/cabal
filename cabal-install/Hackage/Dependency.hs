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
    ) where

import Hackage.Config (listInstalledPackages)
import Hackage.Index (getKnownPackages)
import Hackage.Types 
    (ResolvedPackage(..), UnresolvedDependency(..), ConfigFlags (..),
     PkgInfo (..), FlagAssignment)

import Distribution.Version (Dependency(..), withinRange)
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription 
    (PackageDescription(buildDepends)
    , GenericPackageDescription
    , finalizePackageDescription)
import Distribution.Simple.Compiler (Compiler, showCompilerId, compilerVersion)
import Distribution.Simple.Program (ProgramConfiguration)
import qualified Distribution.Simple.Setup as Cabal

import Control.Monad (mplus)
import Data.List (nub, nubBy, maximumBy)
import Data.Maybe (fromMaybe)
import qualified System.Info (arch,os)


resolveDependencies :: ConfigFlags
                    -> Compiler
                    -> ProgramConfiguration
                    -> [UnresolvedDependency]
                    -> IO [ResolvedPackage]
resolveDependencies cfg comp conf deps 
    = do installed <- listInstalledPackages cfg comp conf
         available <- getKnownPackages cfg
         return [resolveDependency comp installed available dep flags
                     | UnresolvedDependency dep flags <- deps]

-- | Resolve dependencies of a local package description. This is used
-- when the top-level package does not come from hackage.
resolveDependenciesLocal :: ConfigFlags
                         -> Compiler
                         -> ProgramConfiguration
                         -> GenericPackageDescription
                         -> FlagAssignment
                         -> IO [ResolvedPackage]
resolveDependenciesLocal cfg comp conf desc flags
    =  do installed <- listInstalledPackages cfg comp conf
          available <- getKnownPackages cfg
          return [resolveDependency comp installed available dep []
                     | dep <- getDependencies comp installed available desc flags]

resolveDependency :: Compiler
                  -> [PackageIdentifier] -- ^ Installed packages.
                  -> [PkgInfo] -- ^ Installable packages
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
latestInstalledSatisfying = latestSatisfying id

-- | Gets the latest available package satisfying a dependency.
latestAvailableSatisfying :: [PkgInfo] 
                          -> Dependency -> Maybe PkgInfo
latestAvailableSatisfying = latestSatisfying pkgInfoId

latestSatisfying :: (a -> PackageIdentifier) 
                 -> [a]
                 -> Dependency
                 -> Maybe a
latestSatisfying f xs dep =
    case filter ((`satisfies` dep) . f) xs of
      [] -> Nothing
      ys -> Just $ maximumBy (comparing (pkgVersion . f)) ys
  where comparing g a b = g a `compare` g b

-- | Checks if a package satisfies a dependency.
satisfies :: PackageIdentifier -> Dependency -> Bool
satisfies pkg (Dependency depName vrange)
    = pkgName pkg == depName && pkgVersion pkg `withinRange` vrange

-- | Gets the dependencies of an available package.
getDependencies :: Compiler 
                -> [PackageIdentifier] -- ^ Installed packages.
                -> [PkgInfo] -- ^ Available packages
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
                (Just $ nub $ installed ++ map pkgInfoId available) 
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

