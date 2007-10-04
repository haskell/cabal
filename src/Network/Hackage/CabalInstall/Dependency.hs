-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Dependency
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Various kinds of dependency resolution and utilities.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Dependency
    (
    -- * Dependency resolution
      resolveDependencies
    , resolveDependenciesAux 
    -- * Installed packages
    , listInstalledPackages
    -- * Utilities
    , depToUnresolvedDep
    , getPackages            -- :: [ResolvedPackage] -> [(PackageIdentifier,[String],String)]
    , getBuildDeps           -- :: [PackageIdentifier] -> [ResolvedPackage] -> [ResolvedPackage]
    , filterFetchables       -- :: [ResolvedPackage] -> [(PackageIdentifier,String)]
    , fulfillDependency      -- :: Dependency -> PackageIdentifier -> Bool
    ) where

import Distribution.Version (Dependency(..), withinRange)
import Distribution.Package (PackageIdentifier(..))
import Distribution.ParseUtils (showDependency)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Compiler  (PackageDB(..))

import Data.List (nub, maximumBy)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

import Network.Hackage.CabalInstall.Config (getKnownPackages)
import Network.Hackage.CabalInstall.Types ( ResolvedPackage(..), UnresolvedDependency(..)
                                      , ConfigFlags (..), PkgInfo (..), ResolvedDependency(..))
import Text.Printf (printf)


-- |Flattens a list of dependencies, filtering out installed packages.
--  Packages dependencies are placed before the packages and duplicate entries
--  are removed.
flattenDepList :: [PackageIdentifier] -- ^List of installed packages.
               -> [ResolvedPackage] -- ^List of resolved packages.
               -> [ResolvedPackage]
flattenDepList ps
    = nub . filter (not . isInstalled ps . fulfilling) . concatMap flatten
    where flatten pkgInfo = getBuildDeps ps [pkgInfo] ++ [pkgInfo]

-- | Flattens a dependency list, keeping only the transitive closure of the 
--   dependencies of the top-level packages.
--   This is used for installing all the dependencies of set of packages but not the packages
--   themselves. Filters out installed packages and duplicates.
getBuildDeps :: [PackageIdentifier] -> [ResolvedPackage]
             -> [ResolvedPackage]
getBuildDeps ps
    = nub . filter (not . isInstalled ps . fulfilling) . concatMap flattenDeps
    where flattenDeps pkgInfo 
              = case resolvedData pkgInfo of
                  Just (_,_,subDeps) -> flattenDepList ps subDeps
                  Nothing            -> []

{-
getReverseDeps :: [PackageIdentifier] -- All installed packages.
               -> [(PackageIdentifier,[Dependency],String)] -- Known packages.
               -> [(PackageIdentifier,[Dependency],String)] -- Resolved and installed packages.
               -> [(PackageIdentifier,[String],String)] -- Packages to be installed.
               -> [(PackageIdentifier,[String],String)]
getReverseDeps ps knownPkgs ipkgs toBeInstalled
    = nub $ concatMap resolve $ filter depends ipkgs
    where depends (_pkg,deps,_location)
              = or (map (\dep -> or (map (\(p,_,_) -> fulfillDependency dep p) toBeInstalled)) deps)
          resolve (pkg,deps,location)
              = let resolveDep dep
                        = case find (\(p,_,_) -> fulfillDependency dep p) knownPkgs of
                            Just (pkg,_,location) -> Just (pkg,[],location)
                            Nothing
                                | pkg `elem` ps -> Nothing
                                | otherwise     -> error "Urk!"
                in mapMaybe resolveDep deps ++ [(pkg,[],location)]

-- |Find the dependencies and location for installed packages.
--  Packages not located on a Hackage server will be filtered out.
filterInstalledPkgs :: [PackageIdentifier] -> [(PackageIdentifier,[Dependency],String)]
                    -> [(PackageIdentifier,[Dependency],String)]
filterInstalledPkgs ipkgs knownPkgs
    = filter worker knownPkgs
    where worker (pkg,_deps,_location)
              = pkg `elem` ipkgs
-}

depToUnresolvedDep :: Dependency -> UnresolvedDependency
depToUnresolvedDep dep
    = UnresolvedDependency
      { dependency = dep
      , depOptions = [] }

resolvedDepToResolvedPkg :: (Dependency,Maybe ResolvedDependency) -> ResolvedPackage
resolvedDepToResolvedPkg (dep,rDep)
    = ResolvedPackage
      { fulfilling = dep
      , resolvedData = rData
      , pkgOptions = [] }
    where rData = do ResolvedDependency pkg location subDeps <- rDep
                     return ( pkg
                            , location
                            , map resolvedDepToResolvedPkg subDeps )


-- |Locates a @PkgInfo@ which satisfies a given @Dependency@.
--  Fails with "cannot satisfy dependency: %s." where %s == the given dependency.
getLatestPkg :: (Monad m) => [PkgInfo] -> Dependency -> m PkgInfo
getLatestPkg ps dep
    = case filter (fulfillDependency dep . infoId) ps of
        [] -> fail $ printf "cannot satisfy dependency: %s." (show (showDependency dep))
        qs -> return $ maximumBy compareVersions qs
  where compareVersions a b = pkgVersion (infoId a) `compare` pkgVersion (infoId b)

-- |Evaluates to @True@ if the given @Dependency@ is satisfied by the given @PackageIdentifer@.
fulfillDependency :: Dependency -> PackageIdentifier -> Bool
fulfillDependency (Dependency depName vrange) pkg
    = pkgName pkg == depName && pkgVersion pkg `withinRange` vrange

-- | Checks whether there is an installed package that satisfies the
--   given dependency.
isInstalled :: [PackageIdentifier] -- ^Installed packages.
            -> Dependency -> Bool
isInstalled ps dep = any (fulfillDependency dep) ps


getDependency :: [PkgInfo]
              -> UnresolvedDependency -> ResolvedPackage
getDependency ps (UnresolvedDependency { dependency=dep, depOptions=opts})
    = ResolvedPackage { fulfilling = dep
                      , resolvedData = fmap pkgData (getLatestPkg ps dep)
                      , pkgOptions = opts }
    where pkgData p = (infoId p
                      , infoURL p
                      , map (getDependency ps . depToUnresolvedDep) (infoDeps p))

-- |Get the PackageIdentifier, build options and location from a list of resolved packages.
--  Throws an exception if a package couldn't be resolved.
getPackages :: [ResolvedPackage] -> [(PackageIdentifier,[String],String)]
getPackages = map worker
    where worker dep
              = case resolvedData dep of
                  Nothing
                      -> error $ printf "Couldn't satisfy dependency: '%s'." (show $ showDependency (fulfilling dep))
                  Just (pkg,location,_)
                      -> (pkg,pkgOptions dep,location)

-- |List all packages which can be fetched.
filterFetchables :: [ResolvedPackage] -> [(PackageIdentifier,String)]
filterFetchables = mapMaybe worker
    where worker dep = do (pkg,location,_) <- resolvedData dep
                          return (pkg,location)


-- |Resolve some dependencies from the known packages while filtering out installed packages.
--  The result hasn't been modified to put the dependencies in front of the packages.
resolveDependenciesAux :: ConfigFlags
                       -> [PackageIdentifier] -- ^Installed packages.
                       -> [UnresolvedDependency] -- ^Dependencies in need of resolution.
                       -> IO [ResolvedPackage]
resolveDependenciesAux cfg ps deps
    = do knownPkgs <- getKnownPackages cfg
         let resolved = map (resolve knownPkgs) (filter (not . isInstalled ps . dependency) deps)
         return resolved
    where resolve pkgs dep
              = let rDep = getDependency pkgs dep
                in case resolvedData rDep of
                    Nothing -> resolvedDepToResolvedPkg (dependency dep,Nothing)
                    _ -> rDep

-- |Resolve some dependencies from the known packages while filtering out installed packages.
--  The result has been modified to put the dependencies in front of the packages.
resolveDependencies :: ConfigFlags
                    -> [PackageIdentifier] -- ^Installed packages.
                    -> [UnresolvedDependency] -- ^Dependencies in need of resolution.
                    -> IO [ResolvedPackage]
resolveDependencies cfg ps deps
    = fmap (flattenDepList ps) (resolveDependenciesAux cfg ps deps)


listInstalledPackages :: ConfigFlags -> IO [PackageIdentifier]
listInstalledPackages cfg =
    do Just ipkgs <- getInstalledPackages
                         (configVerbose cfg) (configCompiler cfg)
                         (if configUserIns cfg then UserPackageDB
                                               else GlobalPackageDB)
                         (configPrograms cfg)
       return ipkgs
