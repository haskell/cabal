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
    -- * Utilities
    , getPackages            -- :: [ResolvedPackage] -> [(PackageIdentifier,[String],String)]
    , getBuildDeps           -- :: [PackageIdentifier] -> [ResolvedPackage] -> [ResolvedPackage]
    , filterFetchables       -- :: [ResolvedPackage] -> [(PackageIdentifier,String)]
    , fulfillDependency      -- :: Dependency -> PackageIdentifier -> Bool
    ) where

import Distribution.Version (Dependency(..), withinRange)
import Distribution.Package (PackageIdentifier(..))
import Distribution.ParseUtils (showDependency)

import Network.Hackage.Interface
import Data.List (nub, maximumBy)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

import Network.Hackage.CabalInstall.Config (getKnownPackages)
import Network.Hackage.CabalInstall.Types ( ResolvedPackage(..), UnresolvedDependency(..)
                                      , ConfigFlags (..), PkgInfo (..))
import Text.Printf (printf)


-- |Flattens a list of dependencies, filtering out installed packages.
--  Packages dependencies are placed before the packages and duplicate entries
--  are removed.
flattenDepList :: [PackageIdentifier] -- ^List of installed packages.
               -> [ResolvedPackage] -- ^List of resolved packages.
               -> [ResolvedPackage]
flattenDepList ps deps
    = nub $ worker deps
    where isBeingInstalled dep
              = not . null $ flip mapMaybe deps $ \rpkg -> do (pkg,_,_) <- resolvedData rpkg
                                                              guard (fulfillDependency dep pkg)
          worker [] = []
          worker (pkgInfo:xs)
              = case getLatestPkg ps (fulfilling pkgInfo) of
                  Just _pkg -> worker xs
                  Nothing
                      -> case resolvedData pkgInfo of
                           Just (_pkg,_location,subDeps)
                               -> worker (filter (not.isBeingInstalled.fulfilling) subDeps) ++ pkgInfo:worker xs
                           Nothing
                               -> pkgInfo:worker xs

-- |Flattens a dependency list while only keeping the dependencies of the packages.
--  This is used for installing all the dependencies of a package but not the package itself.
getBuildDeps :: [PackageIdentifier] -> [ResolvedPackage]
             -> [ResolvedPackage]
getBuildDeps ps deps
    = nub $ concatMap worker deps
    where worker pkgInfo
              = case getLatestPkg ps (fulfilling pkgInfo) of
                  Just _pkg -> []
                  Nothing -> case resolvedData pkgInfo of
                               Just (_pkg,_location,subDeps)
                                        -> flattenDepList ps subDeps
                               Nothing -> []

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


-- |Locates a @PackageIdentifier@ which satisfies a given @Dependency@.
--  Fails with "cannot satisfy dependency: %s." where %s == the given dependency.
getLatestPkg :: (Monad m) => [PackageIdentifier] -> Dependency -> m PackageIdentifier
getLatestPkg ps dep
    = case filter (fulfillDependency dep) ps of
        [] -> fail $ printf "cannot satisfy dependency: %s." (show (showDependency dep))
        qs -> let pkg = maximumBy versions qs
                  versions a b = pkgVersion a `compare` pkgVersion b
              in return pkg

-- |Evaluates to @True@ if the given @Dependency@ is satisfied by the given @PackageIdentifer@.
fulfillDependency :: Dependency -> PackageIdentifier -> Bool
fulfillDependency (Dependency depName vrange) pkg
    = pkgName pkg == depName && pkgVersion pkg `withinRange` vrange

getDependency :: [PkgInfo]
              -> UnresolvedDependency -> ResolvedPackage
getDependency ps (UnresolvedDependency { dependency=dep@(Dependency pkgname vrange)
                                       , depOptions=opts})
    = case filter ok ps of
        [] -> ResolvedPackage
              { fulfilling = dep
              , resolvedData = Nothing
              , pkgOptions = opts }
        qs -> let PkgInfo { infoId = pkg, infoDeps = deps, infoURL = location } = maximumBy versions qs
                  versions a b = pkgVersion (infoId a) `compare` pkgVersion (infoId b)
              in ResolvedPackage
                 { fulfilling = dep
                 , resolvedData = Just ( pkg
                                       , location
                                       , (map (getDependency ps) (map depToUnresolvedDep deps)))
                 , pkgOptions = opts }
    where ok PkgInfo{ infoId = p } = pkgName p == pkgname && pkgVersion p `withinRange` vrange

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
         let resolved = map (resolve knownPkgs) (filter isNotInstalled deps)
         return resolved
    where isNotInstalled pkgDep = not (or (map (fulfillDependency (dependency pkgDep)) ps))
          resolve pkgs dep
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
