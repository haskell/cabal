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
    , finalizePackage
    -- * Installed packages
    , listInstalledPackages
    -- * Utilities
    , depToUnresolvedDep
    , getPackages            -- :: [ResolvedPackage] -> [(PackageIdentifier,[String],String)]
    , getBuildDeps           -- :: [PackageIdentifier] -> [ResolvedPackage] -> [ResolvedPackage]
    , filterFetchables       -- :: [ResolvedPackage] -> [(PackageIdentifier,String)]
    , fulfillDependency      -- :: Dependency -> PackageIdentifier -> Bool
    ) where

import Distribution.Version (Version, Dependency(..), withinRange)
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription 
    (PackageDescription(package, buildDepends, pkgUrl, synopsis)
    , GenericPackageDescription(packageDescription)
    , finalizePackageDescription)
import Distribution.ParseUtils (showDependency)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Compiler  (PackageDB(..), showCompilerId, compilerVersion)

import Data.Char (toLower)
import Data.List (nub, maximumBy, isPrefixOf)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import qualified System.Info (arch,os)

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
getLatestPkg :: (Monad m) => [GenericPackageDescription] -> Dependency -> m GenericPackageDescription
getLatestPkg ps dep
    = case filter (fulfillDependency dep . pkdId) ps of
        [] -> fail $ printf "cannot satisfy dependency: %s." (show (showDependency dep))
        qs -> return $ maximumBy compareVersions qs
  where compareVersions a b = pkgVersion (pkdId a) `compare` pkgVersion (pkdId b)
        pkdId = package . packageDescription

-- |Evaluates to @True@ if the given @Dependency@ is satisfied by the given @PackageIdentifer@.
fulfillDependency :: Dependency -> PackageIdentifier -> Bool
fulfillDependency (Dependency depName vrange) pkg
    = pkgName pkg == depName && pkgVersion pkg `withinRange` vrange

-- | Checks whether there is an installed package that satisfies the
--   given dependency.
isInstalled :: [PackageIdentifier] -- ^Installed packages.
            -> Dependency -> Bool
isInstalled ps dep = any (fulfillDependency dep) ps


getDependency :: ConfigFlags 
              -> [PackageIdentifier]
              -> [GenericPackageDescription]
              -> UnresolvedDependency -> ResolvedPackage
getDependency cfg installed available (UnresolvedDependency { dependency=dep, depOptions=opts})
    = ResolvedPackage { fulfilling = dep
                      , resolvedData = fmap pkgData (getLatestPkg available dep)
                      , pkgOptions = opts }
    where pkgData p = ( package p'
                      , pkgUrl p'
                      , map (getDependency cfg installed available . depToUnresolvedDep) (buildDepends p'))
             where p' = finalizePackage cfg installed available (configurationsFlags opts) p

configurationsFlags :: [String] -> [(String, Bool)]
configurationsFlags opts = 
    case filter ("--flags=" `isPrefixOf`) opts of
      [] -> []
      xs -> flagList $ removeQuotes $ drop 8 $ last xs
  where removeQuotes ('"':s) = take (length s - 1) s
        removeQuotes s = s
        flagList = map tagWithValue . words
            where tagWithValue ('-':name) = (map toLower name, False)
                  tagWithValue name       = (map toLower name, True)

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

finalizePackage :: ConfigFlags 
                -> [PackageIdentifier] -- ^ All installed packages
                -> [GenericPackageDescription] -- ^  All available packages
                -> [(String,Bool)] -- ^ Configurations flags
                -> GenericPackageDescription
                -> PackageDescription
finalizePackage cfg installed available flags desc
    = case e of
        Left missing -> error $ "Can't resolve dependencies: " ++ show missing
        Right (d,flags) -> d
  where 
    e = finalizePackageDescription 
          flags
          (Just $ nub $ installed ++ map (package . packageDescription) available) 
          System.Info.os
          System.Info.arch
          (showCompilerId (configCompiler cfg), compilerVersion (configCompiler cfg))
          desc

-- |Resolve some dependencies from the known packages while filtering out installed packages.
--  The result hasn't been modified to put the dependencies in front of the packages.
resolveDependenciesAux :: ConfigFlags
                       -> [PackageIdentifier] -- ^Installed packages.
                       -> [UnresolvedDependency] -- ^Dependencies in need of resolution.
                       -> IO [ResolvedPackage]
resolveDependenciesAux cfg ps deps
    = do installed <- listInstalledPackages cfg
         knownPkgs <- getKnownPackages cfg
         let resolve dep
              = let rDep = getDependency cfg installed knownPkgs dep
                in case resolvedData rDep of
                    Nothing -> resolvedDepToResolvedPkg (dependency dep,Nothing)
                    _ -> rDep
         return $ map resolve (filter (not . isInstalled ps . dependency) deps)
    where 
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
