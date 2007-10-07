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
    , depToUnresolvedDep
    , getPackages            -- :: [ResolvedPackage] -> [(PackageIdentifier,[String],String)]
    , getBuildDeps           -- :: [PackageIdentifier] -> [ResolvedPackage] -> [ResolvedPackage]
    , filterFetchables       -- :: [ResolvedPackage] -> [(PackageIdentifier,String)]
    , fulfillDependency      -- :: Dependency -> PackageIdentifier -> Bool
    ) where

import Distribution.Version (Version, Dependency(..), withinRange)
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription 
    (PackageDescription(package, buildDepends)
    , GenericPackageDescription(packageDescription)
    , finalizePackageDescription)
import Distribution.ParseUtils (showDependency)
import Distribution.Simple.Compiler  (PackageDB(..), Compiler, showCompilerId, compilerVersion)
import Distribution.Simple.Program (ProgramConfiguration)

import Data.Char (toLower)
import Data.List (nub, maximumBy, isPrefixOf)
import qualified System.Info (arch,os)

import Network.Hackage.CabalInstall.Config (listInstalledPackages, getKnownPackages, findCompiler)
import Network.Hackage.CabalInstall.Types ( ResolvedPackage(..), UnresolvedDependency(..)
                                      , ConfigFlags (..), PkgInfo (..), ResolvedDependency(..), Repo(..))
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
    where rData = do ResolvedDependency pkg repo subDeps <- rDep
                     return ( pkg
                            , repo
                            , map resolvedDepToResolvedPkg subDeps )


-- |Locates a @PkgInfo@ which satisfies a given @Dependency@.
--  Fails with "cannot satisfy dependency: %s." where %s == the given dependency.
getLatestPkg :: (Monad m) => [PkgInfo] -> Dependency -> m PkgInfo
getLatestPkg ps dep
    = case filter (fulfillDependency dep . pkdId) ps of
        [] -> fail $ printf "cannot satisfy dependency: %s." (show (showDependency dep))
        qs -> return $ maximumBy compareVersions qs
  where compareVersions a b = pkgVersion (pkdId a) `compare` pkgVersion (pkdId b)
        pkdId = package . packageDescription . pkgDesc

-- |Evaluates to @True@ if the given @Dependency@ is satisfied by the given @PackageIdentifer@.
fulfillDependency :: Dependency -> PackageIdentifier -> Bool
fulfillDependency (Dependency depName vrange) pkg
    = pkgName pkg == depName && pkgVersion pkg `withinRange` vrange

-- | Checks whether there is an installed package that satisfies the
--   given dependency.
isInstalled :: [PackageIdentifier] -- ^Installed packages.
            -> Dependency -> Bool
isInstalled ps dep = any (fulfillDependency dep) ps


getDependency :: Compiler
              -> [PackageIdentifier]
              -> [PkgInfo]
              -> UnresolvedDependency -> ResolvedPackage
getDependency comp installed available (UnresolvedDependency { dependency=dep, depOptions=opts})
    = ResolvedPackage { fulfilling = dep
                      , resolvedData = fmap pkgData (getLatestPkg available dep)
                      , pkgOptions = opts }
    where pkgData p = ( package d
                      , pkgRepo p
                      , map (getDependency comp installed available . depToUnresolvedDep) (buildDepends d))
             where d = finalizePackage comp installed available (configurationsFlags opts) p

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
getPackages :: [ResolvedPackage] -> [(PackageIdentifier,[String],Repo)]
getPackages = map worker
    where worker dep
              = case resolvedData dep of
                  Nothing
                      -> error $ printf "Couldn't satisfy dependency: '%s'." (show $ showDependency (fulfilling dep))
                  Just (pkg,repo,_)
                      -> (pkg,pkgOptions dep,repo)

-- |List all packages which can be fetched.
filterFetchables :: [ResolvedPackage] -> [(PackageIdentifier,Repo)]
filterFetchables pkgs = [(pkg,repo) | Just (pkg,repo,_) <- map resolvedData pkgs]

finalizePackage :: Compiler 
                -> [PackageIdentifier] -- ^ All installed packages
                -> [PkgInfo] -- ^  All available packages
                -> [(String,Bool)] -- ^ Configurations flags
                -> PkgInfo
                -> PackageDescription
finalizePackage comp installed available flags desc
    = case e of
        Left missing -> error $ "Can't resolve dependencies: " ++ show missing
        Right (d,flags) -> d
  where 
    e = finalizePackageDescription 
          flags
          (Just $ nub $ installed ++ map (package . packageDescription . pkgDesc) available) 
          System.Info.os
          System.Info.arch
          (showCompilerId comp, compilerVersion comp)
          (pkgDesc desc)

-- |Resolve some dependencies from the known packages while filtering out installed packages.
--  The result hasn't been modified to put the dependencies in front of the packages.
resolveDependenciesAux :: ConfigFlags
                       -> Compiler
                       -> ProgramConfiguration
                       -> [PackageIdentifier] -- ^Installed packages.
                       -> [PkgInfo] -- ^ Installable packages
                       -> [UnresolvedDependency] -- ^Dependencies in need of resolution.
                       -> [ResolvedPackage]
resolveDependenciesAux cfg comp conf installed available deps
    = map resolve (filter (not . isInstalled installed . dependency) deps)
  where resolve dep
              = let rDep = getDependency comp installed available dep
                in case resolvedData rDep of
                    Nothing -> resolvedDepToResolvedPkg (dependency dep,Nothing)
                    _ -> rDep


-- |Resolve some dependencies from the known packages while filtering out installed packages.
--  The result has been modified to put the dependencies in front of the packages.
resolveDependencies :: ConfigFlags
                    -> Compiler
                    -> ProgramConfiguration
                    -> [UnresolvedDependency] -- ^Dependencies in need of resolution.
                    -> IO [ResolvedPackage]
resolveDependencies cfg comp conf deps
    = do installed <- listInstalledPackages cfg comp conf
         available <- getKnownPackages cfg
         return $ flattenDepList installed $ resolveDependenciesAux cfg comp conf installed available deps
