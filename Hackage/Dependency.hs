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

import Distribution.InstalledPackageInfo (InstalledPackageInfo_(package))
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Hackage.DepGraph as DepGraph
import Hackage.Types (ResolvedDependency(..), UnresolvedDependency(..),
                      PkgInfo(..), FlagAssignment)
import Distribution.Package (PackageIdentifier(..), Package(..), Dependency(..))
import Distribution.PackageDescription 
    (PackageDescription(buildDepends)
    , GenericPackageDescription )
import Distribution.PackageDescription.Configuration
    ( finalizePackageDescription)
import Distribution.Simple.Compiler
         ( Compiler(compilerId) )
import Distribution.Simple.Utils (comparing)

import Control.Monad (mplus)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (Monoid(mappend))
import qualified Distribution.System (buildOS, buildArch)

--TODO: never expose the [ResolvedDependency], always gust make a DepGraph

resolveDependencies :: Compiler
                    -> Maybe (PackageIndex InstalledPackageInfo)
                    -> PackageIndex PkgInfo
                    -> [UnresolvedDependency]
                    -> [ResolvedDependency]
resolveDependencies comp (Just installed) available deps =
  [ resolveDependency comp installed available dep flags
  | UnresolvedDependency dep flags <- deps]
resolveDependencies _ Nothing available deps =
  resolveDependenciesBogusly available deps

-- | We're using a compiler where we cannot track installed packages so just
-- pretend everything is installed and hope for the best. Yay!
resolveDependenciesBogusly :: PackageIndex PkgInfo
                           -> [UnresolvedDependency]
                           -> [ResolvedDependency]
resolveDependenciesBogusly available = map resolveFromAvailable
  where resolveFromAvailable (UnresolvedDependency dep flags) =
          case latestAvailableSatisfying available dep of
            Nothing  -> UnavailableDependency dep
            Just pkg -> AvailableDependency dep pkg flags []

-- | Resolve dependencies of a local package description. This is used
-- when the top-level package does not come from hackage.
resolveDependenciesLocal :: Compiler
                         -> Maybe (PackageIndex InstalledPackageInfo)
                         -> PackageIndex PkgInfo
                         -> GenericPackageDescription
                         -> FlagAssignment
                         -> [ResolvedDependency]
resolveDependenciesLocal comp (Just installed) available desc flags =
  [ resolveDependency comp installed available dep flags
  | dep <- getDependencies comp installed available desc flags ]

-- When we do not know what is installed, let us just hope everything is ok:
resolveDependenciesLocal _ Nothing _ _ _ = []

resolveDependency :: Compiler
                  -> PackageIndex InstalledPackageInfo -- ^ Installed packages.
                  -> PackageIndex PkgInfo -- ^ Installable packages
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
latestInstalledSatisfying :: PackageIndex InstalledPackageInfo -> Dependency -> Maybe PackageIdentifier
latestInstalledSatisfying  index dep =
  case PackageIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing pkgVersion) (map package pkgs))

-- | Gets the latest available package satisfying a dependency.
latestAvailableSatisfying :: PackageIndex PkgInfo -> Dependency -> Maybe PkgInfo
latestAvailableSatisfying index dep =
  case PackageIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing (pkgVersion . packageId)) pkgs)

-- | Gets the dependencies of an available package.
getDependencies :: Compiler 
                -> PackageIndex InstalledPackageInfo -- ^ Installed packages.
                -> PackageIndex PkgInfo -- ^ Available packages
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
                (let --TODO: find a better way to do this:
                     flatten :: Package pkg => PackageIndex pkg
                                            -> PackageIndex PackageIdentifier
                     flatten = PackageIndex.fromList . map packageId
                             . PackageIndex.allPackages
                  in Just (flatten available `mappend` flatten installed))
                Distribution.System.buildOS
                Distribution.System.buildArch
                (compilerId comp) [] pkg

packagesToInstall :: [ResolvedDependency]
                  -> Either [Dependency] DepGraph.DepGraph
                     -- ^ Either a list of missing dependencies, or a graph
                     -- of packages to install, with their options.
packagesToInstall deps0 = case unzipEithers (map getDeps deps0) of
  ([], ok)     -> Right (DepGraph.fromList (concatMap snd ok))
  (missing, _) -> Left  (concat missing)

  where
    getDeps :: ResolvedDependency
            -> Either [Dependency]
                      (Maybe PackageIdentifier, [DepGraph.ResolvedPackage])
    getDeps (InstalledDependency _ _    )          = Right (Nothing, [])
    getDeps (AvailableDependency _ pkg flags deps) =
      case unzipEithers (map getDeps deps) of
        ([], ok)     -> let resolved :: [DepGraph.ResolvedPackage]
                            resolved = DepGraph.ResolvedPackage pkg flags
                                         [ pkgid | (Just pkgid, _) <- ok ]
                                     : concatMap snd ok
                         in Right (Just $ packageId pkg, resolved)
        (missing, _) -> Left (concat missing)
    getDeps (UnavailableDependency dep) = Left [dep]

-- |Given the list of installed packages and installable packages, figure
-- out which packages can be upgraded.

getUpgradableDeps :: PackageIndex InstalledPackageInfo
                  -> PackageIndex PkgInfo
                  -> [PkgInfo]
getUpgradableDeps installed available =
  let latestInstalled = getLatestPackageVersions installed
      mNeedingUpgrade = map (flip newerAvailable available) latestInstalled
   in catMaybes mNeedingUpgrade

  where newerAvailable :: PackageIdentifier
                       -> PackageIndex PkgInfo -- ^installable packages
                       -> Maybe PkgInfo -- ^greatest available
        newerAvailable pkgToUpdate index
            = foldl (newerThan pkgToUpdate) Nothing (PackageIndex.allPackages index)
        newerThan :: PackageIdentifier 
                  -> Maybe PkgInfo
                  -> PkgInfo
                  -> Maybe PkgInfo
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

unzipEithers :: [Either a b] -> ([a], [b])
unzipEithers = foldr (flip consEither) ([], [])
  where consEither ~(ls,rs) = either (\l -> (l:ls,rs)) (\r -> (ls,r:rs))
