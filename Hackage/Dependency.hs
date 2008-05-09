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

import Distribution.InstalledPackageInfo (InstalledPackageInfo_(package))
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.InstallPlan (InstallPlan)
import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..)
         , ConfiguredPackage(..) )
import Distribution.Package
         ( PackageIdentifier(..), Dependency(..)
         , Package(..), PackageFixedDeps(..) )
import Distribution.PackageDescription 
         ( PackageDescription(buildDepends), GenericPackageDescription
         , FlagAssignment )
import Distribution.PackageDescription.Configuration
    ( finalizePackageDescription)
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( OS, Arch )
import Distribution.Simple.Utils (comparing, intercalate)
import Distribution.Text
         ( display )

import Control.Monad (mplus)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (Monoid(mempty, mappend))
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
    foldr (PackageIndex.delete . packageId) index
  . PackageIndex.reverseDependencyClosure index
  . map (packageId . fst)
  $ PackageIndex.brokenPackages index

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

-- | This resolver thinks that every package is already installed.
--
bogusResolver :: DependencyResolver a
bogusResolver os arch comp _ available deps =
  case unzipEithers (map resolveFromAvailable deps) of
    (ok, [])      -> Right ok
    (_ , missing) -> Left missing
  where
    resolveFromAvailable (UnresolvedDependency dep flags) =
      case latestAvailableSatisfying available dep of
        Nothing  -> Right dep
        Just apkg@(AvailablePackage _ pkg _) ->
          case finalizePackageDescription flags none os arch comp [] pkg of
            Right (_, flags') -> Left $ InstallPlan.Configured $
                                   ConfiguredPackage apkg flags' []
            --TODO: we actually have to delete the deps of pkg, otherwise
            -- the install plan verifier will say we're missing deps.
            _ -> error "bogusResolver: impossible happened"
          where
            none :: Maybe (PackageIndex PackageIdentifier)
            none = Nothing

naiveResolver :: DependencyResolver a
naiveResolver os arch comp installed available deps =
  packagesToInstall installed
    [ resolveDependency os arch comp installed available dep flags
    | UnresolvedDependency dep flags <- deps]

resolveDependency :: OS
                  -> Arch
                  -> CompilerId
                  -> PackageIndex InstalledPackageInfo -- ^ Installed packages.
                  -> PackageIndex AvailablePackage -- ^ Installable packages
                  -> Dependency
                  -> FlagAssignment
                  -> ResolvedDependency
resolveDependency os arch comp installed available dep flags
    = fromMaybe (UnavailableDependency dep) $ resolveFromInstalled `mplus` resolveFromAvailable
  where
    resolveFromInstalled = fmap (InstalledDependency dep) $ latestInstalledSatisfying installed dep
    resolveFromAvailable = 
        do pkg <- latestAvailableSatisfying available dep
           (deps, flags') <- getDependencies os arch comp installed available
                                             (packageDescription pkg) flags
           return $ AvailableDependency dep pkg flags'
              [ resolveDependency os arch comp installed available dep' []
              | dep' <- deps ]

-- | Gets the latest installed package satisfying a dependency.
latestInstalledSatisfying :: PackageIndex InstalledPackageInfo -> Dependency -> Maybe PackageIdentifier
latestInstalledSatisfying  index dep =
  case PackageIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing pkgVersion) (map package pkgs))

-- | Gets the latest available package satisfying a dependency.
latestAvailableSatisfying :: PackageIndex AvailablePackage
                          -> Dependency
                          -> Maybe AvailablePackage
latestAvailableSatisfying index dep =
  case PackageIndex.lookupDependency index dep of
    []   -> Nothing
    pkgs -> Just (maximumBy (comparing (pkgVersion . packageId)) pkgs)

-- | Gets the dependencies of an available package.
getDependencies :: OS
                -> Arch
                -> CompilerId
                -> PackageIndex InstalledPackageInfo -- ^ Installed packages.
                -> PackageIndex AvailablePackage -- ^ Available packages
                -> GenericPackageDescription
                -> FlagAssignment
                -> Maybe ([Dependency], FlagAssignment)
                   -- ^ If successful, this is the list of dependencies.
                   -- If flag assignment failed, this is the list of
                   -- missing dependencies.
getDependencies os arch comp installed available pkg flags
    = case e of
        Left  _missing      -> Nothing
        Right (desc,flags') -> Just (buildDepends desc, flags')
    where 
      e = finalizePackageDescription 
                flags
                (let --TODO: find a better way to do this:
                     flatten :: Package pkg => PackageIndex pkg
                                            -> PackageIndex PackageIdentifier
                     flatten = PackageIndex.fromList . map packageId
                             . PackageIndex.allPackages
                  in Just (flatten available `mappend` flatten installed))
                os arch comp [] pkg

packagesToInstall :: PackageIndex InstalledPackageInfo
                  -> [ResolvedDependency]
                  -> Either [Dependency] [InstallPlan.PlanPackage a]
                     -- ^ Either a list of missing dependencies, or a graph
                     -- of packages to install, with their options.
packagesToInstall allInstalled deps0 =
  case unzipEithers (map getAvailable deps0) of
    ([], ok)     ->
      let selectedAvailable :: [InstallPlan.ConfiguredPackage]
          selectedAvailable = concatMap snd ok

          selectedInstalled :: [InstalledPackageInfo]
          selectedInstalled = either PackageIndex.allPackages
                              (\borked -> error $ unlines
                                [ "Package " ++ display (packageId pkg)
                                  ++ " depends on the following packages which are missing from the plan "
                                  ++ intercalate ", " (map display missingDeps)
                                | (pkg, missingDeps) <- borked ])
                            $ PackageIndex.dependencyClosure
                                allInstalled
                                (getInstalled deps0)

       in Right $ map InstallPlan.Configured selectedAvailable
              ++ map InstallPlan.PreExisting selectedInstalled

    (missing, _) -> Left $ concat missing

  where
    getAvailable :: ResolvedDependency
                  -> Either [Dependency]
                            (PackageIdentifier, [InstallPlan.ConfiguredPackage])
    getAvailable (InstalledDependency _ pkgid    )          
      = Right (pkgid, [])
    getAvailable (AvailableDependency _ pkg flags deps) =
      case unzipEithers (map getAvailable deps) of
        ([], ok)     -> let resolved = InstallPlan.ConfiguredPackage pkg flags
                                         [ pkgid | (pkgid, _) <- ok ]
                                     : concatMap snd ok
                         in Right (packageId pkg, resolved)
        (missing, _) -> Left (concat missing)
    getAvailable (UnavailableDependency dep) = Left [dep]
    
    getInstalled :: [ResolvedDependency] -> [PackageIdentifier]
    getInstalled [] = []
    getInstalled (dep:deps) = case dep of
      InstalledDependency _ pkgid     -> pkgid : getInstalled deps
      AvailableDependency _ _ _ deps' ->         getInstalled (deps'++deps)
      UnavailableDependency _         ->         getInstalled deps

-- TODO: kill this data type
data ResolvedDependency
       = InstalledDependency Dependency PackageIdentifier
       | AvailableDependency Dependency AvailablePackage FlagAssignment [ResolvedDependency]
       | UnavailableDependency Dependency
       deriving (Show)

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

unzipEithers :: [Either a b] -> ([a], [b])
unzipEithers = foldr (flip consEither) ([], [])
  where consEither ~(ls,rs) = either (\l -> (l:ls,rs)) (\r -> (ls,r:rs))
