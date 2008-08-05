-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency
-- Copyright   :  (c) David Himmelstrup 2005, Bjorn Bringert 2007,
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A dependency resolver that is not very sophisticated.
-- It often makes installation plans with inconsistent dependencies.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency.Naive (
    naiveResolver
  ) where

import Distribution.InstalledPackageInfo (InstalledPackageInfo_(package))
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.Types
         ( UnresolvedDependency(..), AvailablePackage(..)
         , ConfiguredPackage(..) )
import Distribution.Client.Dependency.Types
         ( DependencyResolver, Progress(..) )
import Distribution.Package
         ( PackageIdentifier(..), Dependency(..), Package(..) )
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
import Distribution.Client.Utils
         ( showDependencies )
import Distribution.Text
         ( display )

import Control.Monad (mplus)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mappend))

naiveResolver :: DependencyResolver
naiveResolver os arch comp installed available _ deps =
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
                  -> Progress String String [InstallPlan.PlanPackage a]
                     -- ^ Either a list of missing dependencies, or a graph
                     -- of packages to install, with their options.
packagesToInstall allInstalled deps0 =
  case unzipEithers (map getAvailable deps0) of
    ([], ok)     ->
      let selectedAvailable :: [InstallPlan.ConfiguredPackage]
          selectedAvailable = concatMap snd ok

          selectedInstalled :: [InstalledPackageInfo]
          selectedInstalled =
              either PackageIndex.allPackages
              (\borked -> error $ unlines
                [ "Package " ++ display (packageId pkg)
                  ++ " depends on the following packages which are missing from the plan "
                  ++ intercalate ", " (map display missingDeps)
                | (pkg, missingDeps) <- borked ])
            $ PackageIndex.dependencyClosure
                allInstalled
                (getInstalled deps0)

       in Done $ map InstallPlan.Configured selectedAvailable
              ++ map InstallPlan.PreExisting selectedInstalled

    (missing, _) -> Fail $ "Unresolved dependencies: "
                        ++ showDependencies (concat missing)

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

unzipEithers :: [Either a b] -> ([a], [b])
unzipEithers = foldr (flip consEither) ([], [])
  where consEither ~(ls,rs) = either (\l -> (l:ls,rs)) (\r -> (ls,r:rs))
