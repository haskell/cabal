-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Fetch
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal fetch command
-----------------------------------------------------------------------------
module Distribution.Client.Fetch (
    fetch,
  ) where

import Distribution.Client.Types
         ( UnresolvedDependency (..), AvailablePackage(..)
         , AvailablePackageSource(..), AvailablePackageDb(..)
         , Repo(..), InstalledPackage )
import Distribution.Client.FetchUtils
import Distribution.Client.PackageIndex (PackageIndex)
import Distribution.Client.Dependency as Dependency
         ( resolveDependenciesWithProgress
         , resolveAvailablePackages
         , dependencyConstraints, dependencyTargets
         , PackagesPreference(..), PackagesPreferenceDefault(..)
         , PackagePreference(..) )
import Distribution.Client.Dependency.Types
         ( foldProgress )
import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages, disambiguateDependencies
         , getInstalledPackages )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.Setup
         ( FetchFlags(..) )

import Distribution.Package
         ( packageId, Dependency(..) )
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration )
import Distribution.Simple.Setup
         ( fromFlag )
import Distribution.Simple.Utils
         ( die, notice, info )
import Distribution.System
         ( buildPlatform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.Map as Map
import Control.Monad
         ( when, filterM )

-- ------------------------------------------------------------
-- * The fetch command
-- ------------------------------------------------------------

-- | Fetch a list of packages and their dependencies.
--
fetch :: Verbosity
      -> PackageDBStack
      -> [Repo]
      -> Compiler
      -> ProgramConfiguration
      -> FetchFlags
      -> [UnresolvedDependency]
      -> IO ()
fetch verbosity _ _ _ _ _ [] =
  notice verbosity "No packages requested. Nothing to do."

fetch verbosity packageDBs repos comp conf flags deps = do

  installed <- getInstalledPackages verbosity comp packageDBs conf
  availableDb@(AvailablePackageDb available _)
        <- getAvailablePackages verbosity repos
  deps' <- IndexUtils.disambiguateDependencies available deps

  pkgs <- resolvePackages verbosity
            includeDeps comp
            installed availableDb deps'

  pkgs' <- filterM (fmap not . isFetched) pkgs
  when (null pkgs') $
    notice verbosity $ "No packages need to be fetched. "
                    ++ "All the requested packages are already cached."
  if dryRun
    then notice verbosity $ unlines $
            "The following packages would be fetched:"
          : map (display . packageId) pkgs'
    else sequence_
           [ fetchRepoTarball verbosity repo pkgid
           | (AvailablePackage pkgid _ (RepoTarballPackage repo)) <- pkgs' ]
  where
    includeDeps = fromFlag (fetchDeps flags)
    dryRun      = fromFlag (fetchDryRun flags)


resolvePackages
  :: Verbosity
  -> Bool
  -> Compiler
  -> PackageIndex InstalledPackage
  -> AvailablePackageDb
  -> [UnresolvedDependency]
  -> IO [AvailablePackage]
resolvePackages verbosity includeDependencies comp
  installed (AvailablePackageDb available availablePrefs) deps

  | includeDependencies = do

      notice verbosity "Resolving dependencies..."
      plan <- foldProgress logMsg die return $
                resolveDependenciesWithProgress
                  buildPlatform (compilerId comp)
                  installed' available
                  preferences constraints
                  targets
      --TODO: suggest using --no-deps, unpack or fetch -o
      -- if cannot satisfy deps
      --TODO: add commandline constraint and preference args for fetch

      return (selectPackagesToFetch plan)

  | otherwise = do

    either (die . unlines . map show) return $
      resolveAvailablePackages
        installed   available
        preferences constraints
        targets

  where
    targets     = dependencyTargets     deps
    constraints = dependencyConstraints deps
    preferences = PackagesPreference
                    PreferLatestForSelected
                    [ PackageVersionPreference name ver
                    | (name, ver) <- Map.toList availablePrefs ]

    installed'  = hideGivenDeps deps installed

    -- Hide the packages given on the command line so that the dep resolver
    -- will decide that they need fetching, even if they're already
    -- installed. Sicne we want to get the source packages of things we might
    -- have installed (but not have the sources for).

    -- TODO: to allow for preferences on selecting an available version
    -- corresponding to a package we've got installed, instead of hiding the
    -- installed instances, we should add a constraint on using an installed
    -- instance.
    hideGivenDeps pkgs index =
      foldr PackageIndex.deletePackageName index
        [ name | UnresolvedDependency (Dependency name _) _ <- pkgs ]

    -- The packages we want to fetch are those packages the 'InstallPlan' that
    -- are in the 'InstallPlan.Configured' state.
    selectPackagesToFetch :: InstallPlan.InstallPlan -> [AvailablePackage]
    selectPackagesToFetch plan =
      [ pkg | (InstallPlan.Configured (InstallPlan.ConfiguredPackage pkg _ _))
                 <- InstallPlan.toList plan ]

    logMsg message rest = info verbosity message >> rest
