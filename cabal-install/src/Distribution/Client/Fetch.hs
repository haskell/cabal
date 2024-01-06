------------------------------------------------------------------------------- |
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
module Distribution.Client.Fetch
  ( fetch
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Dependency
import Distribution.Client.FetchUtils hiding (fetchPackage)
import Distribution.Client.IndexUtils as IndexUtils
  ( getInstalledPackages
  , getSourcePackages
  )
import Distribution.Client.Setup
  ( FetchFlags (..)
  , GlobalFlags (..)
  , RepoContext (..)
  )
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.Targets
import Distribution.Client.Types

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb, readPkgConfigDb)
import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.SourcePackage

import Distribution.Client.Errors
import Distribution.Package
  ( packageId
  )
import Distribution.Simple.Compiler
  ( Compiler
  , PackageDBStack
  , compilerInfo
  )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program
  ( ProgramDb
  )
import Distribution.Simple.Setup
  ( fromFlag
  , fromFlagOrDefault
  )
import Distribution.Simple.Utils
  ( debug
  , dieWithException
  , notice
  )
import Distribution.System
  ( Platform
  )

-- ------------------------------------------------------------

-- * The fetch command

-- ------------------------------------------------------------

-- TODO:

-- * add fetch -o support

-- * support tarball URLs via ad-hoc download cache (or in -o mode?)

-- * suggest using --no-deps, unpack or fetch -o if deps cannot be satisfied

-- * Port various flags from install:

--   * --upgrade-dependencies
--   * --constraint and --preference
--   * --only-dependencies, but note it conflicts with --no-deps

-- | Fetch a list of packages and their dependencies.
fetch
  :: Verbosity
  -> PackageDBStack
  -> RepoContext
  -> Compiler
  -> Platform
  -> ProgramDb
  -> GlobalFlags
  -> FetchFlags
  -> [UserTarget]
  -> IO ()
fetch verbosity _ _ _ _ _ _ _ [] =
  notice verbosity "No packages requested. Nothing to do."
fetch
  verbosity
  packageDBs
  repoCtxt
  comp
  platform
  progdb
  _
  fetchFlags
  userTargets = do
    traverse_ (checkTarget verbosity) userTargets

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
    sourcePkgDb <- getSourcePackages verbosity repoCtxt
    pkgConfigDb <- readPkgConfigDb verbosity progdb

    pkgSpecifiers <-
      resolveUserTargets
        verbosity
        repoCtxt
        (packageIndex sourcePkgDb)
        userTargets

    pkgs <-
      planPackages
        verbosity
        comp
        platform
        fetchFlags
        installedPkgIndex
        sourcePkgDb
        pkgConfigDb
        pkgSpecifiers

    pkgs' <- filterM (fmap not . isFetched . srcpkgSource) pkgs
    if null pkgs'
      then -- TODO: when we add support for remote tarballs then this message
      -- will need to be changed because for remote tarballs we fetch them
      -- at the earlier phase.

        notice verbosity $
          "No packages need to be fetched. "
            ++ "All the requested packages are already local "
            ++ "or cached locally."
      else
        if dryRun
          then
            notice verbosity $
              unlines $
                "The following packages would be fetched:"
                  : map (prettyShow . packageId) pkgs'
          else traverse_ (fetchPackage verbosity repoCtxt . srcpkgSource) pkgs'
    where
      dryRun = fromFlag (fetchDryRun fetchFlags)

planPackages
  :: Verbosity
  -> Compiler
  -> Platform
  -> FetchFlags
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> PkgConfigDb
  -> [PackageSpecifier UnresolvedSourcePackage]
  -> IO [UnresolvedSourcePackage]
planPackages
  verbosity
  comp
  platform
  fetchFlags
  installedPkgIndex
  sourcePkgDb
  pkgConfigDb
  pkgSpecifiers
    | includeDependencies = do
        notice verbosity "Resolving dependencies..."
        installPlan <-
          foldProgress logMsg (dieWithException verbosity . PlanPackages . show) return $
            resolveDependencies
              platform
              (compilerInfo comp)
              pkgConfigDb
              resolverParams

        -- The packages we want to fetch are those packages the 'InstallPlan'
        -- that are in the 'InstallPlan.Configured' state.
        return
          [ solverPkgSource cpkg
          | (SolverInstallPlan.Configured cpkg) <-
              SolverInstallPlan.toList installPlan
          ]
    | otherwise =
        either (dieWithException verbosity . PlanPackages . unlines . map show) return $
          resolveWithoutDependencies resolverParams
    where
      resolverParams :: DepResolverParams
      resolverParams =
        setMaxBackjumps
          ( if maxBackjumps < 0
              then Nothing
              else Just maxBackjumps
          )
          . setIndependentGoals independentGoals
          . setReorderGoals reorderGoals
          . setCountConflicts countConflicts
          . setFineGrainedConflicts fineGrainedConflicts
          . setMinimizeConflictSet minimizeConflictSet
          . setShadowPkgs shadowPkgs
          . setStrongFlags strongFlags
          . setAllowBootLibInstalls allowBootLibInstalls
          . setOnlyConstrained onlyConstrained
          . setSolverVerbosity verbosity
          . addConstraints
            [ let pc =
                    PackageConstraint
                      (scopeToplevel $ pkgSpecifierTarget pkgSpecifier)
                      (PackagePropertyStanzas stanzas)
               in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
            | pkgSpecifier <- pkgSpecifiers
            ]
          -- Reinstall the targets given on the command line so that the dep
          -- resolver will decide that they need fetching, even if they're
          -- already installed. Since we want to get the source packages of
          -- things we might have installed (but not have the sources for).
          . reinstallTargets
          $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

      includeDependencies = fromFlag (fetchDeps fetchFlags)
      logMsg message rest = debug verbosity message >> rest

      stanzas =
        [TestStanzas | testsEnabled]
          ++ [BenchStanzas | benchmarksEnabled]
      testsEnabled = fromFlagOrDefault False $ fetchTests fetchFlags
      benchmarksEnabled = fromFlagOrDefault False $ fetchBenchmarks fetchFlags

      reorderGoals = fromFlag (fetchReorderGoals fetchFlags)
      countConflicts = fromFlag (fetchCountConflicts fetchFlags)
      fineGrainedConflicts = fromFlag (fetchFineGrainedConflicts fetchFlags)
      minimizeConflictSet = fromFlag (fetchMinimizeConflictSet fetchFlags)
      independentGoals = fromFlag (fetchIndependentGoals fetchFlags)
      shadowPkgs = fromFlag (fetchShadowPkgs fetchFlags)
      strongFlags = fromFlag (fetchStrongFlags fetchFlags)
      maxBackjumps = fromFlag (fetchMaxBackjumps fetchFlags)
      allowBootLibInstalls = fromFlag (fetchAllowBootLibInstalls fetchFlags)
      onlyConstrained = fromFlag (fetchOnlyConstrained fetchFlags)

checkTarget :: Verbosity -> UserTarget -> IO ()
checkTarget verbosity target = case target of
  UserTargetRemoteTarball _uri ->
    dieWithException verbosity CheckTarget
  _ -> return ()

fetchPackage :: Verbosity -> RepoContext -> PackageLocation a -> IO ()
fetchPackage verbosity repoCtxt pkgsrc = case pkgsrc of
  LocalUnpackedPackage _dir -> return ()
  LocalTarballPackage _file -> return ()
  RemoteTarballPackage _uri _ ->
    dieWithException verbosity CheckTarget
  RemoteSourceRepoPackage _repo _ ->
    dieWithException verbosity FetchPackage
  RepoTarballPackage repo pkgid _ -> do
    _ <- fetchRepoTarball verbosity repoCtxt repo pkgid
    return ()
