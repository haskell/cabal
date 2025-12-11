{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- /Elaborated: worked out with great care and nicety of detail; executed with great minuteness: elaborate preparations; elaborate care./
--
-- In this module we construct an install plan that includes all the information needed to execute it.
--
-- Building a project is therefore split into two phases:
--
-- 1. The construction of the install plan (which as far as possible should be pure), done here.
-- 2. The execution of the plan, done in "ProjectBuilding"
--
-- To achieve this we need a representation of this fully elaborated install plan; this representation
-- consists of two parts:
--
-- * A 'ElaboratedInstallPlan'. This is a 'GenericInstallPlan' with a
--   representation of source packages that includes a lot more detail about
--   that package's individual configuration
--
-- * A 'ElaboratedSharedConfig'. Some package configuration is the same for
--   every package in a plan. Rather than duplicate that info every entry in
--   the 'GenericInstallPlan' we keep that separately.
--
-- The division between the shared and per-package config is not set in stone
-- for all time. For example if we wanted to generalise the install plan to
-- describe a situation where we want to build some packages with GHC and some
-- with GHCJS then the platform and compiler would no longer be shared between
-- all packages but would have to be per-package (probably with some sanity
-- condition on the graph structure).
module Distribution.Client.ProjectPlanning
  ( -- * Types for the elaborated install plan
    ElaboratedInstallPlan
  , ElaboratedInstalledPackageInfo
  , ElaboratedConfiguredPackage (..)
  , ElaboratedPlanPackage
  , ElaboratedSharedConfig (..)
  , ElaboratedReadyPackage
  , BuildStyle (..)
  , CabalFileText
  , Toolchain (..)
  , Stage (..)
  , Staged (..)
  , WithStage (..)
  , elabOrderLibDependencies
  , elabOrderExeDependencies
  , elabLibDependencies
  , elabExeDependencies

    -- * Reading the project configuration
    -- $readingTheProjectConfiguration
  , rebuildProjectConfig

    -- * Producing the elaborated install plan
  , rebuildInstallPlan

    -- * Build targets
  , availableTargets
  , AvailableTarget (..)
  , AvailableTargetStatus (..)
  , TargetRequested (..)
  , ComponentTarget (..)
  , SubComponentTarget (..)
  , showComponentTarget
  , nubComponentTargets

    -- * Selecting a plan subset
  , pruneInstallPlanToTargets
  , TargetAction (..)
  , pruneInstallPlanToDependencies
  , CannotPruneDependencies (..)

    -- * Utils required for building
  , pkgHasEphemeralBuildTargets
  , elabBuildTargetWholeComponents
  , configureToolchains

    -- * Setup.hs CLI flags for building
  , setupHsScriptOptions
  , setupHsCommonFlags
  , setupHsConfigureFlags
  , setupHsConfigureArgs
  , setupHsBuildFlags
  , setupHsBuildArgs
  , setupHsReplFlags
  , setupHsReplArgs
  , setupHsTestFlags
  , setupHsTestArgs
  , setupHsBenchFlags
  , setupHsBenchArgs
  , setupHsCopyFlags
  , setupHsRegisterFlags
  , setupHsHaddockFlags
  , setupHsHaddockArgs
  , packageHashInputs

    -- * Path construction
  , binDirectoryFor
  , binDirectories
  , storePackageInstallDirs
  , storePackageInstallDirs'
  , elabDistDirParams
  ) where

import Distribution.Client.Compat.Prelude
import Text.PrettyPrint
  ( comma
  , fsep
  , hang
  , punctuate
  , quotes
  , render
  , text
  , vcat
  , ($$)
  )
import Prelude ()

import Distribution.Client.Config
import Distribution.Client.Dependency
import Distribution.Client.DistDirLayout
import Distribution.Client.FetchUtils
import Distribution.Client.HashValue
import Distribution.Client.HttpUtils
import Distribution.Client.JobControl
import Distribution.Client.PackageHash
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy
import Distribution.Client.ProjectConfig.Types (defaultProjectFileParser)
import Distribution.Client.ProjectPlanOutput
import Distribution.Client.ProjectPlanning.SetupPolicy
  ( NonSetupLibDepSolverPlanPackage (..)
  , packageSetupScriptSpecVersion
  , packageSetupScriptStyle
  )
import Distribution.Client.ProjectPlanning.Types as Ty
import Distribution.Client.RebuildMonad
import Distribution.Client.Setup hiding (cabalVersion, packageName)
import Distribution.Client.SetupWrapper
import Distribution.Client.Store
import Distribution.Client.Targets (userToPackageConstraint)
import Distribution.Client.Toolchain
import Distribution.Client.Types
import Distribution.Client.Utils (concatMapM)

import qualified Distribution.Client.BuildReports.Storage as BuildReports
import qualified Distribution.Client.IndexUtils as IndexUtils
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan

import Distribution.CabalSpecVersion
import Distribution.Utils.LogProgress
import Distribution.Utils.MapAccum
import Distribution.Utils.NubList
import Distribution.Utils.Path hiding
  ( (<.>)
  , (</>)
  )

import qualified Hackage.Security.Client as Sec

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.InstSolverPackage
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PkgConfigDb
import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.SourcePackage

import Distribution.ModuleName
import Distribution.Package
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.LocalBuildInfo
  ( Component (..)
  , componentBuildInfo
  , componentName
  , pkgComponents
  )

import Distribution.Simple.BuildWay
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Find
import Distribution.System

import Distribution.Types.AnnotatedId
import Distribution.Types.ComponentInclude
import Distribution.Types.ComponentName
import Distribution.Types.DependencySatisfaction
  ( DependencySatisfaction (..)
  )
import Distribution.Types.DumpBuildInfo
import Distribution.Types.GivenComponent
import Distribution.Types.LibraryName
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.PkgconfigDependency
import Distribution.Types.UnqualComponentName

import Distribution.Backpack hiding (mkDefUnitId)
import Distribution.Backpack.ComponentsGraph
import Distribution.Backpack.ConfiguredComponent
import Distribution.Backpack.FullUnitId
import Distribution.Backpack.LinkedComponent
import Distribution.Backpack.ModuleShape

import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version

import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import qualified Distribution.Simple.Configure as Cabal
import qualified Distribution.Simple.GHC as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Solver.Types.ComponentDeps as CD

import qualified Distribution.Compat.Graph as Graph

import Control.Exception (assert)
import Control.Monad (sequence)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, execState, gets, modify)
import Data.Foldable (fold)
import Data.List (deleteBy, groupBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Client.Errors
import Distribution.Client.InstallPlan (foldPlanPackage)
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Solver.Types.ResolverPackage (solverId)
import qualified Distribution.Solver.Types.ResolverPackage as ResolverPackage
import GHC.Stack (HasCallStack)
import System.Directory (getCurrentDirectory)
import System.FilePath
import qualified Text.PrettyPrint as Disp

-- | Check that an 'ElaboratedConfiguredPackage' actually makes
-- sense under some 'ElaboratedSharedConfig'.
sanityCheckElaboratedConfiguredPackage
  :: ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> a
  -> a
sanityCheckElaboratedConfiguredPackage
  sharedConfig
  elab@ElaboratedConfiguredPackage{..} =
    ( case elabPkgOrComp of
        ElabPackage pkg -> sanityCheckElaboratedPackage elab pkg
        ElabComponent comp -> sanityCheckElaboratedComponent elab comp
    )
      -- The assertion below fails occasionally for unknown reason
      -- so it was muted until we figure it out, otherwise it severely
      -- hinders our ability to share and test development builds of cabal-install.
      -- Tracking issue: https://github.com/haskell/cabal/issues/6006
      --
      -- either a package is being built inplace, or the
      -- 'installedPackageId' we assigned is consistent with
      -- the 'hashedInstalledPackageId' we would compute from
      -- the elaborated configured package
      . assert
        ( isInplaceBuildStyle elabBuildStyle
            || elabComponentId
              == hashedInstalledPackageId
                (packageHashInputs sharedConfig elab)
        )
      -- the stanzas explicitly disabled should not be available
      . assert
        ( optStanzaSetNull $
            optStanzaKeysFilteredByValue (maybe False not) elabStanzasRequested `optStanzaSetIntersection` elabStanzasAvailable
        )
      -- either a package is built inplace, or we are not attempting to
      -- build any test suites or benchmarks (we never build these
      -- for remote packages!)
      . assert
        ( isInplaceBuildStyle elabBuildStyle
            || optStanzaSetNull elabStanzasAvailable
        )

sanityCheckElaboratedComponent
  :: ElaboratedConfiguredPackage
  -> ElaboratedComponent
  -> a
  -> a
sanityCheckElaboratedComponent
  ElaboratedConfiguredPackage{..}
  ElaboratedComponent{..} =
    -- Should not be building bench or test if not inplace.
    assert
      ( isInplaceBuildStyle elabBuildStyle
          || case compComponentName of
            Nothing -> True
            Just (CLibName _) -> True
            Just (CExeName _) -> True
            -- This is interesting: there's no way to declare a dependency
            -- on a foreign library at the moment, but you may still want
            -- to install these to the store
            Just (CFLibName _) -> True
            Just (CBenchName _) -> False
            Just (CTestName _) -> False
      )

sanityCheckElaboratedPackage
  :: ElaboratedConfiguredPackage
  -> ElaboratedPackage
  -> a
  -> a
sanityCheckElaboratedPackage
  ElaboratedConfiguredPackage{..}
  ElaboratedPackage{..} =
    -- we should only have enabled stanzas that actually can be built
    -- (according to the solver)
    assert (pkgStanzasEnabled `optStanzaSetIsSubset` elabStanzasAvailable)
      -- the stanzas that the user explicitly requested should be
      -- enabled (by the previous test, they are also available)
      . assert
        ( optStanzaKeysFilteredByValue (fromMaybe False) elabStanzasRequested
            `optStanzaSetIsSubset` pkgStanzasEnabled
        )

-- $readingTheProjectConfiguration
--
-- The project configuration is assembled into a ProjectConfig as follows:
--
-- CLI arguments are converted using "commandLineFlagsToProjectConfig" in the
-- v2 command entrypoints and passed to "establishProjectBaseContext" which
-- then calls "rebuildProjectConfig".
--
-- "rebuildProjectConfig" then calls "readProjectConfig" to read the project
-- files. Due to the presence of conditionals, this output is in the form of a
-- "ProjectConfigSkeleton" and will be resolved by "rebuildProjectConfig" using
-- "instantiateProjectConfigSkeletonFetchingCompiler".
--
-- "readProjectConfig" also loads the global configuration, which is read with
-- "loadConfig" and convertd to a "ProjectConfig" with "convertLegacyGlobalConfig".
--
-- *Important:* You can notice how some project config options are needed to read the
-- project config! This is evident by the fact that "rebuildProjectConfig"
-- takes "HttpTransport" and "DistDirLayout" as parameters. Two arguments are
-- infact determined from the CLI alone (in "establishProjectBaseContext").
-- Consequently, project files (including global configuration) cannot
-- affect those parameters!
--
-- Furthermore, the project configuration can specify a compiler to use,
-- which we need to resolve the conditionals in the project configuration!
-- To solve this, we configure the compiler from what is obtained by applying
-- the CLI configuration over the the configuration obtained by "flattening"
-- ProjectConfigSkeleton. This means collapsing all conditionals by taking
-- both branches.

-- | Return the up-to-date project config and information about the local
-- packages within the project.
rebuildProjectConfig
  :: HasCallStack
  => Verbosity
  -> HttpTransport
  -> DistDirLayout
  -> ProjectConfig
  -> IO
      ( ProjectConfig
      , [PackageSpecifier UnresolvedSourcePackage]
      )
rebuildProjectConfig
  verbosity
  httpTransport
  distDirLayout@DistDirLayout
    { distProjectRootDirectory
    , distDirectory
    , distProjectCacheFile
    , distProjectCacheDirectory
    , distProjectFile
    }
  cliConfig = do
    progsearchpath <- liftIO $ getSystemSearchPath

    let fileMonitorProjectConfig = newFileMonitor (distProjectCacheFile "config")

    fileMonitorProjectConfigKey <- do
      configPath <- getConfigFilePath projectConfigConfigFile
      return
        ( configPath
        , distProjectFile ""
        , projectConfigProjectFileParser
        , projectConfigToolchain
        , progsearchpath
        , packageConfigProgramPaths
        , packageConfigProgramPathExtra
        )

    (projectConfig, localPackages) <-
      runRebuild distProjectRootDirectory
        $ rerunIfChanged
          verbosity
          fileMonitorProjectConfig
          fileMonitorProjectConfigKey -- todo check deps too?
        $ do
          liftIO $ info verbosity "Project settings changed, reconfiguring..."
          projectConfigSkeleton <- phaseReadProjectConfig

          let fetchCompiler = do
                -- have to create the cache directory before configuring the compiler
                liftIO $ createDirectoryIfMissingVerbose verbosity True distProjectCacheDirectory
                toolchains <- configureToolchains verbosity distDirLayout (fst (PD.ignoreConditions projectConfigSkeleton) <> cliConfig)
                -- The project configuration is always done with the host compiler
                let Toolchain{toolchainCompiler = compiler, toolchainPlatform = Platform arch os} = getStage toolchains Host
                return (os, arch, compiler)

          (projectConfig, _compiler) <- instantiateProjectConfigSkeletonFetchingCompiler fetchCompiler mempty projectConfigSkeleton
          when (projectConfigDistDir (projectConfigShared $ projectConfig) /= NoFlag) $
            liftIO $
              warn verbosity "The builddir option is not supported in project and config files. It will be ignored."
          localPackages <- phaseReadLocalPackages (projectConfig <> cliConfig)
          return (projectConfig, localPackages)

    informAboutConfigFiles projectConfig

    return (projectConfig <> cliConfig, localPackages)
    where
      ProjectConfigShared{projectConfigProjectFileParser, projectConfigIgnoreProject, projectConfigConfigFile, projectConfigToolchain} =
        projectConfigShared cliConfig

      PackageConfig{packageConfigProgramPaths, packageConfigProgramPathExtra} =
        projectConfigLocalPackages cliConfig

      -- Read the cabal.project (or implicit config) and combine it with
      -- arguments from the command line

      configFileParser = fromFlagOrDefault defaultProjectFileParser projectConfigProjectFileParser
      --
      phaseReadProjectConfig :: Rebuild ProjectConfigSkeleton
      phaseReadProjectConfig = do
        readProjectConfig verbosity configFileParser httpTransport projectConfigIgnoreProject projectConfigConfigFile distDirLayout

      -- Look for all the cabal packages in the project
      -- some of which may be local src dirs, tarballs etc
      --
      -- NOTE: These are all packages mentioned in the project configuration.
      -- Whether or not they will be considered local to the project will be decided by `shouldBeLocal`.
      phaseReadLocalPackages
        :: ProjectConfig
        -> Rebuild [PackageSpecifier UnresolvedSourcePackage]
      phaseReadLocalPackages
        projectConfig@ProjectConfig
          { projectConfigShared
          , projectConfigBuildOnly
          } = do
          pkgLocations <- findProjectPackages distDirLayout projectConfig
          -- Create folder only if findProjectPackages did not throw a
          -- BadPackageLocations exception.
          liftIO $ do
            createDirectoryIfMissingVerbose verbosity True distDirectory
            createDirectoryIfMissingVerbose verbosity True distProjectCacheDirectory

          fetchAndReadSourcePackages
            verbosity
            distDirLayout
            projectConfigShared
            projectConfigBuildOnly
            pkgLocations

      informAboutConfigFiles projectConfig = do
        cwd <- getCurrentDirectory
        let out -- output mode is verbose ('notice') if we build outside the project root
              | cwd == distProjectRootDirectory = info
              | otherwise = notice
        unless (null configFiles)
          . out (verboseStderr verbosity)
          . render
          $ message
        where
          -- message formatting depends on |config files| (the number of config files)
          message = case configFilesDoc of
            (_ : _ : _ : _) ->
              -- if |config files| > 2 then use vertical list
              vcat
                [ affectedByMsg <> text "the following files:"
                , configFilesVertList
                , atProjectRootMsg
                ]
            [path1, path2] ->
              affectedByMsg <> path1 <> text " and " <> (path2 <+> atProjectRootMsg)
            [path] ->
              affectedByMsg <> (path <+> atProjectRootMsg)
            [] ->
              error "impossible" -- see `unless (null configFiles)` above
            where
              configFilesDoc = map (quoteUntrimmed . projectConfigPathRoot) configFiles
              configFilesVertList -- if verbose, include provenance ("imported by" stuff)
                | verbosity < verbose = docProjectConfigFiles configFiles
                | otherwise = vcat $ map (\p -> text "- " <> docProjectConfigPath p) configFiles
              affectedByMsg = text "Configuration is affected by "
              atProjectRootMsg = text "at '" <> text distProjectRootDirectory <> text "'."

          configFiles =
            [ path
            | Explicit path <-
                Set.toList
                  . (if verbosity >= verbose then id else onlyTopLevelProvenance)
                  $ projectConfigProvenance projectConfig
            ]

configureToolchains
  :: Verbosity
  -> DistDirLayout
  -> ProjectConfig
  -> Rebuild Toolchains
configureToolchains
  verbosity
  DistDirLayout
    { distProjectCacheFile
    }
  ProjectConfig
    { projectConfigShared =
      ProjectConfigShared
        { projectConfigToolchain =
          ProjectConfigToolchain
            { projectConfigHcFlavor
            , projectConfigHcPath
            , projectConfigHcPkg
            , projectConfigPackageDBs
            , projectConfigBuildHcFlavor
            , projectConfigBuildHcPath
            , projectConfigBuildHcPkg
            , projectConfigBuildPackageDBs
            }
        , projectConfigProgPathExtra
        }
    , projectConfigLocalPackages =
      PackageConfig
        { packageConfigProgramPaths
        , packageConfigProgramPathExtra
        }
    } = do
    let fileMonitorBuildCompiler = newFileMonitor $ distProjectCacheFile "build-compiler"
        fileMonitorHostCompiler = newFileMonitor $ distProjectCacheFile "host-compiler"

    progsearchpath <- liftIO $ getSystemSearchPath

    (buildHc, buildPlat, buildHcProgDb) <-
      rerunIfChanged
        verbosity
        fileMonitorBuildCompiler
        ( buildHcFlavor
        , buildHcPath
        , buildHcPkg
        , progsearchpath
        , packageConfigProgramPaths
        , packageConfigProgramPathExtra
        )
        $ do
          liftIO $ info verbosity "Compiler settings changed, reconfiguring..."
          progdb <-
            liftIO $
              -- Add paths in the global config
              prependProgramSearchPath verbosity (fromNubList projectConfigProgPathExtra) [] defaultProgramDb
                -- Add paths in the local config
                >>= prependProgramSearchPath verbosity (fromNubList packageConfigProgramPathExtra) []
                >>= pure . userSpecifyPaths (Map.toList (getMapLast packageConfigProgramPaths))
          result@(_, _, progdb') <-
            liftIO $
              Cabal.configCompiler
                buildHcFlavor
                buildHcPath
                progdb
                verbosity
          -- Note that we added the user-supplied program locations and args
          -- for /all/ programs, not just those for the compiler prog and
          -- compiler-related utils. In principle we don't know which programs
          -- the compiler will configure (and it does vary between compilers).
          -- We do know however that the compiler will only configure the
          -- programs it cares about, and those are the ones we monitor here.
          monitorFiles (programsMonitorFiles progdb')
          return result

    (hostHc, hostPlat, hostHcProgDb) <-
      rerunIfChanged
        verbosity
        fileMonitorHostCompiler
        ( hostHcFlavor
        , hostHcPath
        , hostHcPkg
        , progsearchpath
        , packageConfigProgramPaths
        , packageConfigProgramPathExtra
        )
        $ do
          liftIO $ info verbosity "Compiler settings changed, reconfiguring..."
          progdb <-
            liftIO $
              -- Add paths in the global config
              prependProgramSearchPath verbosity (fromNubList projectConfigProgPathExtra) [] defaultProgramDb
                -- Add paths in the local config
                >>= prependProgramSearchPath verbosity (fromNubList packageConfigProgramPathExtra) []
                >>= pure . userSpecifyPaths (Map.toList (getMapLast packageConfigProgramPaths))
          result@(_, _, progdb') <-
            liftIO $
              Cabal.configCompiler
                hostHcFlavor
                hostHcPath
                progdb
                verbosity
          -- Note that we added the user-supplied program locations and args
          -- for /all/ programs, not just those for the compiler prog and
          -- compiler-related utils. In principle we don't know which programs
          -- the compiler will configure (and it does vary between compilers).
          -- We do know however that the compiler will only configure the
          -- programs it cares about, and those are the ones we monitor here.
          monitorFiles (programsMonitorFiles progdb')
          return result

    -- Now, **outside** of the caching logic of 'rerunIfChanged', add on
    -- auxiliary unconfigured programs to the ProgramDb (e.g. hc-pkg, haddock, ar, ld...).
    --
    -- See Note [Caching the result of configuring the compiler]
    finalBuildProgDb <- liftIO $ Cabal.configCompilerProgDb verbosity buildHc buildHcProgDb buildHcPkg
    finalHostProgDb <- liftIO $ Cabal.configCompilerProgDb verbosity hostHc hostHcProgDb hostHcPkg

    return $ Staged $ \case
      Build ->
        Toolchain
          { toolchainCompiler = buildHc
          , toolchainPlatform = buildPlat
          , toolchainProgramDb = finalBuildProgDb
          , toolchainPackageDBs = Cabal.interpretPackageDbFlags False projectConfigBuildPackageDBs
          }
      Host ->
        Toolchain
          { toolchainCompiler = hostHc
          , toolchainPlatform = hostPlat
          , toolchainProgramDb = finalHostProgDb
          , toolchainPackageDBs = Cabal.interpretPackageDbFlags False projectConfigPackageDBs
          }
    where
      hostHcFlavor = flagToMaybe projectConfigHcFlavor
      hostHcPath = flagToMaybe projectConfigHcPath
      hostHcPkg = flagToMaybe projectConfigHcPkg
      -- Use the host compiler if a separate build compiler is not specified
      buildHcFlavor = flagToMaybe projectConfigBuildHcFlavor <|> flagToMaybe projectConfigHcFlavor
      buildHcPath = flagToMaybe projectConfigBuildHcPath <|> flagToMaybe projectConfigHcPath
      buildHcPkg = flagToMaybe projectConfigBuildHcPkg <|> flagToMaybe projectConfigHcPkg

{- Note [Caching the result of configuring the compiler]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can't straightforwardly cache anything that contains a 'ProgramDb', because
the 'Binary' instance for 'ProgramDb' discards all unconfigured programs.
See that instance, as well as 'restoreProgramDb', for a few more details.

This means that if we try to cache the result of configuring the compiler (which
contains a 'ProgramDb'):

 - On the first run, we will obtain a 'ProgramDb' which may contain several
   unconfigured programs. In particular, configuring GHC will add tools such
   as `ar` and `ld` as unconfigured programs to the 'ProgramDb', with custom
   logic for finding their location based on the location of the GHC binary
   and its associated settings file.
 - On subsequent runs, if we use the cache created by 'rerunIfChanged', we will
   deserialise the 'ProgramDb' from disk, which means it won't include any
   unconfigured programs, which might mean we are unable to find 'ar' or 'ld'.

To solve this, we cache the ProgramDb containing the compiler (which will be
a configured program, hence properly serialised/deserialised), and then
re-compute any attendant unconfigured programs (such as hc-pkg, haddock or build
tools such as ar, ld) using 'configCompilerProgDb'.

Another idea would be to simply eagerly configure all unconfigured programs,
as was originally attempted. But this doesn't work, for a couple of reasons:

 - it does more work than necessary, by configuring programs that we may not
   end up needing,
 - it means that we prioritise system executables for built-in build tools
   (such as `alex` and `happy`), instead of using the proper version for a
   package or package component, as specified by a `build-tool-depends` stanza
   or by package-level `extra-prog-path` arguments.
   This lead to bug reports #10633 and #10692.

See #9840 for more information about the problems surrounding the lossy
'Binary ProgramDb' instance.
-}

------------------------------------------------------------------------------

-- * Deciding what to do: making an 'ElaboratedInstallPlan'

------------------------------------------------------------------------------

-- | Return an up-to-date elaborated install plan.
--
-- Two variants of the install plan are returned: with and without packages
-- from the store. That is, the \"improved\" plan where source packages are
-- replaced by pre-existing installed packages from the store (when their ids
-- match), and also the original elaborated plan which uses primarily source
-- packages.

-- The improved plan is what we use for building, but the original elaborated
-- plan is useful for reporting and configuration. For example the @freeze@
-- command needs the source package info to know about flag choices and
-- dependencies of executables and setup scripts.
--
rebuildInstallPlan
  :: HasCallStack
  => Verbosity
  -> DistDirLayout
  -> CabalDirLayout
  -> ProjectConfig
  -> [PackageSpecifier UnresolvedSourcePackage]
  -> Maybe InstalledPackageIndex
  -> IO
      ( ElaboratedInstallPlan -- with store packages
      , ElaboratedInstallPlan -- with source packages
      , ElaboratedSharedConfig
      , IndexUtils.TotalIndexState
      , IndexUtils.ActiveRepos
      )
  -- ^ @(improvedPlan, elaboratedPlan, _, _, _)@
rebuildInstallPlan
  verbosity
  distDirLayout@DistDirLayout
    { distProjectRootDirectory
    , distProjectCacheFile
    }
  CabalDirLayout
    { cabalStoreDirLayout
    } = \projectConfig localPackages mbInstalledPackages ->
    runRebuild distProjectRootDirectory $ do
      progsearchpath <- liftIO $ getSystemSearchPath
      let projectConfigMonitored = projectConfig{projectConfigBuildOnly = mempty}

      -- The overall improved plan is cached
      rerunIfChanged
        verbosity
        fileMonitorImprovedPlan
        -- react to changes in the project config,
        -- the package .cabal files and the path
        (projectConfigMonitored, localPackages, progsearchpath)
        $ do
          -- And so is the elaborated plan that the improved plan based on
          (elaboratedPlan, elaboratedShared, totalIndexState, activeRepos) <-
            rerunIfChanged
              verbosity
              fileMonitorElaboratedPlan
              ( projectConfigMonitored
              , localPackages
              , progsearchpath
              )
              $ do
                toolchains <- phaseConfigureToolchains projectConfig
                phaseConfigurePrograms projectConfig toolchains
                (solverPlan, _, pkgConfigDBs, totalIndexState, activeRepos) <-
                  phaseRunSolver
                    projectConfig
                    toolchains
                    localPackages
                    (fromMaybe mempty mbInstalledPackages)

                (elaboratedPlan, elaboratedShared) <-
                  phaseElaboratePlan
                    projectConfig
                    toolchains
                    pkgConfigDBs
                    solverPlan
                    localPackages

                phaseMaintainPlanOutputs elaboratedPlan elaboratedShared
                return (elaboratedPlan, elaboratedShared, totalIndexState, activeRepos)

          -- \| Given the 'InstalledPackageIndex' for a nix-style package store, and an
          -- 'ElaboratedInstallPlan', replace configured source packages by installed
          -- packages from the store whenever they exist.
          --
          -- The improved plan changes each time we install something, whereas
          -- the underlying elaborated plan only changes when input config
          -- changes, so it's worth caching them separately.
          improvedPlan <- phaseImprovePlan elaboratedPlan elaboratedShared

          return (improvedPlan, elaboratedPlan, elaboratedShared, totalIndexState, activeRepos)
    where
      fileMonitorSolverPlan = newFileMonitorInCacheDir "solver-plan"
      fileMonitorSourceHashes = newFileMonitorInCacheDir "source-hashes"
      fileMonitorElaboratedPlan = newFileMonitorInCacheDir "elaborated-plan"
      fileMonitorImprovedPlan = newFileMonitorInCacheDir "improved-plan"

      newFileMonitorInCacheDir :: Eq a => FilePath -> FileMonitor a b
      newFileMonitorInCacheDir = newFileMonitor . distProjectCacheFile

      -- Configure the compiler we're using.

      -- This is moderately expensive and doesn't change that often so we cache
      -- it independently.
      --
      phaseConfigureToolchains
        :: ProjectConfig
        -> Rebuild Toolchains
      phaseConfigureToolchains projectConfig = do
        toolchains <- configureToolchains verbosity distDirLayout projectConfig
        liftIO $ do
          notice verbosity "Toolchains:"
          for_ stages $ \s ->
            notice verbosity $ show $ Disp.hsep [Disp.text "-" <+> pretty s <+> Disp.text "compiler" <+> pretty (compilerId (toolchainCompiler (getStage toolchains s)))]
        return toolchains

      -- Configuring other programs.
      --
      -- Having configured the compiler, now we configure all the remaining
      -- programs. This is to check we can find them, and to monitor them for
      -- changes.
      --
      -- TODO: [required eventually] we don't actually do this yet.
      --
      -- We rely on the fact that the previous phase added the program config for
      -- all local packages, but that all the programs configured so far are the
      -- compiler program or related util programs.
      --
      phaseConfigurePrograms
        :: ProjectConfig
        -> Toolchains
        -> Rebuild ()
      phaseConfigurePrograms projectConfig toolchains = do
        -- Users are allowed to specify program locations independently for
        -- each package (e.g. to use a particular version of a pre-processor
        -- for some packages). However they cannot do this for the compiler
        -- itself as that's just not going to work. So we check for this.
        for_ toolchains $ \Toolchain{toolchainProgramDb} ->
          liftIO $
            checkBadPerPackageCompilerPaths
              (configuredPrograms toolchainProgramDb)
              (getMapMappend (projectConfigSpecificPackage projectConfig))

      -- TODO: [required eventually] find/configure other programs that the
      -- user specifies.

      -- TODO: [required eventually] find/configure all build-tools
      -- but note that some of them may be built as part of the plan.

      -- Run the solver to get the initial install plan.
      -- This is expensive so we cache it independently.
      --
      phaseRunSolver
        :: ProjectConfig
        -> Toolchains
        -> [PackageSpecifier UnresolvedSourcePackage]
        -> InstalledPackageIndex
        -> Rebuild
            ( SolverInstallPlan
            , Staged InstalledPackageIndex
            , Staged (Maybe PkgConfigDb)
            , IndexUtils.TotalIndexState
            , IndexUtils.ActiveRepos
            )
      phaseRunSolver
        projectConfig@ProjectConfig
          { projectConfigShared
          , projectConfigBuildOnly
          }
        toolchains
        localPackages
        _installedPackages =
          rerunIfChanged
            verbosity
            fileMonitorSolverPlan
            ( solverSettings
            , localPackages
            , localPackagesEnabledStanzas
            , toolchains
            )
            $ do
              (sourcePkgDb, tis, ar) <-
                getSourcePackages
                  verbosity
                  withRepoCtx
                  (solverSettingIndexState solverSettings)
                  (solverSettingActiveRepos solverSettings)

              ipis <- for toolchains (getInstalledPackages verbosity)
              pkgConfigDbs <- for toolchains (getPkgConfigDb verbosity . toolchainProgramDb)

              -- TODO: [code cleanup] it'd be better if the Compiler contained the
              -- ConfiguredPrograms that it needs, rather than relying on the progdb
              -- since we don't need to depend on all the programs here, just the
              -- ones relevant for the compiler.

              liftIO $ do
                notice verbosity "Resolving dependencies..."
                planOrError <-
                  foldProgress logMsg (pure . Left) (pure . Right) $
                    planPackages
                      verbosity
                      solverSettings
                      compilerAndPlatform
                      pkgConfigDbs
                      ipis
                      sourcePkgDb
                      localPackages
                      localPackagesEnabledStanzas
                case planOrError of
                  Left msg -> do
                    -- TODO
                    for_ toolchains $ \(Toolchain{toolchainCompiler, toolchainPlatform}) ->
                      reportPlanningFailure projectConfig toolchainCompiler toolchainPlatform localPackages
                    dieWithException verbosity $ PhaseRunSolverErr msg
                  Right plan -> return (plan, ipis, pkgConfigDbs, tis, ar)
          where
            compilerAndPlatform =
              fmap
                (\Toolchain{toolchainCompiler, toolchainPlatform} -> (compilerInfo toolchainCompiler, toolchainPlatform))
                toolchains

            withRepoCtx :: (RepoContext -> IO a) -> IO a
            withRepoCtx =
              projectConfigWithSolverRepoContext
                verbosity
                projectConfigShared
                projectConfigBuildOnly

            solverSettings = resolveSolverSettings projectConfig
            logMsg message rest = debugNoWrap verbosity message >> rest

            localPackagesEnabledStanzas =
              Map.fromList
                [ (pkgname, stanzas)
                | pkg <- localPackages
                , -- TODO: misnomer: we should separate
                -- builtin/global/inplace/local packages
                -- and packages explicitly mentioned in the project
                --
                let pkgname = pkgSpecifierTarget pkg
                    testsEnabled =
                      lookupLocalPackageConfig
                        packageConfigTests
                        projectConfig
                        pkgname
                    benchmarksEnabled =
                      lookupLocalPackageConfig
                        packageConfigBenchmarks
                        projectConfig
                        pkgname
                    isLocal = isJust (shouldBeLocal pkg)
                    stanzas
                      | isLocal =
                          Map.fromList $
                            [ (TestStanzas, enabled)
                            | enabled <- flagToList testsEnabled
                            ]
                              ++ [ (BenchStanzas, enabled)
                                 | enabled <- flagToList benchmarksEnabled
                                 ]
                      | otherwise = Map.fromList [(TestStanzas, False), (BenchStanzas, False)]
                ]

      -- Elaborate the solver's install plan to get a fully detailed plan. This
      -- version of the plan has the final nix-style hashed ids.
      --
      phaseElaboratePlan
        :: HasCallStack
        => ProjectConfig
        -> Staged Toolchain
        -> Staged (Maybe PkgConfigDb)
        -> SolverInstallPlan
        -> [PackageSpecifier (SourcePackage (PackageLocation loc))]
        -> Rebuild
            ( ElaboratedInstallPlan
            , ElaboratedSharedConfig
            )
      phaseElaboratePlan
        ProjectConfig
          { projectConfigShared
          , projectConfigAllPackages
          , projectConfigLocalPackages
          , projectConfigSpecificPackage
          , projectConfigBuildOnly
          }
        toolchains
        pkgConfigDB
        solverPlan
        localPackages = do
          liftIO $ debug verbosity "Elaborating the install plan..."

          sourcePackageHashes <-
            rerunIfChanged
              verbosity
              fileMonitorSourceHashes
              (packageLocationsSignature solverPlan)
              $ getPackageSourceHashes verbosity withRepoCtx solverPlan

          installDirs <-
            for toolchains $ \t -> do
              defaultInstallDirs <- liftIO $ userInstallDirTemplates (toolchainCompiler t)
              return $ fmap Cabal.fromFlag $ (fmap Flag defaultInstallDirs) <> (projectConfigInstallDirs projectConfigShared)

          liftIO $ runLogProgress verbosity $ do
            (elaboratedPlan, elaboratedShared) <-
              elaborateInstallPlan
                verbosity
                toolchains
                pkgConfigDB
                distDirLayout
                cabalStoreDirLayout
                solverPlan
                localPackages
                sourcePackageHashes
                installDirs
                projectConfigShared
                projectConfigAllPackages
                projectConfigLocalPackages
                (getMapMappend projectConfigSpecificPackage)

            instantiatedPlan <-
              instantiateInstallPlan
                cabalStoreDirLayout
                installDirs
                elaboratedShared
                elaboratedPlan

            infoProgress $ text "Elaborated install plan:" $$ text (showElaboratedInstallPlan instantiatedPlan)

            return (instantiatedPlan, elaboratedShared)
          where
            withRepoCtx :: (RepoContext -> IO a) -> IO a
            withRepoCtx =
              projectConfigWithSolverRepoContext
                verbosity
                projectConfigShared
                projectConfigBuildOnly

      -- Update the files we maintain that reflect our current build environment.
      -- In particular we maintain a JSON representation of the elaborated
      -- install plan (but not the improved plan since that reflects the state
      -- of the build rather than just the input environment).
      --
      phaseMaintainPlanOutputs
        :: ElaboratedInstallPlan
        -> ElaboratedSharedConfig
        -> Rebuild ()
      phaseMaintainPlanOutputs elaboratedPlan elaboratedShared = liftIO $ do
        debug verbosity "Updating plan.json"
        writePlanExternalRepresentation
          distDirLayout
          elaboratedPlan
          elaboratedShared

      -- Improve the elaborated install plan. The elaborated plan consists
      -- mostly of source packages (with full nix-style hashed ids). Where
      -- corresponding installed packages already exist in the store, replace
      -- them in the plan.
      --
      -- Note that we do monitor the store's package db here, so we will redo
      -- this improvement phase when the db changes -- including as a result of
      -- executing a plan and installing things.
      --
      phaseImprovePlan
        :: ElaboratedInstallPlan
        -> ElaboratedSharedConfig
        -> Rebuild ElaboratedInstallPlan
      phaseImprovePlan elaboratedPlan elaboratedShared = do
        liftIO $ debug verbosity "Improving the install plan..."
        improvedPlan <- liftIO $ InstallPlan.installedM canBeImproved elaboratedPlan
        liftIO $ debugNoWrap verbosity (showElaboratedInstallPlan improvedPlan)
        -- TODO: [nice to have] having checked which packages from the store
        -- we're using, it may be sensible to sanity check those packages
        -- by loading up the compiler package db and checking everything
        -- matches up as expected, e.g. no dangling deps, files deleted.
        return improvedPlan
        where
          canBeImproved pkg = do
            let Toolchain{toolchainCompiler} = getStage (pkgConfigToolchains elaboratedShared) (elabStage pkg)
            doesStoreEntryExist cabalStoreDirLayout toolchainCompiler (installedUnitId pkg)

-- | If a 'PackageSpecifier' refers to a single package, return Just that
-- package.
reportPlanningFailure :: ProjectConfig -> Compiler -> Platform -> [PackageSpecifier UnresolvedSourcePackage] -> IO ()
reportPlanningFailure projectConfig comp platform pkgSpecifiers =
  when reportFailure $
    BuildReports.storeLocal
      (compilerInfo comp)
      (fromNubList $ projectConfigSummaryFile . projectConfigBuildOnly $ projectConfig)
      buildReports
      platform
  where
    -- TODO may want to handle the projectConfigLogFile parameter here, or just remove it entirely?

    reportFailure = Cabal.fromFlag . projectConfigReportPlanningFailure . projectConfigBuildOnly $ projectConfig
    pkgids = mapMaybe theSpecifiedPackage pkgSpecifiers
    buildReports =
      BuildReports.fromPlanningFailure
        platform
        (compilerId comp)
        pkgids
        -- TODO we may want to get more flag assignments and merge them here?
        (packageConfigFlagAssignment . projectConfigAllPackages $ projectConfig)

    theSpecifiedPackage :: Package pkg => PackageSpecifier pkg -> Maybe PackageId
    theSpecifiedPackage pkgSpec =
      case pkgSpec of
        NamedPackage name [PackagePropertyVersion version] ->
          PackageIdentifier name <$> trivialRange version
        NamedPackage _ _ -> Nothing
        SpecificSourcePackage pkg -> Just $ packageId pkg
    -- \| If a range includes only a single version, return Just that version.
    trivialRange :: VersionRange -> Maybe Version
    trivialRange =
      foldVersionRange
        Nothing
        Just -- "== v"
        (\_ -> Nothing)
        (\_ -> Nothing)
        (\_ _ -> Nothing)
        (\_ _ -> Nothing)

programsMonitorFiles :: ProgramDb -> [MonitorFilePath]
programsMonitorFiles progdb =
  [ monitor
  | prog <- configuredPrograms progdb
  , monitor <-
      monitorFileSearchPath
        (programMonitorFiles prog)
        (programPath prog)
  ]

getInstalledPackages
  :: Verbosity
  -> Toolchain
  -> Rebuild InstalledPackageIndex
getInstalledPackages verbosity Toolchain{..} = do
  monitorFiles
    . map monitorFileOrDirectory
    =<< liftIO
      ( IndexUtils.getInstalledPackagesMonitorFiles
          verbosity
          toolchainCompiler
          Nothing -- use ambient working directory
          (coercePackageDBStack toolchainPackageDBs)
          toolchainProgramDb
          toolchainPlatform
      )
  liftIO $
    IndexUtils.getInstalledPackages
      verbosity
      toolchainCompiler
      toolchainPackageDBs
      toolchainProgramDb

{-
--TODO: [nice to have] use this but for sanity / consistency checking
getPackageDBContents :: Verbosity
                     -> Compiler -> ProgramDb -> Platform
                     -> PackageDB
                     -> Rebuild InstalledPackageIndex
getPackageDBContents verbosity compiler progdb platform packagedb = do
    monitorFiles . map monitorFileOrDirectory
      =<< liftIO (IndexUtils.getInstalledPackagesMonitorFiles
                    verbosity compiler
                    [packagedb] progdb platform)
    liftIO $ do
      createPackageDBIfMissing verbosity compiler progdb packagedb
      Cabal.getPackageDBContents verbosity compiler
                                 packagedb progdb
-}

getSourcePackages
  :: Verbosity
  -> (forall a. (RepoContext -> IO a) -> IO a)
  -> Maybe IndexUtils.TotalIndexState
  -> Maybe IndexUtils.ActiveRepos
  -> Rebuild (SourcePackageDb, IndexUtils.TotalIndexState, IndexUtils.ActiveRepos)
getSourcePackages verbosity withRepoCtx idxState activeRepos = do
  (sourcePkgDbWithTIS, repos) <-
    liftIO $
      withRepoCtx $
        \repoctx -> do
          sourcePkgDbWithTIS <- IndexUtils.getSourcePackagesAtIndexState verbosity repoctx idxState activeRepos
          return (sourcePkgDbWithTIS, repoContextRepos repoctx)

  traverse_ needIfExists
    . IndexUtils.getSourcePackagesMonitorFiles
    $ repos
  return sourcePkgDbWithTIS

getPkgConfigDb :: Verbosity -> ProgramDb -> Rebuild (Maybe PkgConfigDb)
getPkgConfigDb verbosity progdb = do
  dirs <- liftIO $ getPkgConfigDbDirs verbosity progdb
  -- Just monitor the dirs so we'll notice new .pc files.
  -- Alternatively we could monitor all the .pc files too.
  traverse_ monitorDirectoryStatus dirs
  liftIO $ readPkgConfigDb verbosity progdb

-- | Select the config values to monitor for changes package source hashes.
packageLocationsSignature
  :: SolverInstallPlan
  -> [(PackageId, PackageLocation (Maybe FilePath))]
packageLocationsSignature solverPlan =
  [ (packageId pkg, srcpkgSource pkg)
  | SolverInstallPlan.Configured (SolverPackage{solverPkgSource = pkg}) <-
      SolverInstallPlan.toList solverPlan
  ]

-- | Get the 'HashValue' for all the source packages where we use hashes,
-- and download any packages required to do so.
--
-- Note that we don't get hashes for local unpacked packages.
getPackageSourceHashes
  :: Verbosity
  -> (forall a. (RepoContext -> IO a) -> IO a)
  -> SolverInstallPlan
  -> Rebuild (Map PackageId PackageSourceHash)
getPackageSourceHashes verbosity withRepoCtx solverPlan = do
  -- Determine if and where to get the package's source hash from.
  --
  let allPkgLocations :: [(PackageId, PackageLocation (Maybe FilePath))]
      allPkgLocations =
        [ (packageId pkg, srcpkgSource pkg)
        | SolverInstallPlan.Configured (SolverPackage{solverPkgSource = pkg}) <-
            SolverInstallPlan.toList solverPlan
        ]

      -- Tarballs that were local in the first place.
      -- We'll hash these tarball files directly.
      localTarballPkgs :: [(PackageId, FilePath)]
      localTarballPkgs =
        [ (pkgid, tarball)
        | (pkgid, LocalTarballPackage tarball) <- allPkgLocations
        ]

      -- Tarballs from remote URLs. We must have downloaded these already
      -- (since we extracted the .cabal file earlier)
      remoteTarballPkgs =
        [ (pkgid, tarball)
        | (pkgid, RemoteTarballPackage _ (Just tarball)) <- allPkgLocations
        ]

      -- tarballs from source-repository-package stanzas
      sourceRepoTarballPkgs =
        [ (pkgid, tarball)
        | (pkgid, RemoteSourceRepoPackage _ (Just tarball)) <- allPkgLocations
        ]

      -- Tarballs from repositories, either where the repository provides
      -- hashes as part of the repo metadata, or where we will have to
      -- download and hash the tarball.
      repoTarballPkgsWithMetadataUnvalidated :: [(Repo, [PackageId])]
      repoTarballPkgsWithoutMetadata :: [(Repo, PackageId)]
      ( repoTarballPkgsWithMetadataUnvalidated
        , repoTarballPkgsWithoutMetadata
        ) =
          partitionEithers
            [ case repo of
              RepoSecure{} -> Left (repo, [pkgid])
              _ -> Right (repo, pkgid)
            | (pkgid, RepoTarballPackage repo _ _) <- allPkgLocations
            ]

      -- Group up the unvalidated packages by repo so we only read the remote
      -- index once per repo (see #10110). The packages are ungrouped here and then regrouped
      -- below, it would be better in future to refactor this whole code path so that we don't
      -- repeatedly group and ungroup.
      repoTarballPkgsWithMetadataUnvalidatedMap = Map.fromListWith (++) repoTarballPkgsWithMetadataUnvalidated

  (repoTarballPkgsWithMetadata, repoTarballPkgsToDownloadWithMeta) <- fmap partitionEithers $
    liftIO $
      withRepoCtx $ \repoctx ->
        flip concatMapM (Map.toList repoTarballPkgsWithMetadataUnvalidatedMap) $
          uncurry (verifyFetchedTarballs verbosity repoctx)

  -- For tarballs from repos that do not have hashes available we now have
  -- to check if the packages were downloaded already.
  --
  ( repoTarballPkgsToDownloadWithNoMeta
    , repoTarballPkgsDownloaded
    ) <-
    fmap partitionEithers $
      liftIO $
        sequence
          [ do
            mtarball <- checkRepoTarballFetched repo pkgid
            case mtarball of
              Nothing -> return (Left (repo, pkgid))
              Just tarball -> return (Right (pkgid, tarball))
          | (repo, pkgid) <- repoTarballPkgsWithoutMetadata
          ]

  let repoTarballPkgsToDownload = repoTarballPkgsToDownloadWithMeta ++ repoTarballPkgsToDownloadWithNoMeta
  ( hashesFromRepoMetadata
    , repoTarballPkgsNewlyDownloaded
    ) <-
    -- Avoid having to initialise the repository (ie 'withRepoCtx') if we
    -- don't have to. (The main cost is configuring the http client.)
    if null repoTarballPkgsToDownload && null repoTarballPkgsWithMetadata
      then return (Map.empty, [])
      else liftIO $ withRepoCtx $ \repoctx -> do
        -- For tarballs from repos that do have hashes available as part of the
        -- repo metadata we now load up the index for each repo and retrieve
        -- the hashes for the packages
        --
        hashesFromRepoMetadata <-
          Sec.uncheckClientErrors $
            fmap (Map.fromList . concat) $ -- TODO: [code cleanup] wrap in our own exceptions
              sequence
                -- Reading the repo index is expensive so we group the packages by repo
                [ repoContextWithSecureRepo repoctx repo $ \secureRepo ->
                  Sec.withIndex secureRepo $ \repoIndex ->
                    sequence
                      [ do
                        hash <-
                          Sec.trusted
                            <$> Sec.indexLookupHash repoIndex pkgid -- strip off Trusted tag

                        -- Note that hackage-security currently uses SHA256
                        -- but this API could in principle give us some other
                        -- choice in future.
                        return (pkgid, hashFromTUF hash)
                      | pkgid <- pkgids
                      ]
                | (repo, pkgids) <-
                    map (\grp@((repo, _) :| _) -> (repo, map snd (NE.toList grp)))
                      . NE.groupBy ((==) `on` (remoteRepoName . repoRemote . fst))
                      . sortBy (compare `on` (remoteRepoName . repoRemote . fst))
                      $ repoTarballPkgsWithMetadata
                ]

        -- For tarballs from repos that do not have hashes available, download
        -- the ones we previously determined we need.
        --
        repoTarballPkgsNewlyDownloaded <-
          sequence
            [ do
              tarball <- fetchRepoTarball verbosity repoctx repo pkgid
              return (pkgid, tarball)
            | (repo, pkgid) <- repoTarballPkgsToDownload
            ]

        return
          ( hashesFromRepoMetadata
          , repoTarballPkgsNewlyDownloaded
          )

  -- Hash tarball files for packages where we have to do that. This includes
  -- tarballs that were local in the first place, plus tarballs from repos,
  -- either previously cached or freshly downloaded.
  --
  let allTarballFilePkgs :: [(PackageId, FilePath)]
      allTarballFilePkgs =
        localTarballPkgs
          ++ remoteTarballPkgs
          ++ sourceRepoTarballPkgs
          ++ repoTarballPkgsDownloaded
          ++ repoTarballPkgsNewlyDownloaded
  hashesFromTarballFiles <-
    liftIO $
      fmap Map.fromList $
        sequence
          [ do
            srchash <- readFileHashValue tarball
            return (pkgid, srchash)
          | (pkgid, tarball) <- allTarballFilePkgs
          ]
  monitorFiles
    [ monitorFile tarball
    | (_pkgid, tarball) <- allTarballFilePkgs
    ]

  -- Return the combination
  return $!
    hashesFromRepoMetadata
      <> hashesFromTarballFiles

-- ------------------------------------------------------------

-- * Installation planning

-- ------------------------------------------------------------

planPackages
  :: Verbosity
  -> SolverSettings
  -> Staged (CompilerInfo, Platform)
  -> Staged (Maybe PkgConfigDb)
  -> Staged InstalledPackageIndex
  -> SourcePackageDb
  -> [PackageSpecifier UnresolvedSourcePackage]
  -> Map PackageName (Map OptionalStanza Bool)
  -> Progress String String SolverInstallPlan
planPackages
  verbosity
  SolverSettings{..}
  toolchains
  pkgConfigDbs
  installedPkgs
  sourcePkgs
  localPackages
  pkgStanzasEnable =
    resolveDependencies toolchains pkgConfigDbs installedPkgs resolverParams
    where
      -- TODO: [nice to have] disable multiple instances restriction in
      -- the solver, but then make sure we can cope with that in the
      -- output.
      resolverParams :: DepResolverParams
      resolverParams =
        setMaxBackjumps solverSettingMaxBackjumps
          . setReorderGoals solverSettingReorderGoals
          . setCountConflicts solverSettingCountConflicts
          . setFineGrainedConflicts solverSettingFineGrainedConflicts
          . setMinimizeConflictSet solverSettingMinimizeConflictSet
          -- TODO: [required eventually] should only be configurable for
          -- custom installs
          -- . setAvoidReinstalls solverSettingAvoidReinstalls

          -- TODO: [required eventually] should only be configurable for
          -- custom installs
          -- . setShadowPkgs solverSettingShadowPkgs

          . setStrongFlags solverSettingStrongFlags
          . setAllowBootLibInstalls solverSettingAllowBootLibInstalls
          . setOnlyConstrained solverSettingOnlyConstrained
          . setSolverVerbosity verbosity
          -- TODO: [required eventually] decide if we need to prefer
          -- installed for global packages, or prefer latest even for
          -- global packages. Perhaps should be configurable but with a
          -- different name than "upgrade-dependencies".
          . setPreferenceDefault
            ( if Cabal.asBool solverSettingPreferOldest
                then PreferAllOldest
                else PreferLatestForSelected
            )
          {-(if solverSettingUpgradeDeps
               then PreferAllLatest
               else PreferLatestForSelected)-}

          . removeLowerBounds solverSettingAllowOlder
          . removeUpperBounds solverSettingAllowNewer
          --
          -- TODO: These need to be per compiler. We should be able to do that
          -- when we can use the stage as a solver scope
          --
          -- . addDefaultSetupDependencies
          --   ( mkDefaultSetupDeps compiler platform
          --       . PD.packageDescription
          --       . srcpkgDescription
          --   )
          -- . addSetupCabalMinVersionConstraint setupMinCabalVersionConstraint
          -- . addSetupCabalMaxVersionConstraint setupMaxCabalVersionConstraint
          --
          . addPreferences
            -- preferences from the config file or command line
            [ PackageVersionPreference name ver
            | PackageVersionConstraint name ver <- solverSettingPreferences
            ]
          . addConstraints
            -- version constraints from the config file or command line
            [ LabeledPackageConstraint (userToPackageConstraint pc) src
            | (pc, src) <- solverSettingConstraints
            ]
          . addPreferences
            -- enable stanza preference unilaterally, regardless if the user asked
            -- accordingly or expressed no preference, to help hint the solver
            [ PackageStanzasPreference pkgname stanzas
            | pkg <- localPackages
            , let pkgname = pkgSpecifierTarget pkg
                  stanzaM = Map.findWithDefault Map.empty pkgname pkgStanzasEnable
                  stanzas =
                    [ stanza | stanza <- [minBound .. maxBound], Map.lookup stanza stanzaM /= Just False
                    ]
            , not (null stanzas)
            ]
          . addConstraints
            -- Enable stanza constraints where the user asked to enable
            -- Only applies to the host stage.
            -- TODO: Disable test and bench for build stage packages.
            [ LabeledPackageConstraint
              ( PackageConstraint
                  (scopeToplevel pkgname)
                  (PackagePropertyStanzas stanzas)
              )
              ConstraintSourceConfigFlagOrTarget
            | pkg <- localPackages
            , let pkgname = pkgSpecifierTarget pkg
                  stanzaM = Map.findWithDefault Map.empty pkgname pkgStanzasEnable
                  stanzas =
                    [ stanza | stanza <- [minBound .. maxBound], Map.lookup stanza stanzaM == Just True
                    ]
            , not (null stanzas)
            ]
          . addConstraints
            -- TODO: [nice to have] should have checked at some point that the
            -- package in question actually has these flags.
            [ LabeledPackageConstraint
              ( PackageConstraint
                  (scopeToplevel pkgname)
                  (PackagePropertyFlags flags)
              )
              ConstraintSourceConfigFlagOrTarget
            | (pkgname, flags) <- Map.toList solverSettingFlagAssignments
            ]
          . addConstraints
            -- TODO: [nice to have] we have user-supplied flags for unspecified
            -- local packages (as well as specific per-package flags). For the
            -- former we just apply all these flags to all local targets which
            -- is silly. We should check if the flags are appropriate.
            [ LabeledPackageConstraint
              ( PackageConstraint
                  (scopeToplevel pkgname)
                  (PackagePropertyFlags flags)
              )
              ConstraintSourceConfigFlagOrTarget
            | let flags = solverSettingFlagAssignment
            , not (PD.nullFlagAssignment flags)
            , pkg <- localPackages
            , let pkgname = pkgSpecifierTarget pkg
            ]
          $ stdResolverParams

      stdResolverParams :: DepResolverParams
      stdResolverParams =
        -- Note: we don't use the standardInstallPolicy here, since that uses
        -- its own addDefaultSetupDependencies that is not appropriate for us.
        basicInstallPolicy
          sourcePkgs
          localPackages

------------------------------------------------------------------------------

-- * Install plan post-processing

------------------------------------------------------------------------------

-- This phase goes from the InstallPlan we get from the solver and has to
-- make an elaborated install plan.
--
-- We go in two steps:
--
--  1. elaborate all the source packages that the solver has chosen.
--  2. swap source packages for pre-existing installed packages wherever
--     possible.
--
-- We do it in this order, elaborating and then replacing, because the easiest
-- way to calculate the installed package ids used for the replacement step is
-- from the elaborated configuration for each package.

------------------------------------------------------------------------------

-- * Install plan elaboration

------------------------------------------------------------------------------

-- Note [SolverId to ConfiguredId]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Dependency solving is a per package affair, so after we're done, we
-- end up with 'SolverInstallPlan' that records in 'solverPkgLibDeps'
-- and 'solverPkgExeDeps' what packages provide the libraries and executables
-- needed by each component of the package (phew!)  For example, if I have
--
--      library
--          build-depends: lib
--          build-tool-depends: pkg:exe1
--          build-tools: alex
--
-- After dependency solving, I find out that this library component has
-- library dependencies on lib-0.2, and executable dependencies on pkg-0.1
-- and alex-0.3 (other components of the package may have different
-- dependencies).  Note that I've "lost" the knowledge that I depend

-- * specifically* on the exe1 executable from pkg.

--
-- So, we have a this graph of packages, and we need to transform it into
-- a graph of components which we are actually going to build.  In particular:
--
-- NODE changes from PACKAGE (SolverPackage) to COMPONENTS (ElaboratedConfiguredPackage)
-- EDGE changes from PACKAGE DEP (SolverId) to COMPONENT DEPS (ConfiguredId)
--
-- In both cases, what was previously a single node/edge may turn into multiple
-- nodes/edges.  Multiple components, because there may be multiple components
-- in a package; multiple component deps, because we may depend upon multiple
-- executables from the same package (and maybe, some day, multiple libraries
-- from the same package.)
--
-- Let's talk about how to do this transformation. Naively, we might consider
-- just processing each package, converting it into (zero or) one or more
-- components.  But we also have to update the edges; this leads to
-- two complications:
--
--      1. We don't know what the ConfiguredId of a component is until
--      we've configured it, but we cannot configure a component unless
--      we know the ConfiguredId of all its dependencies.  Thus, we must
--      process the 'SolverInstallPlan' in topological order.
--
--      2. When we process a package, we know the SolverIds of its
--      dependencies, but we have to do some work to turn these into
--      ConfiguredIds.  For example, in the case of build-tool-depends, the
--      SolverId isn't enough to uniquely determine the ConfiguredId we should
--      elaborate to: we have to look at the executable name attached to
--      the package name in the package description to figure it out.
--      At the same time, we NEED to use the SolverId, because there might
--      be multiple versions of the same package in the build plan
--      (due to setup dependencies); we can't just look up the package name
--      from the package description.
--
-- We can adopt the following strategy:
--
--      * When a package is transformed into components, record
--        a mapping from SolverId to ALL of the components
--        which were elaborated.
--
--      * When we look up an edge, we use our knowledge of the
--        component name to *filter* the list of components into
--        the ones we actually wanted to refer to.
--
-- By the way, we can tell that SolverInstallPlan is not the "right" type
-- because a SolverId cannot adequately represent all possible dependency
-- solver states: we may need to record foo-0.1 multiple times in
-- the solver install plan with different dependencies.  This imprecision in the
-- type currently doesn't cause any problems because the dependency solver
-- continues to enforce the single instance restriction regardless of compiler
-- version.  The right way to solve this is to come up with something very much
-- like a 'ConfiguredId', in that it incorporates the version choices of its
-- dependencies, but less fine grained.

-- | Produce an elaborated install plan using the policy for local builds with
-- a nix-style shared store.
--
-- In theory should be able to make an elaborated install plan with a policy
-- matching that of the classic @cabal install --user@ or @--global@
elaborateInstallPlan
  :: HasCallStack
  => Verbosity
  -> Staged Toolchain
  -> Staged (Maybe PkgConfigDb)
  -> DistDirLayout
  -> StoreDirLayout
  -> SolverInstallPlan
  -> [PackageSpecifier (SourcePackage (PackageLocation loc))]
  -> Map PackageId PackageSourceHash
  -> Staged InstallDirs.InstallDirTemplates
  -> ProjectConfigShared
  -> PackageConfig
  -> PackageConfig
  -> Map PackageName PackageConfig
  -> LogProgress (ElaboratedInstallPlan, ElaboratedSharedConfig)
elaborateInstallPlan
  verbosity
  toolchains
  pkgConfigDB
  distDirLayout@DistDirLayout{..}
  storeDirLayout
  solverPlan
  localPackages
  sourcePackageHashes
  defaultInstallDirs
  sharedPackageConfig
  allPackagesConfig
  localPackagesConfig
  perPackageConfig = do
    x <- elaboratedInstallPlan
    return (x, elaboratedSharedConfig)
    where
      elaboratedSharedConfig =
        ElaboratedSharedConfig
          { pkgConfigToolchains = toolchains
          , pkgConfigReplOptions = mempty
          }

      preexistingInstantiatedPkgs :: Map UnitId FullUnitId
      preexistingInstantiatedPkgs =
        Map.fromList (mapMaybe f (SolverInstallPlan.toList solverPlan))
        where
          f (SolverInstallPlan.PreExisting inst)
            | let ipkg = instSolverPkgIPI inst
            , not (IPI.indefinite ipkg) =
                Just
                  ( IPI.installedUnitId ipkg
                  , ( FullUnitId
                        (IPI.installedComponentId ipkg)
                        (Map.fromList (IPI.instantiatedWith ipkg))
                    )
                  )
          f _ = Nothing

      elaboratedInstallPlan :: HasCallStack => LogProgress ElaboratedInstallPlan
      elaboratedInstallPlan =
        flip InstallPlan.fromSolverInstallPlanWithProgress solverPlan $ \mapDep planpkg ->
          case planpkg of
            SolverInstallPlan.PreExisting pkg ->
              return [InstallPlan.PreExisting (WithStage (instSolverStage pkg) (instSolverPkgIPI pkg))]
            SolverInstallPlan.Configured pkg ->
              let inplace_doc
                    | shouldBuildInplaceOnly pkg = text "inplace"
                    | otherwise = Disp.empty
               in addProgressCtx
                    ( text "In the"
                        <+> inplace_doc
                        <+> text "package"
                        <+> quotes (pretty (packageId pkg))
                    )
                    $ map InstallPlan.Configured <$> elaborateSolverToComponents mapDep pkg

      -- NB: We don't INSTANTIATE packages at this point.  That's
      -- a post-pass.  This makes it simpler to compute dependencies.
      elaborateSolverToComponents
        :: HasCallStack
        => (SolverId -> [ElaboratedPlanPackage])
        -> SolverPackage UnresolvedPkgLoc
        -> LogProgress [ElaboratedConfiguredPackage]
      elaborateSolverToComponents
        mapDep
        solverPkg@SolverPackage{solverPkgStage, solverPkgLibDeps, solverPkgExeDeps} =
          case mkComponentsGraph (elabEnabledSpec elab0) pd of
            Left cns ->
              dieProgress $
                hang
                  (text "Dependency cycle between the following components:")
                  4
                  (vcat (map (text . componentNameStanza) cns))
            Right g -> do
              let src_comps = componentsGraphToList g

              infoProgress $
                hang
                  (text "Component graph for" <+> pretty (solverId (ResolverPackage.Configured solverPkg)))
                  4
                  (dispComponentsWithDeps src_comps)

              (_, comps) <-
                mapAccumM
                  buildComponent
                  (Map.empty, Map.empty, Map.empty)
                  (map fst src_comps)

              let whyNotPerComp = why_not_per_component src_comps

              case NE.nonEmpty whyNotPerComp of
                Nothing ->
                  return comps
                Just notPerCompReasons -> do
                  checkPerPackageOk comps notPerCompReasons
                  pkgComp <-
                    elaborateSolverToPackage
                      notPerCompReasons
                      solverPkg
                      g
                      (comps ++ maybeToList setupComponent)
                  return [pkgComp]
          where
            bt = PD.buildType (elabPkgDescription elab0)

            -- You are eligible to per-component build if this list is empty
            why_not_per_component g =
              cuz_buildtype ++ cuz_spec ++ cuz_length ++ cuz_flag
              where
                -- Custom and Hooks are not implemented. Implementing
                -- per-component builds with Custom would require us to create a
                -- new 'ElabSetup' type, and teach all of the code paths how to
                -- handle it.
                -- Once you've implemented this, swap it for the code below.
                cuz_buildtype =
                  case bt of
                    PD.Configure -> []
                    -- Configure is supported, but we only support configuring the
                    -- main library in cabal. Other components will need to depend
                    -- on the main library for configured data.
                    PD.Custom -> [CuzBuildType CuzCustomBuildType]
                    PD.Hooks -> [CuzBuildType CuzHooksBuildType]
                    PD.Make -> [CuzBuildType CuzMakeBuildType]
                    PD.Simple -> []
                -- cabal-format versions prior to 1.8 have different build-depends semantics
                -- for now it's easier to just fallback to legacy-mode when specVersion < 1.8
                -- see, https://github.com/haskell/cabal/issues/4121
                cuz_spec
                  | PD.specVersion pd >= CabalSpecV1_8 = []
                  | otherwise = [CuzCabalSpecVersion]
                -- In the odd corner case that a package has no components at all
                -- then keep it as a whole package, since otherwise it turns into
                -- 0 component graph nodes and effectively vanishes. We want to
                -- keep it around at least for error reporting purposes.
                cuz_length
                  | length g > 0 = []
                  | otherwise = [CuzNoBuildableComponents]
                -- For ease of testing, we let per-component builds be toggled
                -- at the top level
                cuz_flag
                  | fromFlagOrDefault True (projectConfigPerComponent sharedPackageConfig) =
                      []
                  | otherwise = [CuzDisablePerComponent]

            -- \| Sometimes a package may make use of features which are only
            -- supported in per-package mode.  If this is the case, we should
            -- give an error when this occurs.
            checkPerPackageOk comps reasons = do
              let is_sublib (CLibName (LSubLibName _)) = True
                  is_sublib _ = False
              when (any (matchElabPkg is_sublib) comps) $
                dieProgress $
                  text "Internal libraries only supported with per-component builds."
                    $$ text "Per-component builds were disabled because"
                    <+> fsep (punctuate comma $ map (text . whyNotPerComponent) $ toList reasons)
            -- TODO: Maybe exclude Backpack too

            elab0 = elaborateSolverToCommon solverPkg
            pkgid = elabPkgSourceId elab0
            pd = elabPkgDescription elab0

            -- TODO: This is just a skeleton to get elaborateSolverToPackage
            -- working correctly
            -- TODO: When we actually support building these components, we
            -- have to add dependencies on this from all other components
            setupComponent :: Maybe ElaboratedConfiguredPackage
            setupComponent
              | bt `elem` [PD.Custom, PD.Hooks] =
                  Just
                    elab0
                      { elabModuleShape = emptyModuleShape
                      , elabUnitId = notImpl "elabUnitId"
                      , elabComponentId = notImpl "elabComponentId"
                      , elabInstallDirs = notImpl "elabInstallDirs"
                      , elabPkgOrComp =
                          ElabComponent
                            ( ElaboratedComponent
                                { compSolverName = CD.ComponentSetup
                                , compComponentName = Nothing
                                , compLibDependencies =
                                    [ (configuredId cid, False)
                                    | cid <- CD.setupDeps solverPkgLibDeps >>= elaborateLibSolverId mapDep
                                    ]
                                , compLinkedLibDependencies = notImpl "compLinkedLibDependencies"
                                , compOrderLibDependencies = notImpl "compOrderLibDependencies"
                                , -- Not supported:
                                  compExeDependencies = mempty
                                , compExeDependencyPaths = mempty
                                , compPkgConfigDependencies = mempty
                                , compInstantiatedWith = mempty
                                , compLinkedInstantiatedWith = Map.empty
                                }
                            )
                      }
              | otherwise =
                  Nothing
              where
                notImpl f =
                  error $
                    "Distribution.Client.ProjectPlanning.setupComponent: "
                      ++ f
                      ++ " not implemented yet"

            -- Note: this function is used to configure the components in a single package (`elab`, defined in the outer scope)
            buildComponent
              :: HasCallStack
              => ( Map PackageName (Map ComponentName (AnnotatedId ComponentId))
                 , Map ComponentId (OpenUnitId, ModuleShape)
                 , Map ComponentId FilePath
                 )
              -> Cabal.Component
              -> LogProgress
                  ( ( Map PackageName (Map ComponentName (AnnotatedId ComponentId))
                    , Map ComponentId (OpenUnitId, ModuleShape)
                    , Map ComponentId FilePath
                    )
                  , ElaboratedConfiguredPackage
                  )
            buildComponent (cc_map, lc_map, exe_map) comp =
              addProgressCtx
                ( text "In the stanza"
                    <+> quotes (text (componentNameStanza cname))
                )
                $ do
                  let lib_dep_map = Map.unionWith Map.union external_lib_cc_map cc_map
                      -- TODO: is cc_map correct here?
                      exe_dep_map = Map.unionWith Map.union external_exe_cc_map cc_map

                  -- 1. Configure the component, but with a place holder ComponentId.
                  infoProgress $
                    hang (text "configuring component" <+> pretty cname) 4 $
                      vcat
                        [ text "lib_dep_map:" <+> Disp.hsep (punctuate comma $ map pretty (Map.keys lib_dep_map))
                        , text "exe_dep_map:" <+> Disp.hsep (punctuate comma $ map pretty (Map.keys exe_dep_map))
                        ]

                  cc0 <-
                    toConfiguredComponent
                      pd
                      (error "Distribution.Client.ProjectPlanning.cc_cid: filled in later")
                      lib_dep_map
                      exe_dep_map
                      comp

                  let do_ cid =
                        let cid' = annotatedIdToConfiguredId . ci_ann_id $ cid
                         in (cid', False) -- filled in later in pruneInstallPlanPhase2)

                  -- 2. Read out the dependencies from the ConfiguredComponent cc0
                  let compLibDependencies =
                        -- Nub because includes can show up multiple times
                        ordNub
                          ( map
                              (\cid -> do_ cid)
                              (cc_includes cc0)
                          )

                      compExeDependencies :: [WithStage ConfiguredId]
                      compExeDependencies =
                        -- External
                        [ WithStage (stageOf pkg) confId
                        | pkg <- external_exe_dep_pkgs
                        , let confId = configuredId pkg
                        , -- only executables
                        Just (CExeName _) <- [confCompName confId]
                        , confSrcId confId /= pkgid
                        ]
                          <>
                          -- Internal, assume the same stage
                          [ WithStage solverPkgStage confId
                          | aid <- cc_exe_deps cc0
                          , let confId = annotatedIdToConfiguredId aid
                          , confSrcId confId == pkgid
                          ]

                      compExeDependencyPaths :: [(WithStage ConfiguredId, FilePath)]
                      compExeDependencyPaths =
                        -- External
                        [ (WithStage (stageOf pkg) confId, path)
                        | pkg <- external_exe_dep_pkgs
                        , let confId = configuredId pkg
                        , confSrcId confId /= pkgid
                        , -- only executables
                        Just (CExeName _) <- [confCompName confId]
                        , path <- planPackageExePaths pkg
                        ]
                          <>
                          -- Internal, assume the same stage
                          [ (WithStage solverPkgStage confId, path)
                          | aid <- cc_exe_deps cc0
                          , let confId = annotatedIdToConfiguredId aid
                          , confSrcId confId == pkgid
                          , Just paths <- [Map.lookup (ann_id aid) exe_map1]
                          , path <- paths
                          ]

                      elab_comp =
                        ElaboratedComponent
                          { compSolverName
                          , compComponentName
                          , compLibDependencies
                          , compExeDependencies
                          , compPkgConfigDependencies
                          , compExeDependencyPaths
                          , compInstantiatedWith = Map.empty
                          , compLinkedInstantiatedWith = Map.empty
                          , -- filled later (in step 5)
                            compLinkedLibDependencies = error "buildComponent: compLinkedLibDependencies"
                          , compOrderLibDependencies = error "buildComponent: compOrderLibDependencies"
                          }

                  -- 3. Construct a preliminary ElaboratedConfiguredPackage,
                  -- and use this to compute the component ID.  Fix up cc_id
                  -- correctly.
                  let elab1 =
                        elab0
                          { elabPkgOrComp = ElabComponent elab_comp
                          }

                      -- This is where the component id is computed.
                      cid = case elabBuildStyle elab0 of
                        BuildInplaceOnly{} ->
                          mkComponentId $
                            case Cabal.componentNameString cname of
                              Nothing -> prettyShow pkgid
                              Just n -> prettyShow pkgid ++ "-" ++ prettyShow n
                        BuildAndInstall ->
                          hashedInstalledPackageId
                            ( packageHashInputs
                                elaboratedSharedConfig
                                elab1 -- knot tied
                            )

                      cc = cc0{cc_ann_id = fmap (const cid) (cc_ann_id cc0)}

                  -- 4. Perform mix-in linking
                  let lookup_uid def_uid =
                        case Map.lookup (unDefUnitId def_uid) preexistingInstantiatedPkgs of
                          Just full -> full
                          Nothing -> error ("lookup_uid: " ++ prettyShow def_uid)
                      lc_dep_map = Map.union external_lc_map lc_map
                  lc <-
                    toLinkedComponent
                      verbosity
                      False
                      -- \^ whether there are any "promised" package dependencies which we won't find already installed
                      lookup_uid
                      -- \^ full db
                      (elabPkgSourceId elab0)
                      -- \^ the source package id
                      lc_dep_map
                      -- \^ linked component map
                      cc
                  -- \^ configured component

                  -- NB: elab is setup to be the correct form for an
                  -- indefinite library, or a definite library with no holes.
                  -- We will modify it in 'instantiateInstallPlan' to handle
                  -- instantiated packages.

                  -- 5. Construct the final ElaboratedConfiguredPackage
                  let
                    elab2 =
                      elab1
                        { elabModuleShape = lc_shape lc
                        , elabUnitId = abstractUnitId (lc_uid lc)
                        , elabComponentId = lc_cid lc
                        , elabPkgOrComp =
                            ElabComponent $
                              elab_comp
                                { compLinkedLibDependencies =
                                    ordNub (map ci_id (lc_includes lc))
                                , compOrderLibDependencies =
                                    ordNub
                                      ( map
                                          (abstractUnitId . ci_id)
                                          (lc_includes lc ++ lc_sig_includes lc)
                                      )
                                , compLinkedInstantiatedWith =
                                    Map.fromList (lc_insts lc)
                                }
                        }
                    elab =
                      elab2
                        { elabInstallDirs =
                            computeInstallDirs
                              storeDirLayout
                              defaultInstallDirs
                              elaboratedSharedConfig
                              elab2
                        }

                  -- 6. Construct the updated local maps
                  let cc_map' = extendConfiguredComponentMap cc cc_map
                      lc_map' = extendLinkedComponentMap lc lc_map
                      exe_map' = Map.insert cid (inplace_bin_dir elab) exe_map

                  return ((cc_map', lc_map', exe_map'), elab)
              where
                cname = Cabal.componentName comp
                compComponentName = Just cname
                compSolverName = CD.componentNameToComponent cname

                -- External dependencies. I.e. dependencies of the component on components of other packages.
                external_lib_dep_pkgs = concatMap mapDep $ CD.select (== compSolverName) solverPkgLibDeps

                external_exe_dep_pkgs = concatMap mapDep $ CD.select (== compSolverName) solverPkgExeDeps

                external_exe_map =
                  Map.fromList $
                    [ (getComponentId pkg, planPackageExePaths pkg)
                    | pkg <- external_exe_dep_pkgs
                    ]

                exe_map1 = Map.union external_exe_map $ fmap (\x -> [x]) exe_map

                external_lib_cc_map =
                  Map.fromListWith Map.union $
                    map mkCCMapping external_lib_dep_pkgs

                external_exe_cc_map =
                  Map.fromListWith Map.union $
                    map mkCCMapping external_exe_dep_pkgs

                external_lc_map =
                  Map.fromList $
                    map mkShapeMapping $
                      external_lib_dep_pkgs ++ external_exe_dep_pkgs

                compPkgConfigDependencies =
                  [ ( pn
                    , fromMaybe
                        ( error $
                            "compPkgConfigDependencies: impossible! "
                              ++ prettyShow pn
                              ++ " from "
                              ++ prettyShow (elabPkgSourceId elab0)
                        )
                        (getStage pkgConfigDB (elabStage elab0) >>= \db -> pkgConfigDbPkgVersion db pn)
                    )
                  | PkgconfigDependency pn _ <-
                      PD.pkgconfigDepends
                        (Cabal.componentBuildInfo comp)
                  ]

                inplace_bin_dir elab =
                  binDirectoryFor
                    distDirLayout
                    elaboratedSharedConfig
                    elab
                    $ case Cabal.componentNameString cname of
                      Just n -> prettyShow n
                      Nothing -> ""

      -- \| Given a 'SolverId' referencing a dependency on a library, return
      -- the 'ElaboratedPlanPackage' corresponding to the library.  This
      -- returns at most one result.
      elaborateLibSolverId
        :: (SolverId -> [ElaboratedPlanPackage])
        -> SolverId
        -> [ElaboratedPlanPackage]
      elaborateLibSolverId mapDep = filter (matchPlanPkg (== (CLibName LMainLibName))) . mapDep

      -- \| Given an 'ElaboratedPlanPackage', return the paths to where the
      -- executables that this package represents would be installed.
      -- The only case where multiple paths can be returned is the inplace
      -- monolithic package one, since there can be multiple exes and each one
      -- has its own directory.
      planPackageExePaths :: ElaboratedPlanPackage -> [FilePath]
      planPackageExePaths =
        -- Pre-existing executables are assumed to be in PATH
        -- already.  In fact, this should be impossible.
        InstallPlan.foldPlanPackage (const []) $ \elab ->
          let
            executables :: [FilePath]
            executables =
              case elabPkgOrComp elab of
                -- Monolithic mode: all exes of the package
                ElabPackage _ ->
                  unUnqualComponentName . PD.exeName
                    <$> PD.executables (elabPkgDescription elab)
                -- Per-component mode: just the selected exe
                ElabComponent comp ->
                  case fmap
                    Cabal.componentNameString
                    (compComponentName comp) of
                    Just (Just n) -> [prettyShow n]
                    _ -> [""]
           in
            binDirectoryFor
              distDirLayout
              elaboratedSharedConfig
              elab
              <$> executables

      elaborateSolverToPackage
        :: NE.NonEmpty NotPerComponentReason
        -> SolverPackage UnresolvedPkgLoc
        -> ComponentsGraph
        -> [ElaboratedConfiguredPackage]
        -> LogProgress ElaboratedConfiguredPackage
      elaborateSolverToPackage
        pkgWhyNotPerComponent
        solverPkg@SolverPackage{solverPkgSource = SourcePackage{srcpkgPackageId}}
        compGraph
        comps = do
          -- Knot tying: the final elab includes the
          -- pkgInstalledId, which is calculated by hashing many
          -- of the other fields of the elaboratedPackage.
          return elab
          where
            elab0@ElaboratedConfiguredPackage
              { elabPkgSourceHash
              , elabStanzasRequested
              , elabStage
              } = elaborateSolverToCommon solverPkg

            elab1 =
              elab0
                { elabUnitId = newSimpleUnitId pkgInstalledId
                , elabComponentId = pkgInstalledId
                , elabPkgOrComp = ElabPackage elabPkg
                , elabModuleShape = modShape
                }

            elab =
              elab1
                { elabInstallDirs =
                    computeInstallDirs
                      storeDirLayout
                      defaultInstallDirs
                      elaboratedSharedConfig
                      elab1
                }

            modShape = case find (matchElabPkg (== (CLibName LMainLibName))) comps of
              Nothing -> emptyModuleShape
              Just e -> Ty.elabModuleShape e

            pkgInstalledId
              | shouldBuildInplaceOnly solverPkg =
                  mkComponentId (prettyShow srcpkgPackageId)
              | otherwise =
                  assert (isJust elabPkgSourceHash) $
                    hashedInstalledPackageId
                      ( packageHashInputs
                          elaboratedSharedConfig
                          elab -- recursive use of elab
                      )

            -- Need to filter out internal dependencies, because they don't
            -- correspond to anything real anymore.
            isExternal confid = confSrcId confid /= srcpkgPackageId
            isExternal' (WithStage stage confId) = stage /= elabStage || isExternal confId

            elabPkg =
              ElaboratedPackage
                { pkgStage = elabStage
                , pkgInstalledId
                , pkgLibDependencies = buildComponentDeps (filter (isExternal . fst) . compLibDependencies)
                , pkgDependsOnSelfLib
                , pkgExeDependencies = buildComponentDeps (filter isExternal' . compExeDependencies)
                , pkgExeDependencyPaths = buildComponentDeps (filter (isExternal' . fst) . compExeDependencyPaths)
                , -- Why is this flat?
                  pkgPkgConfigDependencies = CD.flatDeps $ buildComponentDeps compPkgConfigDependencies
                , -- NB: This is not the final setting of 'pkgStanzasEnabled'.
                  -- See [Sticky enabled testsuites]; we may enable some extra
                  -- stanzas opportunistically when it is cheap to do so.
                  --
                  -- However, we start off by enabling everything that was
                  -- requested, so that we can maintain an invariant that
                  -- pkgStanzasEnabled is a superset of elabStanzasRequested
                  pkgStanzasEnabled = optStanzaKeysFilteredByValue (fromMaybe False) elabStanzasRequested
                , pkgWhyNotPerComponent
                }

            -- This tells us which components depend on the main library of this package.
            -- Note: the sublib case should not occur, because sub-libraries are not
            -- supported without per-component builds.
            -- TODO: Add a check somewhere that this is the case.
            pkgDependsOnSelfLib :: CD.ComponentDeps [()]
            pkgDependsOnSelfLib =
              CD.fromList
                [ (CD.componentNameToComponent cn, [()])
                | Graph.N _ cn _ <- closure
                ]
              where
                closure =
                  fromMaybe (error "elaborateSolverToPackage: internal error, no closure for main lib") $
                    Graph.revClosure compGraph [k | k@(CLibName LMainLibName) <- Graph.keys compGraph]

            buildComponentDeps :: Monoid a => (ElaboratedComponent -> a) -> CD.ComponentDeps a
            buildComponentDeps f =
              CD.fromList
                [ (compSolverName comp, f comp)
                | ElaboratedConfiguredPackage{elabPkgOrComp = ElabComponent comp} <- comps
                ]

      elaborateSolverToCommon
        :: SolverPackage UnresolvedPkgLoc
        -> ElaboratedConfiguredPackage
      elaborateSolverToCommon
        solverPkg@SolverPackage
          { solverPkgStage
          , solverPkgSource =
            SourcePackage
              { srcpkgPackageId
              , srcpkgDescription
              , srcpkgSource
              , srcpkgDescrOverride
              }
          , solverPkgFlags
          , solverPkgStanzas
          , solverPkgLibDeps
          } =
          elaboratedPackage
          where
            compilers = fmap toolchainCompiler toolchains
            platforms = fmap toolchainPlatform toolchains
            programDbs = fmap toolchainProgramDb toolchains
            packageDbs = fmap toolchainPackageDBs toolchains

            elaboratedPackage = ElaboratedConfiguredPackage{..}

            -- These get filled in later
            elabUnitId = error "elaborateSolverToCommon: elabUnitId"
            elabComponentId = error "elaborateSolverToCommon: elabComponentId"
            elabPkgOrComp = error "elaborateSolverToCommon: elabPkgOrComp"
            elabInstallDirs = error "elaborateSolverToCommon: elabInstallDirs"
            elabModuleShape = error "elaborateSolverToCommon: elabModuleShape"

            elabIsCanonical = True
            elabPkgSourceId = srcpkgPackageId

            elabStage = solverPkgStage
            elabCompiler = getStage compilers elabStage
            elabPlatform = getStage platforms elabStage
            elabProgramDb = getStage programDbs elabStage

            elabPkgDescription =
              case PD.finalizePD
                solverPkgFlags
                elabEnabledSpec
                (const Satisfied)
                elabPlatform
                (compilerInfo elabCompiler)
                []
                srcpkgDescription of
                Right (desc, _) -> desc
                Left _ -> error "Failed to finalizePD in elaborateSolverToCommon"

            elabFlagAssignment = solverPkgFlags

            elabFlagDefaults =
              PD.mkFlagAssignment
                [ (PD.flagName flag, PD.flagDefault flag)
                | flag <- PD.genPackageFlags srcpkgDescription
                ]

            elabEnabledSpec = enableStanzas solverPkgStanzas
            elabStanzasAvailable = solverPkgStanzas

            elabStanzasRequested :: OptionalStanzaMap (Maybe Bool)
            elabStanzasRequested = optStanzaTabulate $ \o -> case o of
              -- NB: even if a package stanza is requested, if the package
              -- doesn't actually have any of that stanza we omit it from
              -- the request, to ensure that we don't decide that this
              -- package needs to be rebuilt.  (It needs to be done here,
              -- because the ElaboratedConfiguredPackage is where we test
              -- whether or not there have been changes.)
              TestStanzas -> listToMaybe [v | v <- maybeToList tests, _ <- PD.testSuites elabPkgDescription]
              BenchStanzas -> listToMaybe [v | v <- maybeToList benchmarks, _ <- PD.benchmarks elabPkgDescription]
              where
                tests, benchmarks :: Maybe Bool
                tests = perPkgOptionMaybe srcpkgPackageId packageConfigTests
                benchmarks = perPkgOptionMaybe srcpkgPackageId packageConfigBenchmarks

            -- This is a placeholder which will get updated by 'pruneInstallPlanPass1'
            -- and 'pruneInstallPlanPass2'.  We can't populate it here
            -- because whether or not tests/benchmarks should be enabled
            -- is heuristically calculated based on whether or not the
            -- dependencies of the test suite have already been installed,
            -- but this function doesn't know what is installed (since
            -- we haven't improved the plan yet), so we do it in another pass.
            -- Check the comments of those functions for more details.
            elabConfigureTargets = []
            elabBuildTargets = []
            elabTestTargets = []
            elabBenchTargets = []
            elabReplTarget = []
            elabHaddockTargets = []

            elabBuildHaddocks =
              perPkgOptionFlag srcpkgPackageId False packageConfigDocumentation

            -- `documentation: true` should imply `-haddock` for GHC
            addHaddockIfDocumentationEnabled :: ConfiguredProgram -> ConfiguredProgram
            addHaddockIfDocumentationEnabled cp@ConfiguredProgram{..} =
              if programId == "ghc" && elabBuildHaddocks
                then cp{programOverrideArgs = "-haddock" : programOverrideArgs}
                else cp

            elabPkgSourceLocation = srcpkgSource

            elabPkgSourceHash = Map.lookup srcpkgPackageId sourcePackageHashes

            elabLocalToProject = isLocalToProject solverPkg

            elabBuildStyle =
              if shouldBuildInplaceOnly solverPkg
                then BuildInplaceOnly OnDisk
                else BuildAndInstall

            elabPackageDbs = getStage packageDbs elabStage
            elabBuildPackageDBStack = buildAndRegisterDbs elabStage
            elabRegisterPackageDBStack = buildAndRegisterDbs elabStage

            elabSetupScriptStyle = packageSetupScriptStyle elabPkgDescription

            elabSetupScriptCliVersion =
              packageSetupScriptSpecVersion
                elabSetupScriptStyle
                elabPkgDescription
                libDepGraph
                solverPkgLibDeps

            elabSetupPackageDBStack = buildAndRegisterDbs (prevStage elabStage)

            -- Same as corePackageDbs but with the addition of the in-place packagedb.
            inplacePackageDbs stage = corePackageDbs stage ++ [SpecificPackageDB (distDirectory </> "packagedb" </> prettyShow stage </> prettyShow (compilerId (getStage compilers stage)))]

            -- The project packagedbs (typically the global packagedb but others can be added) followed by the store.
            corePackageDbs stage = getStage packageDbs stage ++ [storePackageDB storeDirLayout (getStage compilers stage)]

            elabInplaceBuildPackageDBStack = inplacePackageDbs elabStage
            elabInplaceRegisterPackageDBStack = inplacePackageDbs elabStage
            elabInplaceSetupPackageDBStack = inplacePackageDbs (prevStage elabStage)

            buildAndRegisterDbs stage
              | shouldBuildInplaceOnly solverPkg = inplacePackageDbs stage
              | otherwise = corePackageDbs stage

            elabPkgDescriptionOverride = srcpkgDescrOverride

            elabBuildOptions =
              LBC.BuildOptions
                { withVanillaLib = perPkgOptionFlag srcpkgPackageId True packageConfigVanillaLib -- TODO: [required feature]: also needs to be handled recursively
                , withSharedLib = srcpkgPackageId `Set.member` pkgsUseSharedLibrary elabCompiler
                , withStaticLib = perPkgOptionFlag srcpkgPackageId False packageConfigStaticLib
                , withDynExe =
                    perPkgOptionFlag srcpkgPackageId False packageConfigDynExe
                      -- We can't produce a dynamic executable if the user
                      -- wants to enable executable profiling but the
                      -- compiler doesn't support prof+dyn.
                      && (okProfDyn || not profExe)
                , withFullyStaticExe = perPkgOptionFlag srcpkgPackageId False packageConfigFullyStaticExe
                , withGHCiLib = perPkgOptionFlag srcpkgPackageId False packageConfigGHCiLib -- TODO: [required feature] needs to default to enabled on windows still
                , withProfExe = perPkgOptionFlag srcpkgPackageId False packageConfigProf
                , withProfLib = srcpkgPackageId `Set.member` pkgsUseProfilingLibrary
                , withProfLibShared = srcpkgPackageId `Set.member` pkgsUseProfilingLibraryShared
                , exeCoverage = perPkgOptionFlag srcpkgPackageId False packageConfigCoverage
                , libCoverage = perPkgOptionFlag srcpkgPackageId False packageConfigCoverage
                , withOptimization = perPkgOptionFlag srcpkgPackageId NormalOptimisation packageConfigOptimization
                , splitObjs = perPkgOptionFlag srcpkgPackageId False packageConfigSplitObjs
                , splitSections = perPkgOptionFlag srcpkgPackageId False packageConfigSplitSections
                , stripLibs = perPkgOptionFlag srcpkgPackageId False packageConfigStripLibs
                , stripExes = perPkgOptionFlag srcpkgPackageId False packageConfigStripExes
                , withDebugInfo = perPkgOptionFlag srcpkgPackageId NoDebugInfo packageConfigDebugInfo
                , relocatable = perPkgOptionFlag srcpkgPackageId False packageConfigRelocatable
                , withProfLibDetail = elabProfExeDetail
                , withProfExeDetail = elabProfLibDetail
                }
            okProfDyn = profilingDynamicSupportedOrUnknown elabCompiler
            profExe = perPkgOptionFlag srcpkgPackageId False packageConfigProf

            ( elabProfExeDetail
              , elabProfLibDetail
              ) =
                perPkgOptionLibExeFlag
                  srcpkgPackageId
                  ProfDetailDefault
                  packageConfigProfDetail
                  packageConfigProfLibDetail

            elabDumpBuildInfo = perPkgOptionFlag srcpkgPackageId NoDumpBuildInfo packageConfigDumpBuildInfo

            -- Combine the configured compiler prog settings with the user-supplied
            -- config. For the compiler progs any user-supplied config was taken
            -- into account earlier when configuring the compiler so its ok that
            -- our configured settings for the compiler override the user-supplied
            -- config here.
            elabProgramPaths =
              Map.fromList
                [ (programId prog, programPath prog)
                | prog <- configuredPrograms elabProgramDb
                ]
                <> perPkgOptionMapLast srcpkgPackageId packageConfigProgramPaths

            elabProgramArgs =
              Map.unionWith
                (++)
                ( Map.fromList
                    [ (programId prog, args)
                    | prog <- configuredPrograms elabProgramDb
                    , let args = programOverrideArgs $ addHaddockIfDocumentationEnabled prog
                    , not (null args)
                    ]
                )
                (perPkgOptionMapMappend srcpkgPackageId packageConfigProgramArgs)

            elabProgramPathExtra = perPkgOptionNubList srcpkgPackageId packageConfigProgramPathExtra
            elabConfiguredPrograms = configuredPrograms elabProgramDb
            elabConfigureScriptArgs = perPkgOptionList srcpkgPackageId packageConfigConfigureArgs

            elabExtraLibDirs = perPkgOptionList srcpkgPackageId packageConfigExtraLibDirs
            elabExtraLibDirsStatic = perPkgOptionList srcpkgPackageId packageConfigExtraLibDirsStatic
            elabExtraFrameworkDirs = perPkgOptionList srcpkgPackageId packageConfigExtraFrameworkDirs
            elabExtraIncludeDirs = perPkgOptionList srcpkgPackageId packageConfigExtraIncludeDirs

            elabProgPrefix = perPkgOptionMaybe srcpkgPackageId packageConfigProgPrefix
            elabProgSuffix = perPkgOptionMaybe srcpkgPackageId packageConfigProgSuffix

            elabHaddockHoogle = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockHoogle
            elabHaddockHtml = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockHtml
            elabHaddockHtmlLocation = perPkgOptionMaybe srcpkgPackageId packageConfigHaddockHtmlLocation
            elabHaddockForeignLibs = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockForeignLibs
            elabHaddockForHackage = perPkgOptionFlag srcpkgPackageId Cabal.ForDevelopment packageConfigHaddockForHackage
            elabHaddockExecutables = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockExecutables
            elabHaddockTestSuites = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockTestSuites
            elabHaddockBenchmarks = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockBenchmarks
            elabHaddockInternal = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockInternal
            elabHaddockCss = perPkgOptionMaybe srcpkgPackageId packageConfigHaddockCss
            elabHaddockLinkedSource = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockLinkedSource
            elabHaddockQuickJump = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockQuickJump
            elabHaddockHscolourCss = perPkgOptionMaybe srcpkgPackageId packageConfigHaddockHscolourCss
            elabHaddockContents = perPkgOptionMaybe srcpkgPackageId packageConfigHaddockContents
            elabHaddockIndex = perPkgOptionMaybe srcpkgPackageId packageConfigHaddockIndex
            elabHaddockBaseUrl = perPkgOptionMaybe srcpkgPackageId packageConfigHaddockBaseUrl
            elabHaddockResourcesDir = perPkgOptionMaybe srcpkgPackageId packageConfigHaddockResourcesDir
            elabHaddockOutputDir = perPkgOptionMaybe srcpkgPackageId packageConfigHaddockOutputDir
            elabHaddockUseUnicode = perPkgOptionFlag srcpkgPackageId False packageConfigHaddockUseUnicode

            elabTestMachineLog = perPkgOptionMaybe srcpkgPackageId packageConfigTestMachineLog
            elabTestHumanLog = perPkgOptionMaybe srcpkgPackageId packageConfigTestHumanLog
            elabTestShowDetails = perPkgOptionMaybe srcpkgPackageId packageConfigTestShowDetails
            elabTestKeepTix = perPkgOptionFlag srcpkgPackageId False packageConfigTestKeepTix
            elabTestWrapper = perPkgOptionMaybe srcpkgPackageId packageConfigTestWrapper
            elabTestFailWhenNoTestSuites = perPkgOptionFlag srcpkgPackageId False packageConfigTestFailWhenNoTestSuites
            elabTestTestOptions = perPkgOptionList srcpkgPackageId packageConfigTestTestOptions

            elabBenchmarkOptions = perPkgOptionList srcpkgPackageId packageConfigBenchmarkOptions

      perPkgOptionFlag :: PackageId -> a -> (PackageConfig -> Flag a) -> a
      perPkgOptionMaybe :: PackageId -> (PackageConfig -> Flag a) -> Maybe a
      perPkgOptionList :: PackageId -> (PackageConfig -> [a]) -> [a]

      perPkgOptionFlag pkgid def f = fromFlagOrDefault def (lookupPerPkgOption pkgid f)
      perPkgOptionMaybe pkgid f = flagToMaybe (lookupPerPkgOption pkgid f)
      perPkgOptionList pkgid f = lookupPerPkgOption pkgid f
      perPkgOptionNubList pkgid f = fromNubList (lookupPerPkgOption pkgid f)
      perPkgOptionMapLast pkgid f = getMapLast (lookupPerPkgOption pkgid f)
      perPkgOptionMapMappend pkgid f = getMapMappend (lookupPerPkgOption pkgid f)

      perPkgOptionLibExeFlag pkgid def fboth flib = (exe, lib)
        where
          exe = fromFlagOrDefault def bothflag
          lib = fromFlagOrDefault def (bothflag <> libflag)
          bothflag = lookupPerPkgOption pkgid fboth
          libflag = lookupPerPkgOption pkgid flib

      lookupPerPkgOption
        :: (Package pkg, Monoid m)
        => pkg
        -> (PackageConfig -> m)
        -> m
      lookupPerPkgOption pkg f =
        -- This is where we merge the options from the project config that
        -- apply to all packages, all project local packages, and to specific
        -- named packages
        global `mappend` local `mappend` perpkg
        where
          global = f allPackagesConfig
          local
            | isLocalToProject pkg =
                f localPackagesConfig
            | otherwise =
                mempty
          perpkg = maybe mempty f (Map.lookup (packageName pkg) perPackageConfig)

      -- For this local build policy, every package that lives in a local source
      -- dir (as opposed to a tarball), or depends on such a package, will be
      -- built inplace into a shared dist dir. Tarball packages that depend on
      -- source dir packages will also get unpacked locally.
      shouldBuildInplaceOnly :: SolverPackage loc -> Bool
      shouldBuildInplaceOnly pkg =
        Set.member
          (solverId (ResolverPackage.Configured pkg))
          pkgsToBuildInplaceOnly

      -- The reverse dependencies of solver packages which match a package id in pkgLocalToProject.
      pkgsToBuildInplaceOnly :: Set SolverId
      pkgsToBuildInplaceOnly =
        Set.fromList
          [ solverId pkg
          | spkg <- SolverInstallPlan.toList solverPlan
          , packageId spkg `elem` pkgsLocalToProject
          , pkg <- SolverInstallPlan.reverseDependencyClosure solverPlan [solverId spkg]
          ]

      isLocalToProject :: Package pkg => pkg -> Bool
      isLocalToProject pkg =
        Set.member
          (packageId pkg)
          pkgsLocalToProject

      pkgsLocalToProject :: Set PackageId
      pkgsLocalToProject =
        Set.fromList (catMaybes (map shouldBeLocal localPackages))
      -- TODO: localPackages is a misnomer, it's all project packages
      -- here is where we decide which ones will be local!

      pkgsUseSharedLibrary :: Compiler -> Set PackageId
      pkgsUseSharedLibrary compiler =
        packagesWithLibDepsDownwardClosedProperty (needsSharedLib compiler)

      needsSharedLib compiler pkgid =
        fromMaybe
          compilerShouldUseSharedLibByDefault
          -- Case 1: --enable-shared or --disable-shared is passed explicitly, honour that.
          ( case pkgSharedLib of
              Just v -> Just v
              Nothing -> case pkgDynExe of
                -- case 2: If --enable-executable-dynamic is passed then turn on
                -- shared library generation.
                Just True ->
                  -- Case 3: If --enable-profiling is passed, then we are going to
                  -- build profiled dynamic, so no need for shared libraries.
                  case pkgProf of
                    Just True -> Nothing
                    _ -> Just True
                -- But don't necessarily turn off shared library generation if
                -- --disable-executable-dynamic is passed. The shared objects might
                -- be needed for something different.
                _ -> Nothing
          )
        where
          pkgSharedLib = perPkgOptionMaybe pkgid packageConfigSharedLib
          pkgDynExe = perPkgOptionMaybe pkgid packageConfigDynExe
          pkgProf = perPkgOptionMaybe pkgid packageConfigProf

          compilerShouldUseSharedLibByDefault =
            case compilerFlavor compiler of
              GHC -> GHC.compilerBuildWay compiler == DynWay && canBuildSharedLibs
              GHCJS -> GHCJS.isDynamic compiler
              _ -> False

          canBuildWayLibs predicate = case predicate compiler of
            Just can_build -> can_build
            -- If we don't know for certain, just assume we can
            -- which matches behaviour in previous cabal releases
            Nothing -> True

          canBuildSharedLibs = canBuildWayLibs dynamicSupported


      pkgsUseProfilingLibrary :: Set PackageId
      pkgsUseProfilingLibrary =
        packagesWithLibDepsDownwardClosedProperty needsProfilingLib

      needsProfilingLib pkg =
        fromFlagOrDefault False (profBothFlag <> profLibFlag)
        where
          pkgid = packageId pkg
          profBothFlag = lookupPerPkgOption pkgid packageConfigProf
          profLibFlag = lookupPerPkgOption pkgid packageConfigProfLib

      pkgsUseProfilingLibraryShared :: Set PackageId
      pkgsUseProfilingLibraryShared =
        packagesWithLibDepsDownwardClosedProperty needsProfilingLibShared

      needsProfilingLibShared pkg =
        fromMaybe
          -- FIXME
          -- compilerShouldUseProfilingSharedLibByDefault
          False
          -- case 1: If --enable-profiling-shared is passed explicitly, honour that
          ( case profLibSharedFlag of
              Just v -> Just v
              Nothing -> case pkgDynExe of
                Just True ->
                  case pkgProf of
                    -- case 2: --enable-executable-dynamic + --enable-profiling
                    -- turn on shared profiling libraries
                    Just True -> Just True
                    _ -> Nothing
                -- But don't necessarily turn off shared library generation is
                -- --disable-executable-dynamic is passed. The shared objects might
                -- be needed for something different.
                _ -> Nothing
          )
        where
          pkgid = packageId pkg
          profLibSharedFlag = perPkgOptionMaybe pkgid packageConfigProfShared
          pkgDynExe = perPkgOptionMaybe pkgid packageConfigDynExe
          pkgProf = perPkgOptionMaybe pkgid packageConfigProf

      -- TODO: [code cleanup] unused: the old deprecated packageConfigProfExe

      libDepGraph =
        Graph.fromDistinctList $
          map
            NonSetupLibDepSolverPlanPackage
            (SolverInstallPlan.toList solverPlan)

      packagesWithLibDepsDownwardClosedProperty :: (PackageIdentifier -> Bool) -> Set PackageIdentifier
      packagesWithLibDepsDownwardClosedProperty property =
        Set.fromList
          . map packageId
          . fromMaybe []
          $ Graph.closure
            libDepGraph
            [ Graph.nodeKey pkg
            | pkg <- SolverInstallPlan.toList solverPlan
            , property (packageId pkg) -- just the packages that satisfy the property
            -- TODO: [nice to have] this does not check the config consistency,
            -- e.g. a package explicitly turning off profiling, but something
            -- depending on it that needs profiling. This really needs a separate
            -- package config validation/resolution pass.
            ]

-- TODO: [nice to have] config consistency checking:
-- + profiling libs & exes, exe needs lib, recursive
-- + shared libs & exes, exe needs lib, recursive
-- + vanilla libs & exes, exe needs lib, recursive
-- + ghci or shared lib needed by TH, recursive, ghc version dependent

-- TODO: Drop matchPlanPkg/matchElabPkg in favor of mkCCMapping

shouldBeLocal :: PackageSpecifier (SourcePackage (PackageLocation loc)) -> Maybe PackageId
shouldBeLocal (NamedPackage _ _) =
  Nothing
shouldBeLocal (SpecificSourcePackage pkg) =
  case srcpkgSource pkg of
    LocalUnpackedPackage _ -> Just (packageId pkg)
    _ -> Nothing

-- | Given a 'ElaboratedPlanPackage', report if it matches a 'ComponentName'.
-- TODO: check the role of stage here.
matchPlanPkg :: (ComponentName -> Bool) -> ElaboratedPlanPackage -> Bool
matchPlanPkg p = InstallPlan.foldPlanPackage (\(WithStage _stage ipkg) -> p (ipiComponentName ipkg)) (matchElabPkg p)

-- | Get the appropriate 'ComponentName' which identifies an installed
-- component.
ipiComponentName :: IPI.InstalledPackageInfo -> ComponentName
ipiComponentName = CLibName . IPI.sourceLibName

-- | Given a 'ElaboratedConfiguredPackage', report if it matches a
-- 'ComponentName'.
matchElabPkg :: (ComponentName -> Bool) -> ElaboratedConfiguredPackage -> Bool
matchElabPkg p elab =
  case elabPkgOrComp elab of
    ElabComponent comp -> maybe False p (compComponentName comp)
    ElabPackage _ ->
      -- So, what should we do here?  One possibility is to
      -- unconditionally return 'True', because whatever it is
      -- that we're looking for, it better be in this package.
      -- But this is a bit dodgy if the package doesn't actually
      -- have, e.g., a library.  Fortunately, it's not possible
      -- for the build of the library/executables to be toggled
      -- by 'pkgStanzasEnabled', so the only thing we have to
      -- test is if the component in question is *buildable.*
      any
        (p . componentName)
        (Cabal.pkgBuildableComponents (elabPkgDescription elab))

-- | Extract from an 'ElaboratedPlanPackage' a mapping from package and component name
-- to a component id.
mkCCMapping
  :: ElaboratedPlanPackage
  -> (PackageName, Map ComponentName (AnnotatedId ComponentId))
mkCCMapping =
  InstallPlan.foldPlanPackage
    ( \(WithStage _ ipkg) ->
        ( packageName ipkg
        , Map.singleton
            (ipiComponentName ipkg)
            -- TODO: libify
            ( AnnotatedId
                { ann_id = IPI.installedComponentId ipkg
                , ann_pid = packageId ipkg
                , ann_cname = IPI.sourceComponentName ipkg
                }
            )
        )
    )
    $ \elab ->
      let mk_aid cn =
            AnnotatedId
              { ann_id = elabComponentId elab
              , ann_pid = packageId elab
              , ann_cname = cn
              }
       in ( packageName elab
          , case elabPkgOrComp elab of
              ElabComponent comp ->
                case compComponentName comp of
                  -- This should be an error because we cannot explicitly depend on a setup
                  Nothing -> Map.empty
                  Just n -> Map.singleton n (mk_aid n)
              ElabPackage _ ->
                Map.fromList $
                  map
                    (\comp -> let cn = Cabal.componentName comp in (cn, mk_aid cn))
                    -- Shouldn't this be available in ElaboratedPackage?
                    (Cabal.pkgBuildableComponents (elabPkgDescription elab))
          )

-- | Given an 'ElaboratedPlanPackage', generate the mapping from 'ComponentId'
-- to the shape of this package, as per mix-in linking.
mkShapeMapping
  :: ElaboratedPlanPackage
  -> (ComponentId, (OpenUnitId, ModuleShape))
mkShapeMapping dpkg =
  (getComponentId dpkg, (indef_uid, shape))
  where
    (dcid, shape) =
      InstallPlan.foldPlanPackage
        (\(WithStage _stage ipkg) -> (IPI.installedComponentId ipkg, shapeInstalledPackage ipkg))
        (\elab -> (elabComponentId elab, elabModuleShape elab))
        dpkg
    indef_uid =
      IndefFullUnitId
        dcid
        ( Map.fromList
            [ (req, OpenModuleVar req)
            | req <- Set.toList (modShapeRequires shape)
            ]
        )

-- | Get the bin\/ directories that a package's executables should reside in.
--
-- The result may be empty if the package does not build any executables.
--
-- The result may have several entries if this is an inplace build of a package
-- with multiple executables.
binDirectories
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> [FilePath]
binDirectories layout config package = case elabBuildStyle package of
  -- quick sanity check: no sense returning a bin directory if we're not going
  -- to put any executables in it, that will just clog up the PATH
  _ | noExecutables -> []
  BuildAndInstall -> [installedBinDirectory package]
  BuildInplaceOnly{} -> map (root </>) $ case elabPkgOrComp package of
    ElabComponent comp -> case compSolverName comp of
      CD.ComponentExe n -> [prettyShow n]
      _ -> []
    ElabPackage _ ->
      map (prettyShow . PD.exeName)
        . PD.executables
        . elabPkgDescription
        $ package
  where
    noExecutables = null . PD.executables . elabPkgDescription $ package
    root =
      distBuildDirectory layout (elabDistDirParams config package)
        </> "build"

type InstS = Map (WithStage UnitId) ElaboratedPlanPackage
type InstM a = State InstS a

getComponentId
  :: ElaboratedPlanPackage
  -> ComponentId
getComponentId (InstallPlan.PreExisting (WithStage _stage dipkg)) = IPI.installedComponentId dipkg
getComponentId (InstallPlan.Configured elab) = elabComponentId elab
getComponentId (InstallPlan.Installed elab) = elabComponentId elab

extractElabBuildStyle
  :: InstallPlan.GenericPlanPackage ipkg ElaboratedConfiguredPackage
  -> BuildStyle
extractElabBuildStyle (InstallPlan.Configured elab) = elabBuildStyle elab
extractElabBuildStyle _ = BuildAndInstall

-- When using Backpack, packages can have "holes" that need to be filled with concrete implementations.

-- This function takes an initial install plan and creates additional plan entries for all the instantiated versions of packages

-- The function deals with:

-- Indefinite packages - Packages with holes/signatures that need to be filled
-- Instantiated packages - Concrete packages created by filling holes with specific implementations
-- Component IDs - Unique identifiers for components (libraries, executables etc.)
-- Unit IDs - Identifiers that track how holes are filled in instantiated packages

-- instantiateInstallPlan is responsible for filling out an InstallPlan
-- with all of the extra Configured packages that would be generated by
-- recursively instantiating the dependencies of packages.
--
-- Suppose we are compiling the following packages:
--
--  unit f where
--    signature H
--
--  unit g where
--    dependency f[H=containers:Data.Map]
--
-- At entry, we have an InstallPlan with a single plan package per
-- actual source package, e.g., only (indefinite!) f and g.  The job of
-- instantiation is to turn this into three plan packages: each of the
-- packages as before, but also a new, definite package f[H=containers:Data.Map]
--
-- How do we do this?  The general strategy is to iterate over every
-- package in the existing plan and recursively create new entries for
-- each of its dependencies which is an instantiated package (e.g.,
-- f[H=p:G]).  This process must be recursive, as f itself may depend on
-- OTHER packages which it instantiated using its hole H.
--
-- Some subtleties:
--
--  * We have to keep track of whether or not we are instantiating with
--    inplace packages, because instantiating a non-inplace package with
--    an inplace packages makes it inplace (since it depends on
--    something in the inplace store)!  The rule is that if any of the
--    modules in an instantiation are inplace, then the instantiated
--    unit itself must be inplace.  There is then a bunch of faffing
--    about to keep track of BuildStyle.
--
--  * ElaboratedConfiguredPackage was never really designed for post
--    facto instantiation, so some of the steps for generating new
--    instantiations are a little fraught.  For example, the act of
--    flipping a package to be inplace involves faffing about with four
--    fields, because these fields are precomputed.  A good refactor
--    would be to reduce the amount of precomputation to simplify the
--    algorithm here.
--
--  * We use the state monad to cache already instantiated modules, so
--    we don't instantiate the same thing multiple times.
--
instantiateInstallPlan
  :: HasCallStack
  => StoreDirLayout
  -> Staged InstallDirs.InstallDirTemplates
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> LogProgress ElaboratedInstallPlan
instantiateInstallPlan storeDirLayout defaultInstallDirs elaboratedShared plan = do
  InstallPlan.new (Map.elems ready_map)
  where
    pkgs = InstallPlan.toList plan

    cmap = Map.fromList [(WithStage (stageOf pkg) (getComponentId pkg), pkg) | pkg <- pkgs]

    instantiateUnitId
      :: Stage
      -> ComponentId
      -- \^ The id of the component being instantiated
      -> Map ModuleName (Module, BuildStyle)
      -- \^ A mapping from module names (the "holes" or signatures in Backpack)
      -- to the concrete modules (and their build styles) that should fill those
      -- holes.
      -> InstM (DefUnitId, BuildStyle)
    instantiateUnitId stage cid insts =
      gets (Map.lookup (WithStage stage uid)) >>= \case
        Nothing -> do
          r <- instantiateComponent uid (WithStage stage cid) insts
          modify (Map.insert (WithStage stage uid) r)
          return (unsafeMkDefUnitId uid, extractElabBuildStyle r)
        Just r ->
          return (unsafeMkDefUnitId uid, extractElabBuildStyle r)
      where
        uid = mkDefUnitId cid (fmap fst insts)

    -- No need to InplaceT; the inplace-ness is properly computed for
    -- the ElaboratedPlanPackage, so that will implicitly pass it on
    instantiateComponent
      :: UnitId
      -- \^ The unit id to assign to the instantiated component
      -> WithStage ComponentId
      -- \^ The id of the component being instantiated
      -> Map ModuleName (Module, BuildStyle)
      -- \^ A mapping from module names (the "holes" or signatures in Backpack)
      -- to the concrete modules (and their build styles) that should fill those
      -- holes.
      -> InstM ElaboratedPlanPackage
    instantiateComponent uid cidws@(WithStage stage cid) insts =
      case Map.lookup cidws cmap of
        Nothing -> error ("instantiateComponent: " ++ prettyShow cid)
        Just planpkg ->
          case planpkg of
            InstallPlan.Installed{} -> return planpkg
            InstallPlan.PreExisting{} -> return planpkg
            InstallPlan.Configured elab0 ->
              case elabPkgOrComp elab0 of
                ElabPackage{} -> return planpkg
                ElabComponent comp -> do
                  deps <- traverse (fmap fst . instantiateUnit stage insts) (compLinkedLibDependencies comp)
                  let build_style = fold (fmap snd insts)
                  let getDep (Module dep_uid _) = [dep_uid]
                      elab1 =
                        fixupBuildStyle build_style $
                          elab0
                            { elabUnitId = uid
                            , elabComponentId = cid
                            , elabIsCanonical = Map.null (fmap fst insts)
                            , elabPkgOrComp =
                                ElabComponent
                                  comp
                                    { compOrderLibDependencies =
                                        (if Map.null insts then [] else [newSimpleUnitId cid])
                                          ++ ordNub
                                            ( map
                                                unDefUnitId
                                                (deps ++ concatMap (getDep . fst) (Map.elems insts))
                                            )
                                    , compInstantiatedWith = fmap fst insts
                                    }
                            }
                  return $
                    InstallPlan.Configured
                      elab1
                        { elabInstallDirs =
                            computeInstallDirs
                              storeDirLayout
                              defaultInstallDirs
                              elaboratedShared
                              elab1
                        }

    -- \| Instantiates an OpenUnitId into a concrete UnitId, producing a concrete UnitId and its associated BuildStyle.
    --
    -- This function recursively applies a module substitution to an OpenUnitId, producing a fully instantiated
    -- (definite) unit and its build style. This is a key step in Backpack-style instantiation, where "holes" in
    -- a package are filled with concrete modules.
    --
    -- Behavior
    --
    -- If given a DefiniteUnitId, it returns the id and a default build style (BuildAndInstall).
    --
    -- If given an IndefFullUnitId, it:
    --    Recursively applies the substitution to each module in the instantiation map using substSubst.
    --    Calls instantiateUnitId to create or retrieve the fully instantiated unit id and build style for this instantiation.
    --
    instantiateUnit
      :: Stage
      -> Map ModuleName (Module, BuildStyle)
      -- \^ A mapping from module names to their corresponding modules and build styles.
      -> OpenUnitId
      -- \^ The unit to instantiate. This can be:
      --   DefiniteUnitId uid: already fully instantiated (no holes).
      --   IndefFullUnitId cid insts: an indefinite unit (with holes), described by a component id and a mapping of holes to modules.
      -> InstM (DefUnitId, BuildStyle)
    instantiateUnit _stage _subst (DefiniteUnitId def_uid) =
      -- This COULD actually, secretly, be an inplace package, but in
      -- that case it doesn't matter as it's already been recorded
      -- in the package that depends on this
      return (def_uid, BuildAndInstall)
    instantiateUnit stage subst (IndefFullUnitId cid insts) = do
      insts' <- traverse (instantiateModule stage subst) insts
      instantiateUnitId stage cid insts'

    -- \| Instantiates an OpenModule into a concrete Module producing a concrete Module
    -- and its associated BuildStyle.
    instantiateModule
      :: Stage
      -> Map ModuleName (Module, BuildStyle)
      -- \^ A mapping from module names to their corresponding modules and build styles.
      -> OpenModule
      -- \^ The module to substitute, which can be:
      --     OpenModuleVar mod_name: a hole (variable) named mod_name
      --     OpenModule uid mod_name: a module from a specific unit (uid).
      -> InstM (Module, BuildStyle)
    instantiateModule _stage subst (OpenModuleVar mod_name)
      | Just m <- Map.lookup mod_name subst = return m
      | otherwise = error "substModule: non-closing substitution"
    instantiateModule stage subst (OpenModule uid mod_name) = do
      (uid', build_style) <- instantiateUnit stage subst uid
      return (Module uid' mod_name, build_style)

    indefiniteComponent
      :: ElaboratedConfiguredPackage
      -> InstM ElaboratedConfiguredPackage
    indefiniteComponent epkg =
      case elabPkgOrComp epkg of
        ElabPackage{} -> return epkg
        ElabComponent elab_comp -> do
          -- We need to do a little more processing of the includes: some
          -- of them are fully definite even without substitution.  We
          -- want to build those too; see #5634.
          --
          -- This code mimics similar code in Distribution.Backpack.ReadyComponent;
          -- however, unlike the conversion from LinkedComponent to
          -- ReadyComponent, this transformation is done *without*
          -- changing the type in question; and what we are simply
          -- doing is enforcing tighter invariants on the data
          -- structure in question.  The new invariant is that there
          -- is no IndefFullUnitId in compLinkedLibDependencies that actually
          -- has no holes.  We couldn't specify this invariant when
          -- we initially created the ElaboratedPlanPackage because
          -- we have no way of actually reifying the UnitId into a
          -- DefiniteUnitId (that's what substUnitId does!)
          new_deps <- for (compLinkedLibDependencies elab_comp) $ \uid ->
            if Set.null (openUnitIdFreeHoles uid)
              then fmap (DefiniteUnitId . fst) (instantiateUnit (elabStage epkg) Map.empty uid)
              else return uid
          -- NB: no fixupBuildStyle needed here, as if the indefinite
          -- component depends on any inplace packages, it itself must
          -- be indefinite!  There is no substitution here, we can't
          -- post facto add inplace deps
          return
            epkg
              { elabPkgOrComp =
                  ElabComponent
                    elab_comp
                      { compLinkedLibDependencies = new_deps
                      , -- I think this is right: any new definite unit ids we
                        -- minted in the phase above need to be built before us.
                        -- Add 'em in.  This doesn't remove any old dependencies
                        -- on the indefinite package; they're harmless.
                        compOrderLibDependencies =
                          ordNub $
                            compOrderLibDependencies elab_comp
                              ++ [unDefUnitId d | DefiniteUnitId d <- new_deps]
                      }
              }

    fixupBuildStyle BuildAndInstall elab = elab
    fixupBuildStyle _buildStyle (elab@ElaboratedConfiguredPackage{elabBuildStyle = BuildInplaceOnly{}}) = elab
    fixupBuildStyle buildStyle@(BuildInplaceOnly{}) elab =
      elab
        { elabBuildStyle = buildStyle
        , elabBuildPackageDBStack = elabInplaceBuildPackageDBStack elab
        , elabRegisterPackageDBStack = elabInplaceRegisterPackageDBStack elab
        , elabSetupPackageDBStack = elabInplaceSetupPackageDBStack elab
        }

    ready_map = execState work Map.empty
    work = for_ pkgs $ \pkg ->
      case pkg of
        InstallPlan.Configured (elab@ElaboratedConfiguredPackage{elabPkgOrComp = ElabComponent comp})
          | not (Map.null (compLinkedInstantiatedWith comp)) -> do
              r <- indefiniteComponent elab
              modify (Map.insert (WithStage (elabStage elab) (elabUnitId elab)) (InstallPlan.Configured r))
        _ ->
          void $ instantiateUnitId (stageOf pkg) (getComponentId pkg) Map.empty

-- | Create a 'DefUnitId' from a 'ComponentId' and an instantiation
-- with no holes.
--
-- This function is defined in Cabal-syntax but only cabal-install
-- cares about it so I am putting it here.
--
-- I am also not using the DefUnitId newtype since I believe it
-- provides little value in the code above.
mkDefUnitId :: ComponentId -> Map ModuleName Module -> UnitId
mkDefUnitId cid insts =
  mkUnitId (unComponentId cid ++ maybe "" ("+" ++) (hashModuleSubst insts))

---------------------------
-- Build targets
--

-- Refer to ProjectPlanning.Types for details of these important types:

-- data ComponentTarget = ...
-- data SubComponentTarget = ...

-- One step in the build system is to translate higher level intentions like
-- "build this package", "test that package", or "repl that component" into
-- a more detailed specification of exactly which components to build (or other
-- actions like repl or build docs). This translation is somewhat different for
-- different commands. For example "test" for a package will build a different
-- set of components than "build". In addition, the translation of these
-- intentions can fail. For example "run" for a package is only unambiguous
-- when the package has a single executable.
--
-- So we need a little bit of infrastructure to make it easy for the command
-- implementations to select what component targets are meant when a user asks
-- to do something with a package or component. To do this (and to be able to
-- produce good error messages for mistakes and when targets are not available)
-- we need to gather and summarise accurate information about all the possible
-- targets, both available and unavailable. Then a command implementation can
-- decide which of the available component targets should be selected.

-- | An available target represents a component within a package that a user
-- command could plausibly refer to. In this sense, all the components defined
-- within the package are things the user could refer to, whether or not it
-- would actually be possible to build that component.
--
-- In particular the available target contains an 'AvailableTargetStatus' which
-- informs us about whether it's actually possible to select this component to
-- be built, and if not why not. This detail makes it possible for command
-- implementations (like @build@, @test@ etc) to accurately report why a target
-- cannot be used.
--
-- Note that the type parameter is used to help enforce that command
-- implementations can only select targets that can actually be built (by
-- forcing them to return the @k@ value for the selected targets).
-- In particular 'resolveTargets' makes use of this (with @k@ as
-- @('UnitId', ComponentName')@) to identify the targets thus selected.
data AvailableTarget k = AvailableTarget
  { availableTargetPackageId :: PackageId
  , availableTargetComponentName :: ComponentName
  , availableTargetStatus :: AvailableTargetStatus k
  , availableTargetLocalToProject :: Bool
  }
  deriving (Eq, Show, Functor)

-- | The status of a an 'AvailableTarget' component. This tells us whether
-- it's actually possible to select this component to be built, and if not
-- why not.
data AvailableTargetStatus k
  = -- | When the user does @tests: False@
    TargetDisabledByUser
  | -- | When the solver could not enable tests
    TargetDisabledBySolver
  | -- | When the component has @buildable: False@
    TargetNotBuildable
  | -- | When the component is non-core in a non-local package
    TargetNotLocal
  | -- | The target can or should be built
    TargetBuildable k TargetRequested
  deriving (Eq, Ord, Show, Functor)

-- | This tells us whether a target ought to be built by default, or only if
-- specifically requested. The policy is that components like libraries and
-- executables are built by default by @build@, but test suites and benchmarks
-- are not, unless this is overridden in the project configuration.
data TargetRequested
  = -- | To be built by default
    TargetRequestedByDefault
  | -- | Not to be built by default
    TargetNotRequestedByDefault
  deriving (Eq, Ord, Show)

-- | Given the install plan, produce the set of 'AvailableTarget's for each
-- package-component pair.
--
-- Typically there will only be one such target for each component, but for
-- example if we have a plan with both normal and profiling variants of a
-- component then we would get both as available targets, or similarly if we
-- had a plan that contained two instances of the same version of a package.
-- This approach makes it relatively easy to select all instances\/variants
-- of a component.
availableTargets
  :: ElaboratedInstallPlan
  -> Map
      (PackageId, ComponentName)
      [AvailableTarget (WithStage UnitId, ComponentName)]
availableTargets installPlan =
  let rs =
        [ (pkgid, cname, fake, target)
        | pkg <- InstallPlan.toList installPlan
        , (stage, pkgid, cname, fake, target) <- case pkg of
            InstallPlan.PreExisting ipkg -> availableInstalledTargets ipkg
            InstallPlan.Installed elab -> availableSourceTargets elab
            InstallPlan.Configured elab -> availableSourceTargets elab
        , -- Only host stage can be explicitly requested by the user
        stage == Host
        ]
   in Map.union
        ( Map.fromListWith
            (++)
            [ ((pkgid, cname), [target])
            | (pkgid, cname, fake, target) <- rs
            , not fake
            ]
        )
        ( Map.fromList
            [ ((pkgid, cname), [target])
            | (pkgid, cname, fake, target) <- rs
            , fake
            ]
        )

-- The normal targets mask the fake ones. We get all instances of the
-- normal ones and only one copy of the fake ones (as there are many
-- duplicates of the fake ones). See 'availableSourceTargets' below for
-- more details on this fake stuff is about.

availableInstalledTargets
  :: WithStage IPI.InstalledPackageInfo
  -> [ ( Stage
       , PackageId
       , ComponentName
       , Bool
       , AvailableTarget (WithStage UnitId, ComponentName)
       )
     ]
availableInstalledTargets (WithStage stage ipkg) =
  let unitid = installedUnitId ipkg
      cname = CLibName LMainLibName
      status = TargetBuildable (WithStage stage unitid, cname) TargetRequestedByDefault
      target = AvailableTarget (packageId ipkg) cname status False
      fake = False
   in [(stage, IPI.sourcePackageId ipkg, cname, fake, target)]

availableSourceTargets
  :: ElaboratedConfiguredPackage
  -> [ ( Stage
       , PackageId
       , ComponentName
       , Bool
       , AvailableTarget (WithStage UnitId, ComponentName)
       )
     ]
availableSourceTargets elab =
  -- We have a somewhat awkward problem here. We need to know /all/ the
  -- components from /all/ the packages because these are the things that
  -- users could refer to. Unfortunately, at this stage the elaborated install
  -- plan does /not/ contain all components: some components have already
  -- been deleted because they cannot possibly be built. This is the case
  -- for components that are marked @buildable: False@ in their .cabal files.
  -- (It's not unreasonable that the unbuildable components have been pruned
  -- as the plan invariant is considerably simpler if all nodes can be built)
  --
  -- We can recover the missing components but it's not exactly elegant. For
  -- a graph node corresponding to a component we still have the information
  -- about the package that it came from, and this includes the names of
  -- /all/ the other components in the package. So in principle this lets us
  -- find the names of all components, plus full details of the buildable
  -- components.
  --
  -- Consider for example a package with 3 exe components: foo, bar and baz
  -- where foo and bar are buildable, but baz is not. So the plan contains
  -- nodes for the components foo and bar. Now we look at each of these two
  -- nodes and look at the package they come from and the names of the
  -- components in this package. This will give us the names foo, bar and
  -- baz, twice (once for each of the two buildable components foo and bar).
  --
  -- We refer to these reconstructed missing components as fake targets.
  -- It is an invariant that they are not available to be built.
  --
  -- To produce the final set of targets we put the fake targets in a finite
  -- map (thus eliminating the duplicates) and then we overlay that map with
  -- the normal buildable targets. (This is done above in 'availableTargets'.)
  --
  [ (elabStage elab, elabPkgSourceId elab, cname, fake, target)
  | component <- pkgComponents (elabPkgDescription elab)
  , let cname = componentName component
        status = componentAvailableTargetStatus component
        target =
          AvailableTarget
            { availableTargetPackageId = packageId elab
            , availableTargetComponentName = cname
            , availableTargetStatus = status
            , availableTargetLocalToProject = elabLocalToProject elab
            }
        fake = isFakeTarget cname
  , -- TODO: The goal of this test is to exclude "instantiated"
  -- packages as available targets. This means that you can't
  -- ask for a particular instantiated component to be built;
  -- it will only get built by a dependency.  Perhaps the
  -- correct way to implement this is to run selection
  -- prior to instantiating packages.  If you refactor
  -- this, then you can delete this test.
  elabIsCanonical elab
  , -- Filter out some bogus parts of the cross product that are never needed
  case status of
    TargetBuildable{} | fake -> False
    _ -> True
  ]
  where
    isFakeTarget cname =
      case elabPkgOrComp elab of
        ElabPackage _ -> False
        ElabComponent elabComponent ->
          compComponentName elabComponent
            /= Just cname

    componentAvailableTargetStatus
      :: Component -> AvailableTargetStatus (WithStage UnitId, ComponentName)
    componentAvailableTargetStatus component =
      case componentOptionalStanza $ CD.componentNameToComponent cname of
        -- it is not an optional stanza, so a library, exe or foreign lib
        Nothing
          | not buildable -> TargetNotBuildable
          | otherwise ->
              TargetBuildable
                (WithStage (elabStage elab) (elabUnitId elab), cname)
                TargetRequestedByDefault
        -- it is not an optional stanza, so a testsuite or benchmark
        Just stanza ->
          case ( optStanzaLookup stanza (elabStanzasRequested elab) -- TODO
               , optStanzaSetMember stanza (elabStanzasAvailable elab)
               ) of
            _ | not withinPlan -> TargetNotLocal
            (Just False, _) -> TargetDisabledByUser
            (Nothing, False) -> TargetDisabledBySolver
            _ | not buildable -> TargetNotBuildable
            (Just True, True) ->
              TargetBuildable
                (WithStage (elabStage elab) (elabUnitId elab), cname)
                TargetRequestedByDefault
            (Nothing, True) ->
              TargetBuildable
                (WithStage (elabStage elab) (elabUnitId elab), cname)
                TargetNotRequestedByDefault
            (Just True, False) ->
              error $ "componentAvailableTargetStatus: impossible; cname=" ++ prettyShow cname
      where
        cname = componentName component
        buildable = PD.buildable (componentBuildInfo component)
        withinPlan =
          elabLocalToProject elab
            || case elabPkgOrComp elab of
              ElabComponent elabComponent ->
                compComponentName elabComponent == Just cname
              ElabPackage _ ->
                case componentName component of
                  CLibName (LMainLibName) -> True
                  CExeName _ -> True
                  -- TODO: what about sub-libs and foreign libs?
                  _ -> False

-- | Merge component targets that overlap each other. Specially when we have
-- multiple targets for the same component and one of them refers to the whole
-- component (rather than a module or file within) then all the other targets
-- for that component are subsumed.
--
-- We also allow for information associated with each component target, and
-- whenever we targets subsume each other we aggregate their associated info.
nubComponentTargets :: [(ComponentTarget, a)] -> [(ComponentTarget, NonEmpty a)]
nubComponentTargets =
  concatMap (wholeComponentOverrides . map snd)
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . map (\t@((ComponentTarget cname _, _)) -> (cname, t))
    . map compatSubComponentTargets
  where
    -- If we're building the whole component then that the only target all we
    -- need, otherwise we can have several targets within the component.
    wholeComponentOverrides
      :: [(ComponentTarget, a)]
      -> [(ComponentTarget, NonEmpty a)]
    wholeComponentOverrides ts =
      case [ta | ta@(ComponentTarget _ WholeComponent, _) <- ts] of
        ((t, x) : _) ->
          let
            -- Delete tuple (t, x) from original list to avoid duplicates.
            -- Use 'deleteBy', to avoid additional Class constraint on 'nubComponentTargets'.
            ts' = deleteBy (\(t1, _) (t2, _) -> t1 == t2) (t, x) ts
           in
            [(t, x :| map snd ts')]
        [] -> [(t, x :| []) | (t, x) <- ts]

    -- Not all Cabal Setup.hs versions support sub-component targets, so switch
    -- them over to the whole component
    compatSubComponentTargets :: (ComponentTarget, a) -> (ComponentTarget, a)
    compatSubComponentTargets target@(ComponentTarget cname _subtarget, x)
      | not setupHsSupportsSubComponentTargets =
          (ComponentTarget cname WholeComponent, x)
      | otherwise = target

    -- Actually the reality is that no current version of Cabal's Setup.hs
    -- build command actually support building specific files or modules.
    setupHsSupportsSubComponentTargets = False

-- TODO: when that changes, adjust this test, e.g.
-- \| pkgSetupScriptCliVersion >= Version [x,y] []

pkgHasEphemeralBuildTargets :: ElaboratedConfiguredPackage -> Bool
pkgHasEphemeralBuildTargets elab =
  (not . null) (elabReplTarget elab)
    || (not . null) (elabTestTargets elab)
    || (not . null) (elabBenchTargets elab)
    || (not . null) (elabHaddockTargets elab)
    || (not . null)
      [ () | ComponentTarget _ subtarget <- elabBuildTargets elab, subtarget /= WholeComponent
      ]

-- | The components that we'll build all of, meaning that after they're built
-- we can skip building them again (unlike with building just some modules or
-- other files within a component).
elabBuildTargetWholeComponents
  :: ElaboratedConfiguredPackage
  -> Set ComponentName
elabBuildTargetWholeComponents elab =
  Set.fromList
    [cname | ComponentTarget cname WholeComponent <- elabBuildTargets elab]

------------------------------------------------------------------------------

-- * Install plan pruning

------------------------------------------------------------------------------

-- | How 'pruneInstallPlanToTargets' should interpret the per-package
-- 'ComponentTarget's: as build, repl or haddock targets.
data TargetAction
  = TargetActionConfigure
  | TargetActionBuild
  | TargetActionRepl
  | TargetActionTest
  | TargetActionBench
  | TargetActionHaddock

-- | Given a set of per-package\/per-component targets, take the subset of the
-- install plan needed to build those targets. Also, update the package config
-- to specify which optional stanzas to enable, and which targets within each
-- package to build.
--
-- NB: Pruning happens after improvement, which is important because we
-- will prune differently depending on what is already installed (to
-- implement "sticky" test suite enabling behavior).
pruneInstallPlanToTargets
  :: HasCallStack
  => TargetAction
  -> Map (Graph.Key ElaboratedPlanPackage) [ComponentTarget]
  -> ElaboratedInstallPlan
  -> LogProgress ElaboratedInstallPlan
pruneInstallPlanToTargets targetActionType perPkgTargetsMap elaboratedPlan =
  InstallPlan.new
    -- We have to do the pruning in two passes
    . pruneInstallPlanPass2
    . pruneInstallPlanPass1
    -- Set the targets that will be the roots for pruning
    . setRootTargets targetActionType perPkgTargetsMap
    . InstallPlan.toList
    $ elaboratedPlan

-- | This is a temporary data type, where we temporarily
-- override the graph dependencies of an 'ElaboratedPackage',
-- so we can take a closure over them.  We'll throw out the
-- overridden dependencies when we're done so it's strictly temporary.
--
-- For 'ElaboratedComponent', this the cached unit IDs always
-- coincide with the real thing.
data PrunedPackage = PrunedPackage ElaboratedConfiguredPackage [WithStage UnitId]

instance Package PrunedPackage where
  packageId (PrunedPackage elab _) = packageId elab

instance HasUnitId PrunedPackage where
  installedUnitId (PrunedPackage elab _) = installedUnitId elab

instance Graph.IsNode PrunedPackage where
  type Key PrunedPackage = WithStage UnitId
  nodeKey (PrunedPackage elab _) = Graph.nodeKey elab
  nodeNeighbors (PrunedPackage _ deps) = deps

fromPrunedPackage :: PrunedPackage -> ElaboratedConfiguredPackage
fromPrunedPackage (PrunedPackage elab _) = elab

-- | Set the build targets based on the user targets (but not rev deps yet).
-- This is required before we can prune anything.
setRootTargets
  :: TargetAction
  -> Map (Graph.Key ElaboratedPlanPackage) [ComponentTarget]
  -> [ElaboratedPlanPackage]
  -> [ElaboratedPlanPackage]
setRootTargets targetAction perPkgTargetsMap =
  assert (not (Map.null perPkgTargetsMap)) $
    assert (all (not . null) (Map.elems perPkgTargetsMap)) $
      map (mapConfiguredPackage setElabBuildTargets)
  where
    -- Set the targets we'll build for this package/component. This is just
    -- based on the root targets from the user, not targets implied by reverse
    -- dependencies. Those comes in the second pass once we know the rev deps.
    --
    setElabBuildTargets elab =
      case ( Map.lookup (Graph.nodeKey elab) perPkgTargetsMap
           , targetAction
           ) of
        (Nothing, _) -> elab
        (Just tgts, TargetActionConfigure) -> elab{elabConfigureTargets = tgts}
        (Just tgts, TargetActionBuild) -> elab{elabBuildTargets = tgts}
        (Just tgts, TargetActionTest) -> elab{elabTestTargets = tgts}
        (Just tgts, TargetActionBench) -> elab{elabBenchTargets = tgts}
        (Just tgts, TargetActionRepl) ->
          elab
            { elabReplTarget = tgts
            , elabBuildHaddocks = False
            , elabBuildStyle = BuildInplaceOnly InMemory
            }
        (Just tgts, TargetActionHaddock) ->
          foldr
            setElabHaddockTargets
            ( elab
                { elabHaddockTargets = tgts
                , elabBuildHaddocks = True
                }
            )
            tgts

    setElabHaddockTargets tgt elab
      | isTestComponentTarget tgt = elab{elabHaddockTestSuites = True}
      | isBenchComponentTarget tgt = elab{elabHaddockBenchmarks = True}
      | isForeignLibComponentTarget tgt = elab{elabHaddockForeignLibs = True}
      | isExeComponentTarget tgt = elab{elabHaddockExecutables = True}
      | isSubLibComponentTarget tgt = elab{elabHaddockInternal = True}
      | otherwise = elab

-- | Assuming we have previously set the root build targets (i.e. the user
-- targets but not rev deps yet), the first pruning pass does two things:
--
-- * A first go at determining which optional stanzas (testsuites, benchmarks)
--   are needed. We have a second go in the next pass.
-- * Take the dependency closure using pruned dependencies. We prune deps that
--   are used only by unneeded optional stanzas. These pruned deps are only
--   used for the dependency closure and are not persisted in this pass.
pruneInstallPlanPass1
  :: HasCallStack
  => [ElaboratedPlanPackage]
  -> [ElaboratedPlanPackage]
pruneInstallPlanPass1 pkgs
  -- if there are repl targets, we need to do a bit more work
  -- See Note [Pruning for Multi Repl]
  | anyMultiReplTarget = graph_with_repl_targets
  -- otherwise we'll do less
  | otherwise = pruned_packages
  where
    pkgs' :: [InstallPlan.GenericPlanPackage (WithStage IPI.InstalledPackageInfo) PrunedPackage]
    pkgs' = map (mapConfiguredPackage prune) pkgs

    prune :: ElaboratedConfiguredPackage -> PrunedPackage
    prune elab = PrunedPackage elab' (pruneOptionalDependencies elab')
      where
        elab' = addOptionalStanzas elab

    graph = Graph.fromDistinctList pkgs'

    roots :: [Graph.Key ElaboratedPlanPackage]
    roots = map Graph.nodeKey (filter is_root pkgs')

    -- Make a closed graph by calculating the closure from the roots
    pruned_packages :: [ElaboratedPlanPackage]
    pruned_packages = map (mapConfiguredPackage fromPrunedPackage) (fromMaybe [] $ Graph.closure graph roots)

    closed_graph :: Graph.Graph ElaboratedPlanPackage
    closed_graph = Graph.fromDistinctList pruned_packages

    -- whether any package has repl targets enabled, and we need to use multi-repl.
    anyMultiReplTarget :: Bool
    anyMultiReplTarget = length repls > 1
      where
        repls = filter is_repl_gpp pkgs'
        is_repl_gpp (InstallPlan.Configured pkg) = is_repl_pp pkg
        is_repl_gpp _ = False

        is_repl_pp (PrunedPackage elab _) = not (null (elabReplTarget elab))

    -- Anything which is inplace and left after pruning could be a repl target, then just need to check the
    -- reverse closure after calculating roots to capture dependencies which are on the path between roots.
    -- In order to start a multi-repl session with all the desired targets we need to load all these components into
    -- the repl at once to satisfy the closure property.
    all_desired_repl_targets = Set.fromList [elabUnitId cp | InstallPlan.Configured cp <- fromMaybe [] $ Graph.revClosure closed_graph roots]

    add_repl_target :: ElaboratedConfiguredPackage -> ElaboratedConfiguredPackage
    add_repl_target ecp
      | elabUnitId ecp `Set.member` all_desired_repl_targets =
          ecp
            { elabReplTarget = maybeToList (ComponentTarget <$> (elabComponentName ecp) <*> pure WholeComponent)
            , elabBuildStyle = BuildInplaceOnly InMemory
            }
      | otherwise = ecp

    -- Add the repl target information to the ElaboratedPlanPackages
    graph_with_repl_targets
      | anyMultiReplTarget = map (mapConfiguredPackage add_repl_target) (Graph.toList closed_graph)
      | otherwise = Graph.toList closed_graph

    is_root :: InstallPlan.GenericPlanPackage (WithStage IPI.InstalledPackageInfo) PrunedPackage -> Bool
    is_root =
      foldPlanPackage
        (const False)
        ( \(PrunedPackage elab _) ->
            not $
              and
                [ null (elabConfigureTargets elab)
                , null (elabBuildTargets elab)
                , null (elabTestTargets elab)
                , null (elabBenchTargets elab)
                , null (elabReplTarget elab)
                , null (elabHaddockTargets elab)
                ]
        )

    -- Note [Sticky enabled testsuites]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- The testsuite and benchmark targets are somewhat special in that we need
    -- to configure the packages with them enabled, and we need to do that even
    -- if we only want to build one of several testsuites.
    --
    -- There are two cases in which we will enable the testsuites (or
    -- benchmarks): if one of the targets is a testsuite, or if all of the
    -- testsuite dependencies are already cached in the store. The rationale
    -- for the latter is to minimise how often we have to reconfigure due to
    -- the particular targets we choose to build. Otherwise choosing to build
    -- a testsuite target, and then later choosing to build an exe target
    -- would involve unnecessarily reconfiguring the package with testsuites
    -- disabled. Technically this introduces a little bit of stateful
    -- behaviour to make this "sticky", but it should be benign.

    -- Decide whether or not to enable testsuites and benchmarks.
    -- See [Sticky enabled testsuites]
    addOptionalStanzas :: ElaboratedConfiguredPackage -> ElaboratedConfiguredPackage
    addOptionalStanzas elab@ElaboratedConfiguredPackage{elabPkgOrComp = ElabPackage pkg} =
      elab
        { elabPkgOrComp = ElabPackage (pkg{pkgStanzasEnabled = stanzas})
        }
      where
        stanzas :: OptionalStanzaSet
        -- By default, we enabled all stanzas requested by the user,
        -- as per elabStanzasRequested, done in
        -- 'elaborateSolverToPackage'
        stanzas =
          pkgStanzasEnabled pkg
            -- optionalStanzasRequiredByTargets has to be done at
            -- prune-time because it depends on 'elabTestTargets'
            -- et al, which is done by 'setRootTargets' at the
            -- beginning of pruning.
            <> optionalStanzasRequiredByTargets elab
            -- optionalStanzasWithDepsAvailable has to be done at
            -- prune-time because it depends on what packages are
            -- installed, which is not known until after improvement
            -- (pruning is done after improvement)
            <> optionalStanzasWithDepsAvailable availablePkgs elab pkg
    addOptionalStanzas elab = elab

    -- Calculate package dependencies but cut out those needed only by
    -- optional stanzas that we've determined we will not enable.
    -- These pruned deps are not persisted in this pass since they're based on
    -- the optional stanzas and we'll make further tweaks to the optional
    -- stanzas in the next pass.
    --
    pruneOptionalDependencies :: ElaboratedConfiguredPackage -> [Graph.Key ElaboratedConfiguredPackage]
    pruneOptionalDependencies elab@ElaboratedConfiguredPackage{elabPkgOrComp = ElabComponent _} =
      InstallPlan.depends elab -- no pruning
    pruneOptionalDependencies ElaboratedConfiguredPackage{elabPkgOrComp = ElabPackage pkg} =
      (CD.flatDeps . CD.filterDeps keepNeeded) (pkgOrderDependencies pkg)
      where
        keepNeeded (CD.ComponentTest _) _ = TestStanzas `optStanzaSetMember` stanzas
        keepNeeded (CD.ComponentBench _) _ = BenchStanzas `optStanzaSetMember` stanzas
        keepNeeded _ _ = True
        stanzas = pkgStanzasEnabled pkg

    optionalStanzasRequiredByTargets
      :: ElaboratedConfiguredPackage
      -> OptionalStanzaSet
    optionalStanzasRequiredByTargets pkg =
      optStanzaSetFromList
        [ stanza
        | ComponentTarget cname _ <-
            elabBuildTargets pkg
              ++ elabTestTargets pkg
              ++ elabBenchTargets pkg
              ++ elabReplTarget pkg
              ++ elabHaddockTargets pkg
        , stanza <-
            maybeToList $
              componentOptionalStanza $
                CD.componentNameToComponent cname
        ]

    availablePkgs =
      Set.fromList
        [ Graph.nodeKey pkg
        | InstallPlan.PreExisting pkg <- pkgs
        ]

{-
Note [Pruning for Multi Repl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a multi-repl session, where we load more than one component into a GHCi repl,
it is required to uphold the so-called *closure property*.
This property, whose exact Note you can read in the GHC codebase, states
roughly:

\* If a component you want to load into a repl session transitively depends on a
  component which transitively depends on another component you want to
  load into the repl, then this component needs to be loaded
  into the repl session as well.

We make sure here, that this property is upheld, by calculating the
graph of components that we need to load into the repl given the set of 'roots' which
are the targets specified by the user.

Practically, this is simply achieved by traversing all dependencies of
our roots (graph closure), and then from this closed graph, we calculate
the reverse closure, which gives us all components that depend on
'roots'. Thus, the result is a list of components that we need to load
into the repl to uphold the closure property.
-}

-- | Given a set of already installed packages @availablePkgs@,
-- determine the set of available optional stanzas from @pkg@
-- which have all of their dependencies already installed.  This is used
-- to implement "sticky" testsuites, where once we have installed
-- all of the deps needed for the test suite, we go ahead and
-- enable it always.
optionalStanzasWithDepsAvailable
  :: Set (Graph.Key ElaboratedPlanPackage)
  -> ElaboratedConfiguredPackage
  -> ElaboratedPackage
  -> OptionalStanzaSet
optionalStanzasWithDepsAvailable availablePkgs elab pkg =
  optStanzaSetFromList
    [ stanza
    | stanza <- optStanzaSetToList (elabStanzasAvailable elab)
    , let deps =
            CD.select
              (optionalStanzaDeps stanza)
              -- TODO: probably need to select other
              -- dep types too eventually
              (pkgOrderDependencies pkg)
    , all (`Set.member` availablePkgs) deps
    ]
  where
    optionalStanzaDeps TestStanzas (CD.ComponentTest _) = True
    optionalStanzaDeps BenchStanzas (CD.ComponentBench _) = True
    optionalStanzaDeps _ _ = False

-- The second pass does three things:
--

-- * A second go at deciding which optional stanzas to enable.

-- * Prune the dependencies based on the final choice of optional stanzas.

-- * Extend the targets within each package to build, now we know the reverse

--   dependencies, ie we know which libs are needed as deps by other packages.
--
-- Achieving sticky behaviour with enabling\/disabling optional stanzas is
-- tricky. The first approximation was handled by the first pass above, but
-- it's not quite enough. That pass will enable stanzas if all of the deps
-- of the optional stanza are already installed /in the store/. That's important
-- but it does not account for dependencies that get built inplace as part of
-- the project. We cannot take those inplace build deps into account in the
-- pruning pass however because we don't yet know which ones we're going to
-- build. Once we do know, we can have another go and enable stanzas that have
-- all their deps available. Now we can consider all packages in the pruned
-- plan to be available, including ones we already decided to build from
-- source.
--
-- Deciding which targets to build depends on knowing which packages have
-- reverse dependencies (ie are needed). This requires the result of first
-- pass, which is another reason we have to split it into two passes.
--
-- Note that just because we might enable testsuites or benchmarks (in the
-- first or second pass) doesn't mean that we build all (or even any) of them.
-- That depends on which targets we picked in the first pass.
--
pruneInstallPlanPass2
  :: [ElaboratedPlanPackage]
  -> [ElaboratedPlanPackage]
pruneInstallPlanPass2 pkgs =
  map (mapConfiguredPackage setStanzasDepsAndTargets) pkgs
  where
    setStanzasDepsAndTargets elab =
      elab
        { elabBuildTargets =
            ordNub $
              elabBuildTargets elab
                ++ libTargetsRequiredForRevDeps
                ++ exeTargetsRequiredForRevDeps
        , elabPkgOrComp =
            case elabPkgOrComp elab of
              ElabPackage pkg ->
                let stanzas =
                      pkgStanzasEnabled pkg
                        <> optionalStanzasWithDepsAvailable availablePkgs elab pkg

                    keepNeeded :: CD.Component -> a -> Bool
                    keepNeeded (CD.ComponentTest _) _ = TestStanzas `optStanzaSetMember` stanzas
                    keepNeeded (CD.ComponentBench _) _ = BenchStanzas `optStanzaSetMember` stanzas
                    keepNeeded _ _ = True
                 in ElabPackage $
                      pkg
                        { pkgStanzasEnabled =
                            stanzas
                        , pkgLibDependencies =
                            CD.mapDeps (\_ -> map addInternal) $
                              CD.filterDeps keepNeeded (pkgLibDependencies pkg)
                        , pkgExeDependencies =
                            CD.filterDeps keepNeeded (pkgExeDependencies pkg)
                        , pkgExeDependencyPaths =
                            CD.filterDeps keepNeeded (pkgExeDependencyPaths pkg)
                        }
              ElabComponent comp ->
                ElabComponent $
                  comp
                    { compLibDependencies = map addInternal (compLibDependencies comp)
                    }
        }
      where
        -- We initially assume that all the dependencies are external (hence the boolean is always
        -- False) and here we correct the dependencies so the right packages are marked promised.
        addInternal (cid, _) = (cid, (cid `Set.member` inMemoryTargets))

        libTargetsRequiredForRevDeps =
          [ c
          | Graph.nodeKey elab `Set.member` hasReverseLibDeps
          , let c = ComponentTarget (CLibName Cabal.defaultLibName) WholeComponent
          , -- Don't enable building for anything which is being build in memory
          elabBuildStyle elab /= BuildInplaceOnly InMemory
          ]
        exeTargetsRequiredForRevDeps =
          -- TODO: allow requesting executable with different name
          -- than package name
          [ ComponentTarget
            ( Cabal.CExeName $
                packageNameToUnqualComponentName $
                  packageName $
                    elabPkgSourceId elab
            )
            WholeComponent
          | Graph.nodeKey elab `Set.member` hasReverseExeDeps
          ]

    availablePkgs = Set.fromList (map Graph.nodeKey pkgs)

    inMemoryTargets :: Set ConfiguredId
    inMemoryTargets = do
      Set.fromList
        [ configuredId pkg
        | InstallPlan.Configured pkg <- pkgs
        , BuildInplaceOnly InMemory <- [elabBuildStyle pkg]
        ]

    hasReverseLibDeps =
      Set.fromList
        [ depid
        | InstallPlan.Configured pkg <- pkgs
        , depid <- elabOrderLibDependencies pkg
        ]

    hasReverseExeDeps =
      Set.fromList
        [ depid
        | InstallPlan.Configured pkg <- pkgs
        , depid <- elabOrderExeDependencies pkg
        ]

mapConfiguredPackage
  :: (srcpkg -> srcpkg')
  -> InstallPlan.GenericPlanPackage ipkg srcpkg
  -> InstallPlan.GenericPlanPackage ipkg srcpkg'
mapConfiguredPackage f (InstallPlan.Configured pkg) =
  InstallPlan.Configured (f pkg)
mapConfiguredPackage f (InstallPlan.Installed pkg) =
  InstallPlan.Installed (f pkg)
mapConfiguredPackage _ (InstallPlan.PreExisting pkg) =
  InstallPlan.PreExisting pkg

------------------------------------
-- Support for --only-dependencies
--

-- | Try to remove the given targets from the install plan.
--
-- This is not always possible.
pruneInstallPlanToDependencies
  :: HasCallStack
  => Set (Graph.Key ElaboratedPlanPackage)
  -> ElaboratedInstallPlan
  -> Either
      CannotPruneDependencies
      (Graph.Graph ElaboratedPlanPackage)
pruneInstallPlanToDependencies pkgTargets installPlan =
  assert
    ( all
        (isJust . InstallPlan.lookup installPlan)
        (Set.toList pkgTargets)
    )
    $ checkBrokenDeps
      . Graph.fromDistinctList
      . filter (\pkg -> Graph.nodeKey pkg `Set.notMember` pkgTargets)
      . InstallPlan.toList
    $ installPlan
  where
    -- Our strategy is to remove the packages we don't want and then check
    -- if the remaining graph is broken or not, ie any packages with dangling
    -- dependencies. If there are then we cannot prune the given targets.
    checkBrokenDeps
      :: Graph.Graph ElaboratedPlanPackage
      -> Either
          CannotPruneDependencies
          (Graph.Graph ElaboratedPlanPackage)
    checkBrokenDeps graph =
      case Graph.broken graph of
        [] -> Right graph
        brokenPackages ->
          Left $
            CannotPruneDependencies
              [ (pkg, missingDeps)
              | (pkg, missingDepIds) <- brokenPackages
              , let missingDeps = NE.map (fromMaybe (error "should not happen") . lookupDep) missingDepIds
              ]
          where
            -- lookup in the original unpruned graph
            lookupDep = InstallPlan.lookup installPlan

-- | It is not always possible to prune to only the dependencies of a set of
-- targets. It may be the case that removing a package leaves something else
-- that still needed the pruned package.
--
-- This lists all the packages that would be broken, and their dependencies
-- that would be missing if we did prune.
newtype CannotPruneDependencies
  = CannotPruneDependencies
      [ ( ElaboratedPlanPackage
        , NonEmpty ElaboratedPlanPackage
        )
      ]
  deriving (Show)

-- The other aspects of our Setup.hs policy lives here where we decide on
-- the 'SetupScriptOptions'.
--
-- Our current policy for the 'SetupCustomImplicitDeps' case is that we
-- try to make the implicit deps cover everything, and we don't allow the
-- compiler to pick up other deps. This may or may not be sustainable, and
-- we might have to allow the deps to be non-exclusive, but that itself would
-- be tricky since we would have to allow the Setup access to all the packages
-- in the store and local dbs.

setupHsScriptOptions
  :: ElaboratedReadyPackage
  -> ElaboratedInstallPlan
  -> ElaboratedSharedConfig
  -> DistDirLayout
  -> SymbolicPath CWD (Dir Pkg)
  -> SymbolicPath Pkg (Dir Dist)
  -> Bool
  -> Lock
  -> SetupScriptOptions
-- TODO: Fix this so custom is a separate component.  Custom can ALWAYS
-- be a separate component!!!
setupHsScriptOptions
  (ReadyPackage elab@ElaboratedConfiguredPackage{..})
  plan
  ElaboratedSharedConfig{..}
  distdir
  srcdir
  builddir
  isParallelBuild
  cacheLock =
    SetupScriptOptions
      { useCabalVersion = thisVersion elabSetupScriptCliVersion
      , useCabalSpecVersion =
          if PD.buildType elabPkgDescription == PD.Hooks
            then -- NB: we don't want to commit to a Cabal version here:
            --   - all that should matter for Hooks build-type is the
            --     version of Cabal-hooks, not of Cabal,
            --   - if we commit to a Cabal version, the logic in
              Nothing
            else Just elabSetupScriptCliVersion
      , useCompiler = Just toolchainCompiler
      , usePlatform = Just toolchainPlatform
      , useProgramDb = toolchainProgramDb
      , usePackageDB = elabSetupPackageDBStack
      , usePackageIndex = Nothing
      , useDependencies =
          [ (confInstId cid, confSrcId cid)
          | -- TODO: we should filter for dependencies on libraries but that should be implicit in elabSetupLibDependencies
          (WithStage _ cid) <- elabSetupLibDependencies elab
          ]
      , useDependenciesExclusive = True
      , useVersionMacros = elabSetupScriptStyle == SetupCustomExplicitDeps
      , useDistPref = builddir
      , useLoggingHandle = Nothing -- this gets set later
      , useWorkingDir = Just srcdir
      , useExtraPathEnv = elabExeDependencyPaths elab ++ elabProgramPathExtra
      , -- note that the above adds the extra-prog-path directly following the elaborated
        -- dep paths, so that it overrides the normal path, but _not_ the elaborated extensions
        -- for build-tools-depends.
        useExtraEnvOverrides = dataDirsEnvironmentForPlan distdir plan
      , useWin32CleanHack = False -- TODO: [required eventually]
      , forceExternalSetupMethod = isParallelBuild
      , setupCacheLock = Just cacheLock
      , isInteractive = False
      , isMainLibOrExeComponent = case elabPkgOrComp of
          -- if it's a package, it's all together, so we have to assume it's
          -- at least a library or executable.
          ElabPackage _ -> True
          -- if it's a component, we have to check if it's a Main Library or Executable
          -- as opposed to SubLib, FLib, Test, Bench, or Setup component.
          ElabComponent (ElaboratedComponent{compSolverName = CD.ComponentLib}) -> True
          ElabComponent (ElaboratedComponent{compSolverName = CD.ComponentExe _}) -> True
          -- everything else is not a main lib or exe component
          ElabComponent _ -> False
      }
    where
      Toolchain{toolchainCompiler, toolchainPlatform, toolchainProgramDb} =
        -- TODO: It is disappointing that we have to change the stage here
        getStage pkgConfigToolchains (prevStage elabStage)

-- | To be used for the input for elaborateInstallPlan.
--
-- TODO: [code cleanup] make InstallDirs.defaultInstallDirs pure.
userInstallDirTemplates
  :: Compiler
  -> IO InstallDirs.InstallDirTemplates
userInstallDirTemplates compiler = do
  InstallDirs.defaultInstallDirs
    (compilerFlavor compiler)
    True -- user install
    False -- unused

storePackageInstallDirs
  :: StoreDirLayout
  -> Compiler
  -> InstalledPackageId
  -> InstallDirs.InstallDirs FilePath
storePackageInstallDirs storeDirLayout compiler ipkgid =
  storePackageInstallDirs' storeDirLayout compiler $ newSimpleUnitId ipkgid

storePackageInstallDirs'
  :: StoreDirLayout
  -> Compiler
  -> UnitId
  -> InstallDirs.InstallDirs FilePath
storePackageInstallDirs'
  StoreDirLayout
    { storePackageDirectory
    , storeDirectory
    }
  compiler
  unitid =
    InstallDirs.InstallDirs{..}
    where
      store = storeDirectory compiler
      prefix = storePackageDirectory compiler unitid
      bindir = prefix </> "bin"
      libdir = prefix </> "lib"
      libsubdir = ""
      -- Note: on macOS, we place libraries into
      --       @store/lib@ to work around the load
      --       command size limit of macOSs mach-o linker.
      --       See also @PackageHash.hashedInstalledPackageIdVeryShort@
      dynlibdir
        | buildOS == OSX = store </> "lib"
        | otherwise = libdir
      flibdir = libdir
      libexecdir = prefix </> "libexec"
      libexecsubdir = ""
      includedir = libdir </> "include"
      datadir = prefix </> "share"
      datasubdir = ""
      docdir = datadir </> "doc"
      mandir = datadir </> "man"
      htmldir = docdir </> "html"
      haddockdir = htmldir
      sysconfdir = prefix </> "etc"

computeInstallDirs
  :: StoreDirLayout
  -> Staged InstallDirs.InstallDirTemplates
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> InstallDirs.InstallDirs FilePath
computeInstallDirs storeDirLayout defaultInstallDirs sharedConfig elab =
  if isInplaceBuildStyle (elabBuildStyle elab)
    then -- use the ordinary default install dirs

      ( InstallDirs.absoluteInstallDirs
          (elabPkgSourceId elab)
          (elabUnitId elab)
          (compilerInfo toolchainCompiler)
          InstallDirs.NoCopyDest
          toolchainPlatform
          defaultInstallDirs'
      )
        { -- absoluteInstallDirs sets these as 'undefined' but we have
          -- to use them as "Setup.hs configure" args
          InstallDirs.libsubdir = ""
        , InstallDirs.libexecsubdir = ""
        , InstallDirs.datasubdir = ""
        }
    else -- use special simplified install dirs

      storePackageInstallDirs'
        storeDirLayout
        toolchainCompiler
        (elabUnitId elab)
  where
    Toolchain{toolchainCompiler, toolchainPlatform} = getStage (pkgConfigToolchains sharedConfig) (elabStage elab)
    defaultInstallDirs' = getStage defaultInstallDirs (elabStage elab)

-- TODO: [code cleanup] perhaps reorder this code
-- based on the ElaboratedInstallPlan + ElaboratedSharedConfig,
-- make the various Setup.hs {configure,build,copy} flags
setupHsConfigureFlags
  :: Monad m
  => (FilePath -> m (SymbolicPath Pkg (Dir PkgDB)))
  -- ^ How to transform a path which is relative to cabal-install cwd to one which
  -- is relative to the route of the package about to be compiled. The simplest way
  -- to do this is to convert the potentially relative path into an absolute path.
  -> ElaboratedInstallPlan
  -> ElaboratedReadyPackage
  -> ElaboratedSharedConfig
  -> Cabal.CommonSetupFlags
  -> m Cabal.ConfigFlags
setupHsConfigureFlags
  mkSymbolicPath
  plan
  (ReadyPackage elab@ElaboratedConfiguredPackage{..})
  sharedConfig
  configCommonFlags = do
    -- explicitly clear, then our package db stack
    -- TODO: [required eventually] have to do this differently for older Cabal versions
    configPackageDBs <- (traverse . traverse . traverse) mkSymbolicPath (Nothing : map Just elabBuildPackageDBStack)
    return $
      sanityCheckElaboratedConfiguredPackage
        sharedConfig
        elab
        Cabal.ConfigFlags{..}
    where
      Toolchain{toolchainCompiler} = getStage (pkgConfigToolchains sharedConfig) elabStage

      Cabal.ConfigFlags
        { configVanillaLib
        , configSharedLib
        , configStaticLib
        , configDynExe
        , configFullyStaticExe
        , configGHCiLib
        , -- , configProfExe -- overridden
        configProfLib
        , configProfShared
        , -- , configProf -- overridden
        configProfDetail
        , configProfLibDetail
        , configCoverage
        , configLibCoverage
        , configRelocatable
        , configOptimization
        , configSplitSections
        , configSplitObjs
        , configStripExes
        , configStripLibs
        , configDebugInfo
        } = LBC.buildOptionsConfigFlags elabBuildOptions
      configProfExe = mempty
      configProf = toFlag $ LBC.withProfExe elabBuildOptions

      configInstantiateWith = case elabPkgOrComp of
        ElabPackage _ -> mempty
        ElabComponent comp -> Map.toList (compInstantiatedWith comp)

      configDeterministic = mempty -- doesn't matter, configIPID/configCID overridese
      configIPID = case elabPkgOrComp of
        ElabPackage pkg -> toFlag (prettyShow (pkgInstalledId pkg))
        ElabComponent _ -> mempty
      configCID = case elabPkgOrComp of
        ElabPackage _ -> mempty
        ElabComponent _ -> toFlag elabComponentId

      configProgramPaths = Map.toList elabProgramPaths
      configProgramArgs
        | {- elabSetupScriptCliVersion < mkVersion [1,24,3] -} True =
            -- workaround for <https://github.com/haskell/cabal/issues/4010>
            --
            -- It turns out, that even with Cabal 2.0, there's still cases such as e.g.
            -- custom Setup.hs scripts calling out to GHC even when going via
            -- @runProgram ghcProgram@, as e.g. happy does in its
            -- <http://hackage.haskell.org/package/happy-1.19.5/src/Setup.lhs>
            -- (see also <https://github.com/haskell/cabal/pull/4433#issuecomment-299396099>)
            --
            -- So for now, let's pass the rather harmless and idempotent
            -- `-hide-all-packages` flag to all invocations (which has
            -- the benefit that every GHC invocation starts with a
            -- consistently well-defined clean slate) until we find a
            -- better way.
            Map.toList $
              Map.insertWith
                (++)
                "ghc"
                ["-hide-all-packages"]
                elabProgramArgs
      configProgramPathExtra = toNubList elabProgramPathExtra
      configHcFlavor = toFlag (compilerFlavor toolchainCompiler)
      configHcPath = mempty -- we use configProgramPaths instead
      configHcPkg = mempty -- we use configProgramPaths instead
      configDumpBuildInfo = toFlag elabDumpBuildInfo

      configConfigurationsFlags = elabFlagAssignment
      configConfigureArgs = elabConfigureScriptArgs
      configExtraLibDirs = fmap makeSymbolicPath $ elabExtraLibDirs
      configExtraLibDirsStatic = fmap makeSymbolicPath $ elabExtraLibDirsStatic
      configExtraFrameworkDirs = fmap makeSymbolicPath $ elabExtraFrameworkDirs
      configExtraIncludeDirs = fmap makeSymbolicPath $ elabExtraIncludeDirs
      configProgPrefix = maybe mempty toFlag elabProgPrefix
      configProgSuffix = maybe mempty toFlag elabProgSuffix

      configInstallDirs =
        fmap
          (toFlag . InstallDirs.toPathTemplate)
          elabInstallDirs

      -- we only use configDependencies, unless we're talking to an old Cabal
      -- in which case we use configConstraints
      -- NB: This does NOT use InstallPlan.depends, which includes executable
      -- dependencies which should NOT be fed in here (also you don't have
      -- enough info anyway)
      --
      -- FIXME: stage?
      configDependencies =
        [ cidToGivenComponent cid
        | (WithStage _stage cid, is_internal) <- elabLibDependencies elab
        , not is_internal
        ]

      -- FIXME: stage?
      configPromisedDependencies =
        [ cidToPromisedComponent cid
        | (WithStage _stage cid, is_internal) <- elabLibDependencies elab
        , is_internal
        ]

      -- FIXME: stage?
      configConstraints =
        case elabPkgOrComp of
          ElabPackage _ ->
            [ thisPackageVersionConstraint srcid
            | (WithStage _stage (ConfiguredId srcid _ _uid), _) <- elabLibDependencies elab
            ]
          ElabComponent _ -> []

      configTests = case elabPkgOrComp of
        ElabPackage pkg -> toFlag (TestStanzas `optStanzaSetMember` pkgStanzasEnabled pkg)
        ElabComponent _ -> mempty

      configBenchmarks = case elabPkgOrComp of
        ElabPackage pkg -> toFlag (BenchStanzas `optStanzaSetMember` pkgStanzasEnabled pkg)
        ElabComponent _ -> mempty

      configExactConfiguration = toFlag True
      configFlagError = mempty -- TODO: [research required] appears not to be implemented
      configScratchDir = mempty -- never use
      configUserInstall = mempty -- don't rely on defaults
      configPrograms_ = mempty -- never use, shouldn't exist
      configUseResponseFiles = mempty
      configAllowDependingOnPrivateLibs = Flag $ not $ libraryVisibilitySupported toolchainCompiler
      configIgnoreBuildTools = mempty

      cidToGivenComponent :: ConfiguredId -> GivenComponent
      cidToGivenComponent (ConfiguredId srcid mb_cn cid) = GivenComponent (packageName srcid) ln cid
        where
          ln = case mb_cn of
            Just (CLibName lname) -> lname
            Just _ -> error "non-library dependency"
            Nothing -> LMainLibName

      -- FIXME: whathever
      -- configCoverageFor = determineCoverageFor elab plan
      configCoverageFor = NoFlag

      cidToPromisedComponent :: ConfiguredId -> PromisedComponent
      cidToPromisedComponent (ConfiguredId srcid mb_cn cid) =
        PromisedComponent srcid ln cid
        where
          ln = case mb_cn of
            Just (CLibName lname) -> lname
            Just _ -> error "non-library dependency"
            Nothing -> LMainLibName

setupHsConfigureArgs
  :: ElaboratedConfiguredPackage
  -> [String]
setupHsConfigureArgs (ElaboratedConfiguredPackage{elabPkgOrComp = ElabPackage _}) = []
setupHsConfigureArgs elab@(ElaboratedConfiguredPackage{elabPkgOrComp = ElabComponent comp}) =
  [showComponentTarget (packageId elab) (ComponentTarget cname WholeComponent)]
  where
    cname =
      fromMaybe
        (error "setupHsConfigureArgs: trying to configure setup")
        (compComponentName comp)

setupHsCommonFlags
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg (Dir Dist)
  -> Bool
  -> Cabal.CommonSetupFlags
setupHsCommonFlags verbosity mbWorkDir builddir keepTempFiles =
  Cabal.CommonSetupFlags
    { setupDistPref = toFlag builddir
    , setupVerbosity = toFlag verbosity
    , setupCabalFilePath = mempty
    , setupWorkingDir = maybeToFlag mbWorkDir
    , setupTargets = []
    , setupKeepTempFiles = toFlag keepTempFiles
    }

setupHsBuildFlags
  :: Flag String
  -> ElaboratedConfiguredPackage
  -> ElaboratedSharedConfig
  -> Cabal.CommonSetupFlags
  -> Cabal.BuildFlags
setupHsBuildFlags par_strat elab _ common =
  Cabal.BuildFlags
    { buildCommonFlags = common
    , buildProgramPaths = mempty -- unused, set at configure time
    , buildProgramArgs = mempty -- unused, set at configure time
    , buildNumJobs = mempty -- TODO: [nice to have] sometimes want to use toFlag (Just numBuildJobs),
    , buildUseSemaphore =
        if elabSetupScriptCliVersion elab >= mkVersion [3, 11, 0, 0]
          then -- Cabal 3.11 is the first version that supports parallelism semaphores
            par_strat
          else mempty
    }

setupHsBuildArgs :: ElaboratedConfiguredPackage -> [String]
setupHsBuildArgs elab@(ElaboratedConfiguredPackage{elabPkgOrComp = ElabPackage _})
  -- Fix for #3335, don't pass build arguments if it's not supported
  | elabSetupScriptCliVersion elab >= mkVersion [1, 17] =
      map (showComponentTarget (packageId elab)) (elabBuildTargets elab)
  | otherwise =
      []
setupHsBuildArgs (ElaboratedConfiguredPackage{elabPkgOrComp = ElabComponent _}) =
  []

setupHsTestFlags
  :: ElaboratedConfiguredPackage
  -> Cabal.CommonSetupFlags
  -> Cabal.TestFlags
setupHsTestFlags (ElaboratedConfiguredPackage{..}) common =
  Cabal.TestFlags
    { testCommonFlags = common
    , testMachineLog = maybe mempty toFlag elabTestMachineLog
    , testHumanLog = maybe mempty toFlag elabTestHumanLog
    , testShowDetails = maybe (Flag Cabal.Always) toFlag elabTestShowDetails
    , testKeepTix = toFlag elabTestKeepTix
    , testWrapper = maybe mempty toFlag elabTestWrapper
    , testFailWhenNoTestSuites = toFlag elabTestFailWhenNoTestSuites
    , testOptions = elabTestTestOptions
    }

setupHsTestArgs :: ElaboratedConfiguredPackage -> [String]
-- TODO: Does the issue #3335 affects test as well
setupHsTestArgs elab =
  mapMaybe (showTestComponentTarget (packageId elab)) (elabTestTargets elab)

setupHsBenchFlags
  :: ElaboratedConfiguredPackage
  -> ElaboratedSharedConfig
  -> Cabal.CommonSetupFlags
  -> Cabal.BenchmarkFlags
setupHsBenchFlags (ElaboratedConfiguredPackage{..}) _ common =
  Cabal.BenchmarkFlags
    { benchmarkCommonFlags = common
    , benchmarkOptions = elabBenchmarkOptions
    }

setupHsBenchArgs :: ElaboratedConfiguredPackage -> [String]
setupHsBenchArgs elab =
  mapMaybe (showBenchComponentTarget (packageId elab)) (elabBenchTargets elab)

setupHsReplFlags
  :: ElaboratedConfiguredPackage
  -> ElaboratedSharedConfig
  -> Cabal.CommonSetupFlags
  -> Cabal.ReplFlags
setupHsReplFlags _ sharedConfig common =
  Cabal.ReplFlags
    { replCommonFlags = common
    , replProgramPaths = mempty -- unused, set at configure time
    , replProgramArgs = mempty -- unused, set at configure time
    , replReload = mempty -- only used as callback from repl
    , replReplOptions = pkgConfigReplOptions sharedConfig -- runtime override for repl flags
    }

setupHsReplArgs :: ElaboratedConfiguredPackage -> [String]
setupHsReplArgs elab =
  map (\t -> showComponentTarget (packageId elab) t) (elabReplTarget elab)

setupHsCopyFlags
  :: ElaboratedConfiguredPackage
  -> ElaboratedSharedConfig
  -> Cabal.CommonSetupFlags
  -> FilePath
  -> Cabal.CopyFlags
setupHsCopyFlags _ _ common destdir =
  Cabal.CopyFlags
    { copyCommonFlags = common
    , copyDest = toFlag (InstallDirs.CopyTo destdir)
    }

setupHsRegisterFlags
  :: ElaboratedConfiguredPackage
  -> ElaboratedSharedConfig
  -> Cabal.CommonSetupFlags
  -> FilePath
  -> Cabal.RegisterFlags
setupHsRegisterFlags
  ElaboratedConfiguredPackage{..}
  _
  common
  pkgConfFile =
    Cabal.RegisterFlags
      { registerCommonFlags = common
      , regPackageDB = mempty -- misfeature
      , regGenScript = mempty -- never use
      , regGenPkgConf = toFlag (Just (makeSymbolicPath pkgConfFile))
      , regInPlace = case elabBuildStyle of
          BuildInplaceOnly{} -> toFlag True
          BuildAndInstall -> toFlag False
      , regPrintId = mempty -- never use
      }

setupHsHaddockFlags
  :: ElaboratedConfiguredPackage
  -> ElaboratedSharedConfig
  -> BuildTimeSettings
  -> Cabal.CommonSetupFlags
  -> Cabal.HaddockFlags
setupHsHaddockFlags
  (ElaboratedConfiguredPackage{..})
  sharedConfig
  _buildTimeSettings
  common =
    Cabal.HaddockFlags
      { haddockCommonFlags = common
      , haddockProgramPaths =
          case lookupProgram haddockProgram toolchainProgramDb of
            Nothing -> mempty
            Just prg ->
              [
                ( programName haddockProgram
                , locationPath (programLocation prg)
                )
              ]
      , haddockProgramArgs = mempty -- unused, set at configure time
      , haddockHoogle = toFlag elabHaddockHoogle
      , haddockHtml = toFlag elabHaddockHtml
      , haddockHtmlLocation = maybe mempty toFlag elabHaddockHtmlLocation
      , haddockForHackage = toFlag elabHaddockForHackage
      , haddockForeignLibs = toFlag elabHaddockForeignLibs
      , haddockExecutables = toFlag elabHaddockExecutables
      , haddockTestSuites = toFlag elabHaddockTestSuites
      , haddockBenchmarks = toFlag elabHaddockBenchmarks
      , haddockInternal = toFlag elabHaddockInternal
      , haddockCss = maybe mempty toFlag elabHaddockCss
      , haddockLinkedSource = toFlag elabHaddockLinkedSource
      , haddockQuickJump = toFlag elabHaddockQuickJump
      , haddockHscolourCss = maybe mempty toFlag elabHaddockHscolourCss
      , haddockContents = maybe mempty toFlag elabHaddockContents
      , haddockIndex = maybe mempty toFlag elabHaddockIndex
      , haddockBaseUrl = maybe mempty toFlag elabHaddockBaseUrl
      , haddockResourcesDir = maybe mempty toFlag elabHaddockResourcesDir
      , haddockOutputDir = maybe mempty toFlag elabHaddockOutputDir
      , haddockUseUnicode = toFlag elabHaddockUseUnicode
      }
    where
      Toolchain{toolchainProgramDb} = getStage (pkgConfigToolchains sharedConfig) elabStage

setupHsHaddockArgs :: ElaboratedConfiguredPackage -> [String]
-- TODO: Does the issue #3335 affects test as well
setupHsHaddockArgs elab =
  map (showComponentTarget (packageId elab)) (elabHaddockTargets elab)

------------------------------------------------------------------------------

-- * Sharing installed packages

------------------------------------------------------------------------------

--
-- Nix style store management for tarball packages
--
-- So here's our strategy:
--
-- We use a per-user nix-style hashed store, but /only/ for tarball packages.
-- So that includes packages from hackage repos (and other http and local
-- tarballs). For packages in local directories we do not register them into
-- the shared store by default, we just build them locally inplace.
--
-- The reason we do it like this is that it's easy to make stable hashes for
-- tarball packages, and these packages benefit most from sharing. By contrast
-- unpacked dir packages are harder to hash and they tend to change more
-- frequently so there's less benefit to sharing them.
--
-- When using the nix store approach we have to run the solver *without*
-- looking at the packages installed in the store, just at the source packages
-- (plus core\/global installed packages). Then we do a post-processing pass
-- to replace configured packages in the plan with pre-existing ones, where
-- possible. Where possible of course means where the nix-style package hash
-- equals one that's already in the store.
--
-- One extra wrinkle is that unless we know package tarball hashes upfront, we
-- will have to download the tarballs to find their hashes. So we have two
-- options: delay replacing source with pre-existing installed packages until
-- the point during the execution of the install plan where we have the
-- tarball, or try to do as much up-front as possible and then check again
-- during plan execution. The former isn't great because we would end up
-- telling users we're going to re-install loads of packages when in fact we
-- would just share them. It'd be better to give as accurate a prediction as
-- we can. The latter is better for users, but we do still have to check
-- during plan execution because it's important that we don't replace existing
-- installed packages even if they have the same package hash, because we
-- don't guarantee ABI stability.

-- TODO: [required eventually] for safety of concurrent installs, we must make sure we register but
-- not replace installed packages with ghc-pkg.

packageHashInputs
  :: ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> PackageHashInputs
packageHashInputs
  pkgshared
  elab@( ElaboratedConfiguredPackage
          { elabPkgSourceHash = Just srchash
          }
        ) =
    PackageHashInputs
      { pkgHashPkgId = packageId elab
      , pkgHashComponent
      , pkgHashSourceHash = srchash
      , pkgHashPkgConfigDeps = Set.fromList (elabPkgConfigDependencies elab)
      , pkgHashLibDeps
      , pkgHashExeDeps
      , pkgHashOtherConfig = packageHashConfigInputs pkgshared elab
      }
    where
      pkgHashComponent =
        case elabPkgOrComp elab of
          ElabPackage _ -> Nothing
          ElabComponent comp -> Just (compSolverName comp)
      pkgHashLibDeps =
        case elabPkgOrComp elab of
          ElabPackage (ElaboratedPackage{..}) ->
            Set.fromList
              [confInstId c | (c, _promised) <- CD.select relevantDeps pkgLibDependencies]
          ElabComponent comp ->
            Set.fromList
              [confInstId c | (c, _promised) <- compLibDependencies comp]
      pkgHashExeDeps =
        case elabPkgOrComp elab of
          ElabPackage (ElaboratedPackage{..}) ->
            Set.fromList
              [ confInstId c
              | WithStage _stage c <- CD.select relevantDeps pkgExeDependencies
              ]
          ElabComponent comp ->
            Set.fromList
              [ confInstId c
              | WithStage _stage c <- compExeDependencies comp
              ]

      -- Obviously the main deps are relevant
      relevantDeps CD.ComponentLib = True
      relevantDeps (CD.ComponentSubLib _) = True
      relevantDeps (CD.ComponentFLib _) = True
      relevantDeps (CD.ComponentExe _) = True
      -- Setup deps can affect the Setup.hs behaviour and thus what is built
      relevantDeps CD.ComponentSetup = True
      -- However testsuites and benchmarks do not get installed and should not
      -- affect the result, so we do not include them.
      relevantDeps (CD.ComponentTest _) = False
      relevantDeps (CD.ComponentBench _) = False
packageHashInputs _ pkg =
  error $
    "packageHashInputs: only for packages with source hashes. "
      ++ prettyShow (packageId pkg)

packageHashConfigInputs
  :: ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> PackageHashConfigInputs
packageHashConfigInputs sharedConfig pkg =
  PackageHashConfigInputs
    { pkgHashCompilerId = compilerId toolchainCompiler
    , pkgHashCompilerABI = compilerAbiTag toolchainCompiler
    , pkgHashPlatform = toolchainPlatform
    , pkgHashFlagAssignment = elabFlagAssignment
    , pkgHashConfigureScriptArgs = elabConfigureScriptArgs
    , pkgHashVanillaLib = withVanillaLib
    , pkgHashSharedLib = withSharedLib
    , pkgHashDynExe = withDynExe
    , pkgHashFullyStaticExe = withFullyStaticExe
    , pkgHashGHCiLib = withGHCiLib
    , pkgHashProfLib = withProfLib
    , pkgHashProfExe = withProfExe
    , pkgHashProfLibDetail = withProfLibDetail
    , pkgHashProfExeDetail = withProfExeDetail
    , pkgHashCoverage = exeCoverage
    , pkgHashOptimization = withOptimization
    , pkgHashSplitSections = splitSections
    , pkgHashSplitObjs = splitObjs
    , pkgHashStripLibs = stripLibs
    , pkgHashStripExes = stripExes
    , pkgHashDebugInfo = withDebugInfo
    , pkgHashProgramArgs = elabProgramArgs
    , pkgHashExtraLibDirs = elabExtraLibDirs
    , pkgHashExtraLibDirsStatic = elabExtraLibDirsStatic
    , pkgHashExtraFrameworkDirs = elabExtraFrameworkDirs
    , pkgHashExtraIncludeDirs = elabExtraIncludeDirs
    , pkgHashProgPrefix = elabProgPrefix
    , pkgHashProgSuffix = elabProgSuffix
    , pkgHashPackageDbs = elabPackageDbs
    , pkgHashDocumentation = elabBuildHaddocks
    , pkgHashHaddockHoogle = elabHaddockHoogle
    , pkgHashHaddockHtml = elabHaddockHtml
    , pkgHashHaddockHtmlLocation = elabHaddockHtmlLocation
    , pkgHashHaddockForeignLibs = elabHaddockForeignLibs
    , pkgHashHaddockExecutables = elabHaddockExecutables
    , pkgHashHaddockTestSuites = elabHaddockTestSuites
    , pkgHashHaddockBenchmarks = elabHaddockBenchmarks
    , pkgHashHaddockInternal = elabHaddockInternal
    , pkgHashHaddockCss = elabHaddockCss
    , pkgHashHaddockLinkedSource = elabHaddockLinkedSource
    , pkgHashHaddockQuickJump = elabHaddockQuickJump
    , pkgHashHaddockContents = elabHaddockContents
    , pkgHashHaddockIndex = elabHaddockIndex
    , pkgHashHaddockBaseUrl = elabHaddockBaseUrl
    , pkgHashHaddockResourcesDir = elabHaddockResourcesDir
    , pkgHashHaddockOutputDir = elabHaddockOutputDir
    , pkgHashHaddockUseUnicode = elabHaddockUseUnicode
    }
  where
    Toolchain{toolchainCompiler, toolchainPlatform} = getStage (pkgConfigToolchains sharedConfig) elabStage
    ElaboratedConfiguredPackage{..} = normaliseConfiguredPackage sharedConfig pkg
    LBC.BuildOptions{..} = elabBuildOptions

-- TODO: sanity checks:
-- \* the installed package must have the expected deps etc
-- \* the installed package must not be broken, valid dep closure

-- TODO: decide what to do if we encounter broken installed packages,
-- since overwriting is never safe.

-- Path construction
------

-- | The path to the directory that contains a specific executable.
-- NB: For inplace NOT InstallPaths.bindir installDirs; for an
-- inplace build those values are utter nonsense.  So we
-- have to guess where the directory is going to be.
-- Fortunately this is "stable" part of Cabal API.
-- But the way we get the build directory is A HORRIBLE
-- HACK.
binDirectoryFor
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> FilePath
  -> FilePath
binDirectoryFor layout config package exe = case elabBuildStyle package of
  BuildAndInstall -> installedBinDirectory package
  BuildInplaceOnly{} -> inplaceBinRoot layout config package </> exe

-- package has been built and installed.
installedBinDirectory :: ElaboratedConfiguredPackage -> FilePath
installedBinDirectory = InstallDirs.bindir . elabInstallDirs

-- | The path to the @build@ directory for an inplace build.
inplaceBinRoot
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> FilePath
inplaceBinRoot layout config package =
  distBuildDirectory layout (elabDistDirParams config package)
    </> "build"

-- FIXME: whathever
-- --------------------------------------------------------------------------------
-- -- Configure --coverage-for flags

-- The list of non-pre-existing libraries without module holes, i.e. the
-- main library and sub-libraries components of all the local packages in
-- the project that are dependencies of the components being built and that do
-- not require instantiations or are instantiations.
-- determineCoverageFor
--   :: ElaboratedConfiguredPackage
--   -- ^ The package or component being configured
--   -> ElaboratedInstallPlan
--   -> Flag [UnitId]
-- determineCoverageFor configuredPkg plan =
--   Flag
--     $ mapMaybe
--       ( \case
--           InstallPlan.Installed elab
--             | shouldCoverPkg elab -> Just $ elabUnitId elab
--           InstallPlan.Configured elab
--             | shouldCoverPkg elab -> Just $ elabUnitId elab
--           _ -> Nothing
--       )
--     $ Graph.toList
--     $ InstallPlan.toGraph plan
--   where
--     libDeps = elabLibDependencies configuredPkg
--     shouldCoverPkg elab@ElaboratedConfiguredPackage{elabModuleShape, elabPkgSourceId = pkgSID, elabLocalToProject} =
--       elabLocalToProject
--         && not (isIndefiniteOrInstantiation elabModuleShape)
--         -- TODO(#9493): We can only cover libraries in the same package
--         -- as the testsuite
--         && elabPkgSourceId configuredPkg == pkgSID
--         -- Libraries only! We don't cover testsuite modules, so we never need
--         -- the paths to their mix dirs. Furthermore, we do not install testsuites...
--         && maybe False (\case CLibName{} -> True; CNotLibName{} -> False) (elabComponentName elab)
--         -- We only want coverage for libraries which are dependencies of the given one
--         && pkgSID `elem` map (confSrcId . fst) libDeps

--     isIndefiniteOrInstantiation :: ModuleShape -> Bool
--     isIndefiniteOrInstantiation = not . Set.null . modShapeRequires

-- While we can talk to older Cabal versions (we need to be able to
-- do so for custom Setup scripts that require older Cabal lib
-- versions), we have problems talking to some older versions that
-- don't support certain features.
--
-- For example, Cabal-1.16 and older do not know about build targets.
-- Even worse, 1.18 and older only supported the --constraint flag
-- with source package ids, not --dependency with installed package
-- ids. That is bad because we cannot reliably select the right
-- dependencies in the presence of multiple instances (i.e. the
-- store). See issue #3932. So we require Cabal 1.20 as a minimum.
--
-- Moreover, lib:Cabal generally only supports the interface of
-- current and past compilers; in fact recent lib:Cabal versions
-- will warn when they encounter a too new or unknown GHC compiler
-- version (c.f. #415). To avoid running into unsupported
-- configurations we encode the compatibility matrix as lower
-- bounds on lib:Cabal here (effectively corresponding to the
-- respective major Cabal version bundled with the respective GHC
-- release).
--
-- GHC 9.2   needs  Cabal >= 3.6
-- GHC 9.0   needs  Cabal >= 3.4
-- GHC 8.10  needs  Cabal >= 3.2
-- GHC 8.8   needs  Cabal >= 3.0
-- GHC 8.6   needs  Cabal >= 2.4
-- GHC 8.4   needs  Cabal >= 2.2
-- GHC 8.2   needs  Cabal >= 2.0
-- GHC 8.0   needs  Cabal >= 1.24
-- GHC 7.10  needs  Cabal >= 1.22
--
-- (NB: we don't need to consider older GHCs as Cabal >= 1.20 is
-- the absolute lower bound)
--
-- TODO: long-term, this compatibility matrix should be
--       stored as a field inside 'Distribution.Compiler.Compiler'
--
-- setupMinCabalVersionConstraint :: Compiler -> Version
-- setupMinCabalVersionConstraint compiler
--   | isGHC, compVer >= mkVersion [9, 10] = mkVersion [3, 12]
--   | isGHC, compVer >= mkVersion [9, 6] = mkVersion [3, 10]
--   | isGHC, compVer >= mkVersion [9, 4] = mkVersion [3, 8]
--   | isGHC, compVer >= mkVersion [9, 2] = mkVersion [3, 6]
--   | isGHC, compVer >= mkVersion [9, 0] = mkVersion [3, 4]
--   | isGHC, compVer >= mkVersion [8, 10] = mkVersion [3, 2]
--   | isGHC, compVer >= mkVersion [8, 8] = mkVersion [3, 0]
--   | isGHC, compVer >= mkVersion [8, 6] = mkVersion [2, 4]
--   | isGHC, compVer >= mkVersion [8, 4] = mkVersion [2, 2]
--   | isGHC, compVer >= mkVersion [8, 2] = mkVersion [2, 0]
--   | isGHC, compVer >= mkVersion [8, 0] = mkVersion [1, 24]
--   | isGHC, compVer >= mkVersion [7, 10] = mkVersion [1, 22]
--   | otherwise = mkVersion [1, 20]
--   where
--     isGHC = compFlav `elem` [GHC, GHCJS]
--     compFlav = compilerFlavor compiler
--     compVer = compilerVersion compiler

-- As we can't predict the future, we also place a global upper
-- bound on the lib:Cabal version we know how to interact with:
--
-- The upper bound is computed by incrementing the current major
-- version twice in order to allow for the current version, as
-- well as the next adjacent major version (one of which will not
-- be released, as only "even major" versions of Cabal are
-- released to Hackage or bundled with proper GHC releases).
--
-- For instance, if the current version of cabal-install is an odd
-- development version, e.g.  Cabal-2.1.0.0, then we impose an
-- upper bound `setup.Cabal < 2.3`; if `cabal-install` is on a
-- stable/release even version, e.g. Cabal-2.2.1.0, the upper
-- bound is `setup.Cabal < 2.4`. This gives us enough flexibility
-- when dealing with development snapshots of Cabal and cabal-install.
--
-- setupMaxCabalVersionConstraint :: Version
-- setupMaxCabalVersionConstraint =
--   alterVersion (take 2) $ incVersion 1 $ incVersion 1 cabalVersion
