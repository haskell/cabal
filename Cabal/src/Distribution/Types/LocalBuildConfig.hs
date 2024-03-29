{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Types.LocalBuildConfig
  ( -- * The types
    PackageBuildDescr (..)
  , ComponentBuildDescr (..)
  , LocalBuildDescr (..)
  , LocalBuildConfig (..)
  , BuildOptions (..)

    -- * Conversion functions
  , buildOptionsConfigFlags
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.ComponentId
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.PackageDescription
import Distribution.Types.UnitId

import Distribution.PackageDescription
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs hiding
  ( absoluteInstallDirs
  , prefixRelativeInstallDirs
  , substPathTemplate
  )
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program.Db (ProgramDb)
import Distribution.Simple.Setup.Config
import Distribution.System
import Distribution.Utils.Path

import Distribution.Compat.Graph (Graph)

-- | 'PackageBuildDescr' contains the information Cabal determines after
-- performing package-wide configuration of a package, before doing any
-- per-component configuration.
data PackageBuildDescr = PackageBuildDescr
  { configFlags :: ConfigFlags
  -- ^ Options passed to the configuration step.
  -- Needed to re-run configuration when .cabal is out of date
  , flagAssignment :: FlagAssignment
  -- ^ The final set of flags which were picked for this package
  , componentEnabledSpec :: ComponentRequestedSpec
  -- ^ What components were enabled during configuration, and why.
  , compiler :: Compiler
  -- ^ The compiler we're building with
  , hostPlatform :: Platform
  -- ^ The platform we're building for
  , pkgDescrFile :: Maybe (SymbolicPath Pkg File)
  -- ^ the filename containing the .cabal file, if available
  , localPkgDescr :: PackageDescription
  -- ^ WARNING WARNING WARNING Be VERY careful about using
  -- this function; we haven't deprecated it but using it
  -- could introduce subtle bugs related to
  -- 'HookedBuildInfo'.
  --
  -- In principle, this is supposed to contain the
  -- resolved package description, that does not contain
  -- any conditionals.  However, it MAY NOT contain
  -- the description with a 'HookedBuildInfo' applied
  -- to it; see 'HookedBuildInfo' for the whole sordid saga.
  -- As much as possible, Cabal library should avoid using
  -- this parameter.
  , installDirTemplates :: InstallDirTemplates
  -- ^ The installation directories for the various different
  -- kinds of files
  -- TODO: inplaceDirTemplates :: InstallDirs FilePath
  , withPackageDB :: PackageDBStack
  -- ^ What package database to use, global\/user
  , extraCoverageFor :: [UnitId]
  -- ^ For per-package builds-only: an extra list of libraries to be included in
  -- the hpc coverage report for testsuites run with @--enable-coverage@.
  -- Notably, this list must exclude indefinite libraries and instantiations
  -- because HPC does not support backpack (Nov. 2023).
  }
  deriving (Generic, Read, Show)

-- | Information about individual components in a package,
-- determined after the configure step.
data ComponentBuildDescr = ComponentBuildDescr
  { componentGraph :: Graph ComponentLocalBuildInfo
  -- ^ All the components to build, ordered by topological
  -- sort, and with their INTERNAL dependencies over the
  -- intrapackage dependency graph.
  -- TODO: this is assumed to be short; otherwise we want
  -- some sort of ordered map.
  , componentNameMap :: Map ComponentName [ComponentLocalBuildInfo]
  -- ^ A map from component name to all matching
  -- components.  These coincide with 'componentGraph'
  -- There may be more than one matching component because of backpack instantiations
  , promisedPkgs :: Map (PackageName, ComponentName) ComponentId
  -- ^ The packages we were promised, but aren't already installed.
  -- MP: Perhaps this just needs to be a Set UnitId at this stage.
  , installedPkgs :: InstalledPackageIndex
  -- ^ All the info about the installed packages that the
  -- current package depends on (directly or indirectly).
  -- The copy saved on disk does NOT include internal
  -- dependencies (because we just don't have enough
  -- information at this point to have an
  -- 'InstalledPackageInfo' for an internal dep), but we
  -- will often update it with the internal dependencies;
  -- see for example 'Distribution.Simple.Build.build'.
  -- (This admonition doesn't apply for per-component builds.)
  }
  deriving (Generic, Read, Show)

-- | 'LocalBuildDescr ' contains the information Cabal determines after
-- performing package-wide and per-component configuration of a package.
--
-- This information can no longer be changed after that point.
data LocalBuildDescr = LocalBuildDescr
  { packageBuildDescr :: PackageBuildDescr
  -- ^ Information that is available after configuring the package itself,
  -- before looking at individual components.
  , componentBuildDescr :: ComponentBuildDescr
  -- ^ Information about individual components in the package
  -- determined after the configure step.
  }
  deriving (Generic, Read, Show)

-- | 'LocalBuildConfig' contains options that can be controlled
-- by the user and serve as inputs to the configuration of a package.
data LocalBuildConfig = LocalBuildConfig
  { extraConfigArgs :: [String]
  -- ^ Extra args on the command line for the configuration step.
  -- Needed to re-run configuration when .cabal is out of date
  , withPrograms :: ProgramDb
  -- ^ Location and args for all programs
  , withBuildOptions :: BuildOptions
  -- ^ Options to control the build, e.g. whether to
  -- enable profiling or to enable program coverage.
  }
  deriving (Generic, Read, Show)

-- | 'BuildOptions' contains configuration options that can be controlled
-- by the user.
data BuildOptions = BuildOptions
  { withVanillaLib :: Bool
  -- ^ Whether to build normal libs.
  , withProfLib :: Bool
  -- ^ Whether to build profiling versions of libs.
  , withSharedLib :: Bool
  -- ^ Whether to build shared versions of libs.
  , withStaticLib :: Bool
  -- ^ Whether to build static versions of libs (with all other libs rolled in)
  , withDynExe :: Bool
  -- ^ Whether to link executables dynamically
  , withFullyStaticExe :: Bool
  -- ^ Whether to link executables fully statically
  , withProfExe :: Bool
  -- ^ Whether to build executables for profiling.
  , withProfLibDetail :: ProfDetailLevel
  -- ^ Level of automatic profile detail.
  , withProfExeDetail :: ProfDetailLevel
  -- ^ Level of automatic profile detail.
  , withOptimization :: OptimisationLevel
  -- ^ Whether to build with optimization (if available).
  , withDebugInfo :: DebugInfoLevel
  -- ^ Whether to emit debug info (if available).
  , withGHCiLib :: Bool
  -- ^ Whether to build libs suitable for use with GHCi.
  , splitSections :: Bool
  -- ^ Use -split-sections with GHC, if available
  , splitObjs :: Bool
  -- ^ Use -split-objs with GHC, if available
  , stripExes :: Bool
  -- ^ Whether to strip executables during install
  , stripLibs :: Bool
  -- ^ Whether to strip libraries during install
  , exeCoverage :: Bool
  -- ^ Whether to enable executable program coverage
  , libCoverage :: Bool
  -- ^ Whether to enable library program coverage
  , relocatable :: Bool
  -- ^ Whether to build a relocatable package
  }
  deriving (Eq, Generic, Read, Show)

instance Binary PackageBuildDescr
instance Structured PackageBuildDescr
instance Binary ComponentBuildDescr
instance Structured ComponentBuildDescr
instance Binary LocalBuildDescr
instance Structured LocalBuildDescr
instance Binary LocalBuildConfig
instance Structured LocalBuildConfig
instance Binary BuildOptions
instance Structured BuildOptions

buildOptionsConfigFlags :: BuildOptions -> ConfigFlags
buildOptionsConfigFlags (BuildOptions{..}) =
  mempty
    { configVanillaLib = toFlag $ withVanillaLib
    , configSharedLib = toFlag $ withSharedLib
    , configStaticLib = toFlag $ withStaticLib
    , configDynExe = toFlag $ withDynExe
    , configFullyStaticExe = toFlag $ withFullyStaticExe
    , configGHCiLib = toFlag $ withGHCiLib
    , configProfExe = toFlag $ withProfExe
    , configProfLib = toFlag $ withProfLib
    , configProf = mempty
    , -- configProfDetail is for exe+lib, but overridden by configProfLibDetail
      -- so we specify both so we can specify independently
      configProfDetail = toFlag $ withProfExeDetail
    , configProfLibDetail = toFlag $ withProfLibDetail
    , configCoverage = toFlag $ exeCoverage
    , configLibCoverage = mempty
    , configRelocatable = toFlag $ relocatable
    , configOptimization = toFlag $ withOptimization
    , configSplitSections = toFlag $ splitSections
    , configSplitObjs = toFlag $ splitObjs
    , configStripExes = toFlag $ stripExes
    , configStripLibs = toFlag $ stripLibs
    , configDebugInfo = toFlag $ withDebugInfo
    }
