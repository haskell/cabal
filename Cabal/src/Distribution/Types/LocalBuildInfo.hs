{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Types.LocalBuildInfo
  ( -- * The types
    LocalBuildInfo
      ( LocalBuildInfo
      , configFlags
      , flagAssignment
      , componentEnabledSpec
      , extraConfigArgs
      , installDirTemplates
      , compiler
      , hostPlatform
      , pkgDescrFile
      , componentGraph
      , componentNameMap
      , promisedPkgs
      , installedPkgs
      , localPkgDescr
      , withPrograms
      , withPackageDB
      , withVanillaLib
      , withProfLib
      , withProfLibShared
      , withDynExe
      , withFullyStaticExe
      , withProfExe
      , withSharedLib
      , withStaticLib
      , withBytecodeLib
      , withProfLibDetail
      , withProfExeDetail
      , withOptimization
      , withDebugInfo
      , withGHCiLib
      , splitSections
      , splitObjs
      , stripExes
      , stripLibs
      , exeCoverage
      , libCoverage
      , extraCoverageFor
      , relocatable
      , ..
      )

    -- * Convenience accessors
  , localComponentId
  , localUnitId
  , localCompatPackageKey
  , localPackage
  , buildDir
  , buildDirPBD
  , setupFlagsBuildDir
  , distPrefLBI
  , packageRoot
  , progPrefix
  , progSuffix

    -- * Build targets of the 'LocalBuildInfo'.
  , componentNameCLBIs
  -- NB: the primes mean that they take a 'PackageDescription'
  -- which may not match 'localPkgDescr' in 'LocalBuildInfo'.
  -- More logical types would drop this argument, but
  -- at the moment, this is the ONLY supported function, because
  -- 'localPkgDescr' is not guaranteed to match.  At some point
  -- we will fix it and then we can use the (free) unprimed
  -- namespace for the correct commands.
  --
  -- See https://github.com/haskell/cabal/issues/3606 for more
  -- details.

  , componentNameTargets'
  , unitIdTarget'
  , allTargetsInBuildOrder'
  , withAllTargetsInBuildOrder'
  , neededTargetsInBuildOrder'
  , withNeededTargetsInBuildOrder'
  , testCoverage
  , buildWays

    -- * Functions you SHOULD NOT USE (yet), but are defined here to

  -- prevent someone from accidentally defining them

  , componentNameTargets
  , unitIdTarget
  , allTargetsInBuildOrder
  , withAllTargetsInBuildOrder
  , neededTargetsInBuildOrder
  , withNeededTargetsInBuildOrder
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.ComponentId
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.GivenComponent
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.TargetInfo
import Distribution.Types.UnitId

import Distribution.Utils.Path

import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.BuildWay
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs hiding
  ( absoluteInstallDirs
  , prefixRelativeInstallDirs
  , substPathTemplate
  )
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Config
import Distribution.System

import qualified Data.Map as Map
import Distribution.Compat.Graph (Graph)
import qualified Distribution.Compat.Graph as Graph

import qualified System.FilePath as FilePath (takeDirectory)

-- | Data cached after configuration step.  See also
-- 'Distribution.Simple.Setup.ConfigFlags'.
data LocalBuildInfo = NewLocalBuildInfo
  { localBuildDescr :: LBC.LocalBuildDescr
  -- ^ Information about a package determined by Cabal
  -- after the configuration step.
  , localBuildConfig :: LBC.LocalBuildConfig
  -- ^ Information about a package configuration
  -- that can be modified by the user at configuration time.
  }
  deriving (Generic, Read, Show)

{-# COMPLETE LocalBuildInfo #-}

-- | This pattern synonym is for backwards compatibility, to adapt
-- to 'LocalBuildInfo' being split into 'LocalBuildDescr' and 'LocalBuildConfig'.
pattern LocalBuildInfo
  :: ConfigFlags
  -> FlagAssignment
  -> ComponentRequestedSpec
  -> [String]
  -> InstallDirTemplates
  -> Compiler
  -> Platform
  -> Maybe (SymbolicPath Pkg File)
  -> Graph ComponentLocalBuildInfo
  -> Map ComponentName [ComponentLocalBuildInfo]
  -> Map (PackageName, ComponentName) PromisedComponent
  -> InstalledPackageIndex
  -> PackageDescription
  -> ProgramDb
  -> PackageDBStack
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> ProfDetailLevel
  -> ProfDetailLevel
  -> OptimisationLevel
  -> DebugInfoLevel
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> [UnitId]
  -> Bool
  -> LocalBuildInfo
pattern LocalBuildInfo
  { configFlags
  , flagAssignment
  , componentEnabledSpec
  , extraConfigArgs
  , installDirTemplates
  , compiler
  , hostPlatform
  , pkgDescrFile
  , componentGraph
  , componentNameMap
  , promisedPkgs
  , installedPkgs
  , localPkgDescr
  , withPrograms
  , withPackageDB
  , withVanillaLib
  , withProfLib
  , withProfLibShared
  , withSharedLib
  , withStaticLib
  , withBytecodeLib
  , withDynExe
  , withFullyStaticExe
  , withProfExe
  , withProfLibDetail
  , withProfExeDetail
  , withOptimization
  , withDebugInfo
  , withGHCiLib
  , splitSections
  , splitObjs
  , stripExes
  , stripLibs
  , exeCoverage
  , libCoverage
  , extraCoverageFor
  , relocatable
  } =
  NewLocalBuildInfo
    { localBuildDescr =
      LBC.LocalBuildDescr
        { packageBuildDescr =
          LBC.PackageBuildDescr
            { configFlags
            , flagAssignment
            , componentEnabledSpec
            , compiler
            , hostPlatform
            , localPkgDescr
            , installDirTemplates
            , withPackageDB
            , pkgDescrFile
            , extraCoverageFor
            }
        , componentBuildDescr =
          LBC.ComponentBuildDescr
            { componentGraph
            , componentNameMap
            , promisedPkgs
            , installedPkgs
            }
        }
    , localBuildConfig =
      LBC.LocalBuildConfig
        { extraConfigArgs
        , withPrograms
        , withBuildOptions =
          LBC.BuildOptions
            { withVanillaLib
            , withProfLib
            , withProfLibShared
            , withSharedLib
            , withStaticLib
            , withBytecodeLib
            , withDynExe
            , withFullyStaticExe
            , withProfExe
            , withProfLibDetail
            , withProfExeDetail
            , withOptimization
            , withDebugInfo
            , withGHCiLib
            , splitSections
            , splitObjs
            , stripExes
            , stripLibs
            , exeCoverage
            , libCoverage
            , relocatable
            }
        }
    }

instance Binary LocalBuildInfo
instance Structured LocalBuildInfo

-------------------------------------------------------------------------------
-- Accessor functions

buildDir :: LocalBuildInfo -> SymbolicPath Pkg (Dir Build)
buildDir lbi =
  buildDirPBD $ LBC.packageBuildDescr $ localBuildDescr lbi

buildDirPBD :: LBC.PackageBuildDescr -> SymbolicPath Pkg (Dir Build)
buildDirPBD (LBC.PackageBuildDescr{configFlags = cfg}) =
  setupFlagsBuildDir $ configCommonFlags cfg

setupFlagsBuildDir :: CommonSetupFlags -> SymbolicPath Pkg (Dir Build)
setupFlagsBuildDir cfg = fromFlag (setupDistPref cfg) </> makeRelativePathEx "build"

distPrefLBI :: LocalBuildInfo -> SymbolicPath Pkg (Dir Dist)
distPrefLBI = fromFlag . setupDistPref . configCommonFlags . LBC.configFlags . LBC.packageBuildDescr . localBuildDescr

-- | The (relative or absolute) path to the package root, based on
--
--  - the working directory flag
--  - the @.cabal@ path
packageRoot :: CommonSetupFlags -> FilePath
packageRoot cfg =
  case flagToMaybe (setupCabalFilePath cfg) of
    Just cabalPath -> FilePath.takeDirectory $ interpretSymbolicPath mbWorkDir cabalPath
    Nothing -> maybe "." getSymbolicPath mbWorkDir
  where
    mbWorkDir = flagToMaybe $ setupWorkingDir cfg

progPrefix, progSuffix :: LocalBuildInfo -> PathTemplate
progPrefix (LocalBuildInfo{configFlags = cfg}) =
  fromFlag $ configProgPrefix cfg
progSuffix (LocalBuildInfo{configFlags = cfg}) =
  fromFlag $ configProgSuffix cfg

-- TODO: Get rid of these functions, as much as possible.  They are
-- a bit useful in some cases, but you should be very careful!

-- | Extract the 'ComponentId' from the public library component of a
-- 'LocalBuildInfo' if it exists, or make a fake component ID based
-- on the package ID.
localComponentId :: LocalBuildInfo -> ComponentId
localComponentId lbi =
  case componentNameCLBIs lbi (CLibName LMainLibName) of
    [LibComponentLocalBuildInfo{componentComponentId = cid}] ->
      cid
    _ -> mkComponentId (prettyShow (localPackage lbi))

-- | Extract the 'PackageIdentifier' of a 'LocalBuildInfo'.
-- This is a "safe" use of 'localPkgDescr'
localPackage :: LocalBuildInfo -> PackageId
localPackage (LocalBuildInfo{localPkgDescr = pkg}) = package pkg

-- | Extract the 'UnitId' from the library component of a
-- 'LocalBuildInfo' if it exists, or make a fake unit ID based on
-- the package ID.
localUnitId :: LocalBuildInfo -> UnitId
localUnitId lbi =
  case componentNameCLBIs lbi (CLibName LMainLibName) of
    [LibComponentLocalBuildInfo{componentUnitId = uid}] ->
      uid
    _ -> mkLegacyUnitId $ localPackage lbi

-- | Extract the compatibility package key from the public library component of a
-- 'LocalBuildInfo' if it exists, or make a fake package key based
-- on the package ID.
localCompatPackageKey :: LocalBuildInfo -> String
localCompatPackageKey lbi =
  case componentNameCLBIs lbi (CLibName LMainLibName) of
    [LibComponentLocalBuildInfo{componentCompatPackageKey = pk}] ->
      pk
    _ -> prettyShow (localPackage lbi)

-- | Convenience function to generate a default 'TargetInfo' from a
-- 'ComponentLocalBuildInfo'.  The idea is to call this once, and then
-- use 'TargetInfo' everywhere else.  Private to this module.
mkTargetInfo :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> TargetInfo
mkTargetInfo pkg_descr _lbi clbi =
  TargetInfo
    { targetCLBI = clbi
    , -- NB: @pkg_descr@, not @localPkgDescr lbi@!
      targetComponent =
        getComponent
          pkg_descr
          (componentLocalName clbi)
    }

-- | Return all 'TargetInfo's associated with 'ComponentName'.
-- In the presence of Backpack there may be more than one!
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
componentNameTargets' :: PackageDescription -> LocalBuildInfo -> ComponentName -> [TargetInfo]
componentNameTargets' pkg_descr lbi@(LocalBuildInfo{componentNameMap = comps}) cname =
  case Map.lookup cname comps of
    Just clbis -> map (mkTargetInfo pkg_descr lbi) clbis
    Nothing -> []

unitIdTarget' :: PackageDescription -> LocalBuildInfo -> UnitId -> Maybe TargetInfo
unitIdTarget' pkg_descr lbi@(LocalBuildInfo{componentGraph = compsGraph}) uid =
  case Graph.lookup uid compsGraph of
    Just clbi -> Just (mkTargetInfo pkg_descr lbi clbi)
    Nothing -> Nothing

-- | Return all 'ComponentLocalBuildInfo's associated with 'ComponentName'.
-- In the presence of Backpack there may be more than one!
componentNameCLBIs :: LocalBuildInfo -> ComponentName -> [ComponentLocalBuildInfo]
componentNameCLBIs (LocalBuildInfo{componentNameMap = comps}) cname =
  case Map.lookup cname comps of
    Just clbis -> clbis
    Nothing -> []

-- TODO: Maybe cache topsort (Graph can do this)

-- | Return the list of default 'TargetInfo's associated with a
-- configured package, in the order they need to be built.
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
allTargetsInBuildOrder' :: PackageDescription -> LocalBuildInfo -> [TargetInfo]
allTargetsInBuildOrder' pkg_descr lbi@(LocalBuildInfo{componentGraph = compsGraph}) =
  map (mkTargetInfo pkg_descr lbi) (Graph.revTopSort compsGraph)

-- | Execute @f@ for every 'TargetInfo' in the package, respecting the
-- build dependency order.  (TODO: We should use Shake!)
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
withAllTargetsInBuildOrder' :: PackageDescription -> LocalBuildInfo -> (TargetInfo -> IO ()) -> IO ()
withAllTargetsInBuildOrder' pkg_descr lbi f =
  sequence_ [f target | target <- allTargetsInBuildOrder' pkg_descr lbi]

-- | Return the list of all targets needed to build the @uids@, in
-- the order they need to be built.
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
neededTargetsInBuildOrder' :: PackageDescription -> LocalBuildInfo -> [UnitId] -> [TargetInfo]
neededTargetsInBuildOrder' pkg_descr lbi@(LocalBuildInfo{componentGraph = compsGraph}) uids =
  case Graph.closure compsGraph uids of
    Nothing -> error $ "localBuildPlan: missing uids " ++ intercalate ", " (map prettyShow uids)
    Just clos -> map (mkTargetInfo pkg_descr lbi) (Graph.revTopSort (Graph.fromDistinctList clos))

-- | Execute @f@ for every 'TargetInfo' needed to build @uid@s, respecting
-- the build dependency order.
-- Has a prime because it takes a 'PackageDescription' argument
-- which may disagree with 'localPkgDescr' in 'LocalBuildInfo'.
withNeededTargetsInBuildOrder' :: PackageDescription -> LocalBuildInfo -> [UnitId] -> (TargetInfo -> IO ()) -> IO ()
withNeededTargetsInBuildOrder' pkg_descr lbi uids f =
  sequence_ [f target | target <- neededTargetsInBuildOrder' pkg_descr lbi uids]

-- | Is coverage enabled for test suites? In practice, this requires library
-- and executable profiling to be enabled.
testCoverage :: LocalBuildInfo -> Bool
testCoverage (LocalBuildInfo{exeCoverage = exes, libCoverage = libs}) =
  exes && libs

-- | Returns a list of ways, in the order which they should be built, and the
-- way we build executable and foreign library components.
--
-- Ideally all this info should be fixed at configure time and not dependent on
-- additional info but `LocalBuildInfo` is per package (not per component) so it's
-- currently not possible to configure components to be built in certain ways.
buildWays :: LocalBuildInfo -> (Bool -> [BuildWay], Bool -> BuildWay, BuildWay)
buildWays lbi =
  let
    -- enable-library-profiling (enable (static profiling way)) .p_o
    -- enable-shared (enabled dynamic way)  .dyn_o
    -- enable-profiling-shared (enable dynamic profilng way) .p_dyn_o
    -- enable-library-vanilla (enable vanilla way) .o
    --
    -- enable-executable-dynamic => build dynamic executables
    -- => --enable-profiling + --enable-executable-dynamic => build dynamic profiled executables
    -- => --enable-profiling => build vanilla profiled executables

    wantedLibWays is_indef =
      [ProfDynWay | withProfLibShared lbi && not is_indef]
        <> [ProfWay | withProfLib lbi]
        -- I don't see why we shouldn't build with dynamic-- indefinite components.
        <> [DynWay | withSharedLib lbi && not is_indef]
        -- MP: Ideally we should have `BuildOptions` on a per component basis, in
        -- which case this `is_indef` check could be moved to configure time.
        <> [StaticWay | withVanillaLib lbi || withStaticLib lbi]

    wantedFLibWay is_dyn_flib =
      case (is_dyn_flib, withProfExe lbi) of
        (True, True) -> ProfDynWay
        (False, True) -> ProfWay
        (True, False) -> DynWay
        (False, False) -> StaticWay

    wantedExeWay =
      case (withDynExe lbi, withProfExe lbi) of
        (True, True) -> ProfDynWay
        (True, False) -> DynWay
        (False, True) -> ProfWay
        (False, False) -> StaticWay
   in
    (wantedLibWays, wantedFLibWay, wantedExeWay)

-------------------------------------------------------------------------------
-- Stub functions to prevent someone from accidentally defining them

{-# WARNING componentNameTargets, unitIdTarget, allTargetsInBuildOrder, withAllTargetsInBuildOrder, neededTargetsInBuildOrder, withNeededTargetsInBuildOrder "By using this function, you may be introducing a bug where you retrieve a 'Component' which does not have 'HookedBuildInfo' applied to it.  See the documentation for 'HookedBuildInfo' for an explanation of the issue.  If you have a 'PackageDescription' handy (NOT from the 'LocalBuildInfo'), try using the primed version of the function, which takes it as an extra argument." #-}
componentNameTargets :: LocalBuildInfo -> ComponentName -> [TargetInfo]
componentNameTargets lbi@(LocalBuildInfo{localPkgDescr = pkg}) =
  componentNameTargets' pkg lbi
unitIdTarget :: LocalBuildInfo -> UnitId -> Maybe TargetInfo
unitIdTarget lbi@(LocalBuildInfo{localPkgDescr = pkg}) =
  unitIdTarget' pkg lbi
allTargetsInBuildOrder :: LocalBuildInfo -> [TargetInfo]
allTargetsInBuildOrder lbi@(LocalBuildInfo{localPkgDescr = pkg}) =
  allTargetsInBuildOrder' pkg lbi
withAllTargetsInBuildOrder :: LocalBuildInfo -> (TargetInfo -> IO ()) -> IO ()
withAllTargetsInBuildOrder lbi@(LocalBuildInfo{localPkgDescr = pkg}) =
  withAllTargetsInBuildOrder' pkg lbi
neededTargetsInBuildOrder :: LocalBuildInfo -> [UnitId] -> [TargetInfo]
neededTargetsInBuildOrder lbi@(LocalBuildInfo{localPkgDescr = pkg}) =
  neededTargetsInBuildOrder' pkg lbi
withNeededTargetsInBuildOrder :: LocalBuildInfo -> [UnitId] -> (TargetInfo -> IO ()) -> IO ()
withNeededTargetsInBuildOrder lbi@(LocalBuildInfo{localPkgDescr = pkg}) =
  withNeededTargetsInBuildOrder' pkg lbi
