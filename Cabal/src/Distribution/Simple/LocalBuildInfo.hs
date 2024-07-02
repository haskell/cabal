{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.LocalBuildInfo
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Once a package has been configured we have resolved conditionals and
-- dependencies, configured the compiler and other needed external programs.
-- The 'LocalBuildInfo' is used to hold all this information. It holds the
-- install dirs, the compiler, the exact package dependencies, the configured
-- programs, the package database to use and a bunch of miscellaneous configure
-- flags. It gets saved and reloaded from a file (@dist\/setup-config@). It gets
-- passed in to very many subsequent build actions.
module Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (..)
  , localComponentId
  , localUnitId
  , localCompatPackageKey

    -- * Convenience accessors
  , buildDir
  , packageRoot
  , progPrefix
  , progSuffix
  , interpretSymbolicPathLBI
  , mbWorkDirLBI
  , absoluteWorkingDirLBI
  , buildWays

    -- * Buildable package components
  , Component (..)
  , ComponentName (..)
  , LibraryName (..)
  , defaultLibName
  , showComponentName
  , componentNameString
  , ComponentLocalBuildInfo (..)
  , componentBuildDir
  , foldComponent
  , componentName
  , componentBuildInfo
  , componentBuildable
  , pkgComponents
  , pkgBuildableComponents
  , lookupComponent
  , getComponent
  , allComponentsInBuildOrder
  , depLibraryPaths
  , allLibModules
  , withAllComponentsInBuildOrder
  , withLibLBI
  , withExeLBI
  , withBenchLBI
  , withTestLBI
  , enabledTestLBIs
  , enabledBenchLBIs

    -- * Installation directories
  , module Distribution.Simple.InstallDirs
  , absoluteInstallDirs
  , prefixRelativeInstallDirs
  , absoluteInstallCommandDirs
  , absoluteComponentInstallDirs
  , prefixRelativeComponentInstallDirs
  , substPathTemplate
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Component
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentName
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.TargetInfo
import Distribution.Types.UnitId
import Distribution.Types.UnqualComponentName

import qualified Distribution.Compat.Graph as Graph
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs hiding
  ( absoluteInstallDirs
  , prefixRelativeInstallDirs
  , substPathTemplate
  )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Config
import Distribution.Simple.Utils
import Distribution.Utils.Path

import Data.List (stripPrefix)
import qualified System.Directory as Directory
  ( canonicalizePath
  , doesDirectoryExist
  )

-- -----------------------------------------------------------------------------
-- Configuration information of buildable components

componentBuildDir :: LocalBuildInfo -> ComponentLocalBuildInfo -> SymbolicPath Pkg (Dir Build)
-- For now, we assume that libraries/executables/test-suites/benchmarks
-- are only ever built once.  With Backpack, we need a special case for
-- libraries so that we can handle building them multiple times.
componentBuildDir lbi clbi =
  (buildDir lbi </>) $
    makeRelativePathEx $
      case componentLocalName clbi of
        CLibName LMainLibName ->
          if prettyShow (componentUnitId clbi) == prettyShow (componentComponentId clbi)
            then ""
            else prettyShow (componentUnitId clbi)
        CLibName (LSubLibName s) ->
          if prettyShow (componentUnitId clbi) == prettyShow (componentComponentId clbi)
            then unUnqualComponentName s
            else prettyShow (componentUnitId clbi)
        CFLibName s -> unUnqualComponentName s
        CExeName s -> unUnqualComponentName s
        CTestName s -> unUnqualComponentName s
        CBenchName s -> unUnqualComponentName s

-- | Interpret a symbolic path with respect to the working directory
-- stored in 'LocalBuildInfo'.
--
-- Use this before directly interacting with the file system.
--
-- NB: when invoking external programs (such as @GHC@), it is preferable to set
-- the working directory of the process rather than calling this function, as
-- this function will turn relative paths into absolute paths if the working
-- directory is an absolute path. This can degrade error messages, or worse,
-- break the behaviour entirely (because the program might expect certain paths
-- to be relative).
--
-- See Note [Symbolic paths] in Distribution.Utils.Path
interpretSymbolicPathLBI :: LocalBuildInfo -> SymbolicPathX allowAbsolute Pkg to -> FilePath
interpretSymbolicPathLBI lbi =
  interpretSymbolicPath (mbWorkDirLBI lbi)

-- | Retrieve an optional working directory from 'LocalBuildInfo'.
mbWorkDirLBI :: LocalBuildInfo -> Maybe (SymbolicPath CWD (Dir Pkg))
mbWorkDirLBI =
  flagToMaybe . setupWorkingDir . configCommonFlags . configFlags

-- | Absolute path to the current working directory.
absoluteWorkingDirLBI :: LocalBuildInfo -> IO FilePath
absoluteWorkingDirLBI lbi = absoluteWorkingDir (mbWorkDirLBI lbi)

-- | Perform the action on each enabled 'library' in the package
-- description with the 'ComponentLocalBuildInfo'.
withLibLBI
  :: PackageDescription
  -> LocalBuildInfo
  -> (Library -> ComponentLocalBuildInfo -> IO ())
  -> IO ()
withLibLBI pkg lbi f =
  withAllTargetsInBuildOrder' pkg lbi $ \target ->
    case targetComponent target of
      CLib lib -> f lib (targetCLBI target)
      _ -> return ()

-- | Perform the action on each enabled 'Executable' in the package
-- description.  Extended version of 'withExe' that also gives corresponding
-- build info.
withExeLBI
  :: PackageDescription
  -> LocalBuildInfo
  -> (Executable -> ComponentLocalBuildInfo -> IO ())
  -> IO ()
withExeLBI pkg lbi f =
  withAllTargetsInBuildOrder' pkg lbi $ \target ->
    case targetComponent target of
      CExe exe -> f exe (targetCLBI target)
      _ -> return ()

-- | Perform the action on each enabled 'Benchmark' in the package
-- description.
withBenchLBI
  :: PackageDescription
  -> LocalBuildInfo
  -> (Benchmark -> ComponentLocalBuildInfo -> IO ())
  -> IO ()
withBenchLBI pkg lbi f =
  sequence_ [f bench clbi | (bench, clbi) <- enabledBenchLBIs pkg lbi]

withTestLBI
  :: PackageDescription
  -> LocalBuildInfo
  -> (TestSuite -> ComponentLocalBuildInfo -> IO ())
  -> IO ()
withTestLBI pkg lbi f =
  sequence_ [f test clbi | (test, clbi) <- enabledTestLBIs pkg lbi]

enabledTestLBIs
  :: PackageDescription
  -> LocalBuildInfo
  -> [(TestSuite, ComponentLocalBuildInfo)]
enabledTestLBIs pkg lbi =
  [ (test, targetCLBI target)
  | target <- allTargetsInBuildOrder' pkg lbi
  , CTest test <- [targetComponent target]
  ]

enabledBenchLBIs
  :: PackageDescription
  -> LocalBuildInfo
  -> [(Benchmark, ComponentLocalBuildInfo)]
enabledBenchLBIs pkg lbi =
  [ (bench, targetCLBI target)
  | target <- allTargetsInBuildOrder' pkg lbi
  , CBench bench <- [targetComponent target]
  ]

-- | Perform the action on each buildable 'Library' or 'Executable' (Component)
-- in the PackageDescription, subject to the build order specified by the
-- 'compBuildOrder' field of the given 'LocalBuildInfo'
withAllComponentsInBuildOrder
  :: PackageDescription
  -> LocalBuildInfo
  -> (Component -> ComponentLocalBuildInfo -> IO ())
  -> IO ()
withAllComponentsInBuildOrder pkg lbi f =
  withAllTargetsInBuildOrder' pkg lbi $ \target ->
    f (targetComponent target) (targetCLBI target)

allComponentsInBuildOrder
  :: LocalBuildInfo
  -> [ComponentLocalBuildInfo]
allComponentsInBuildOrder (LocalBuildInfo{componentGraph = compGraph}) =
  Graph.topSort compGraph

-- -----------------------------------------------------------------------------
-- A random function that has no business in this module

-- | Determine the directories containing the dynamic libraries of the
-- transitive dependencies of the component we are building.
--
-- When wanted, and possible, returns paths relative to the installDirs 'prefix'
depLibraryPaths
  :: Bool
  -- ^ Building for inplace?
  -> Bool
  -- ^ Generate prefix-relative library paths
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -- ^ Component that is being built
  -> IO [FilePath]
depLibraryPaths
  inplace
  relative
  lbi@( LocalBuildInfo
          { localPkgDescr = pkgDescr
          , installedPkgs = installed
          }
        )
  clbi = do
    let installDirs = absoluteComponentInstallDirs pkgDescr lbi (componentUnitId clbi) NoCopyDest
        executable = case clbi of
          ExeComponentLocalBuildInfo{} -> True
          _ -> False
        relDir
          | executable = bindir installDirs
          | otherwise = libdir installDirs

    let
      -- TODO: this is kind of inefficient
      internalDeps =
        [ uid
        | (uid, _) <- componentPackageDeps clbi
        , -- Test that it's internal
        sub_target <- allTargetsInBuildOrder' pkgDescr lbi
        , componentUnitId (targetCLBI (sub_target)) == uid
        ]
      internalLibs =
        [ getLibDir (targetCLBI sub_target)
        | sub_target <-
            neededTargetsInBuildOrder'
              pkgDescr
              lbi
              internalDeps
        ]
      {-
      -- This is better, but it doesn't work, because we may be passed a
      -- CLBI which doesn't actually exist, and was faked up when we
      -- were building a test suite/benchmark.  See #3599 for proposal
      -- to fix this.
      let internalCLBIs = filter ((/= componentUnitId clbi) . componentUnitId)
                        . map targetCLBI
                        $ neededTargetsInBuildOrder lbi [componentUnitId clbi]
          internalLibs = map getLibDir internalCLBIs
      -}
      getLibDir sub_clbi
        | inplace = interpretSymbolicPathLBI lbi $ componentBuildDir lbi sub_clbi
        | otherwise = dynlibdir (absoluteComponentInstallDirs pkgDescr lbi (componentUnitId sub_clbi) NoCopyDest)

    -- Why do we go through all the trouble of a hand-crafting
    -- internalLibs, when 'installedPkgs' actually contains the
    -- internal libraries?  The trouble is that 'installedPkgs'
    -- may contain *inplace* entries, which we must NOT use for
    -- not inplace 'depLibraryPaths' (e.g., for RPATH calculation).
    -- See #4025 for more details. This is all horrible but it
    -- is a moot point if you are using a per-component build,
    -- because you never have any internal libraries in this case;
    -- they're all external.
    let external_ipkgs = filter is_external (allPackages installed)
        is_external ipkg = not (installedUnitId ipkg `elem` internalDeps)
        -- First look for dynamic libraries in `dynamic-library-dirs`, and use
        -- `library-dirs` as a fall back.
        getDynDir pkg = case Installed.libraryDynDirs pkg of
          [] -> Installed.libraryDirs pkg
          d -> d
        allDepLibDirs = concatMap getDynDir external_ipkgs

        allDepLibDirs' = internalLibs ++ allDepLibDirs
    allDepLibDirsC <- traverse canonicalizePathNoFail allDepLibDirs'

    let p = prefix installDirs
        prefixRelative l = isJust (stripPrefix p l)
        libPaths
          | relative
              && prefixRelative relDir =
              map
                ( \l ->
                    if prefixRelative l
                      then shortRelativePath relDir l
                      else l
                )
                allDepLibDirsC
          | otherwise = allDepLibDirsC

    -- For some reason, this function returns lots of duplicates. Avoid
    -- exceeding `ARG_MAX` (the result of this function is used to populate
    -- `LD_LIBRARY_PATH`) by deduplicating the list.
    return $ ordNub libPaths
    where
      -- 'canonicalizePath' fails on UNIX when the directory does not exists.
      -- So just don't canonicalize when it doesn't exist.
      canonicalizePathNoFail p = do
        exists <- Directory.doesDirectoryExist p
        if exists
          then Directory.canonicalizePath p
          else return p

-- | Get all module names that needed to be built by GHC; i.e., all
-- of these 'ModuleName's have interface files associated with them
-- that need to be installed.
allLibModules :: Library -> ComponentLocalBuildInfo -> [ModuleName]
allLibModules lib clbi =
  ordNub $
    explicitLibModules lib
      ++ case clbi of
        LibComponentLocalBuildInfo{componentInstantiatedWith = insts} -> map fst insts
        _ -> []

-- -----------------------------------------------------------------------------
-- Wrappers for a couple functions from InstallDirs

-- | Backwards compatibility function which computes the InstallDirs
-- assuming that @$libname@ points to the public library (or some fake
-- package identifier if there is no public library.)  IF AT ALL
-- POSSIBLE, please use 'absoluteComponentInstallDirs' instead.
absoluteInstallDirs
  :: PackageDescription
  -> LocalBuildInfo
  -> CopyDest
  -> InstallDirs FilePath
absoluteInstallDirs pkg lbi copydest =
  absoluteComponentInstallDirs pkg lbi (localUnitId lbi) copydest

-- | See 'InstallDirs.absoluteInstallDirs'.
absoluteComponentInstallDirs
  :: PackageDescription
  -> LocalBuildInfo
  -> UnitId
  -> CopyDest
  -> InstallDirs FilePath
absoluteComponentInstallDirs
  pkg
  (LocalBuildInfo{compiler = comp, hostPlatform = plat, installDirTemplates = installDirs})
  uid
  copydest =
    InstallDirs.absoluteInstallDirs
      (packageId pkg)
      uid
      (compilerInfo comp)
      copydest
      plat
      installDirs

absoluteInstallCommandDirs
  :: PackageDescription
  -> LocalBuildInfo
  -> UnitId
  -> CopyDest
  -> InstallDirs FilePath
absoluteInstallCommandDirs pkg lbi uid copydest =
  dirs
    { -- Handle files which are not
      -- per-component (data files and Haddock files.)
      datadir = datadir dirs'
    , -- NB: The situation with Haddock is a bit delicate.  On the
      -- one hand, the easiest to understand Haddock documentation
      -- path is pkgname-0.1, which means it's per-package (not
      -- per-component).  But this means that it's impossible to
      -- install Haddock documentation for internal libraries.  We'll
      -- keep this constraint for now; this means you can't use
      -- Cabal to Haddock internal libraries.  This does not seem
      -- like a big problem.
      docdir = docdir dirs'
    , htmldir = htmldir dirs'
    , haddockdir = haddockdir dirs'
    }
  where
    dirs = absoluteComponentInstallDirs pkg lbi uid copydest
    -- Notice use of 'absoluteInstallDirs' (not the
    -- per-component variant).  This means for non-library
    -- packages we'll just pick a nondescriptive foo-0.1
    dirs' = absoluteInstallDirs pkg lbi copydest

-- | Backwards compatibility function which computes the InstallDirs
-- assuming that @$libname@ points to the public library (or some fake
-- package identifier if there is no public library.)  IF AT ALL
-- POSSIBLE, please use 'prefixRelativeComponentInstallDirs' instead.
prefixRelativeInstallDirs
  :: PackageId
  -> LocalBuildInfo
  -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkg_descr lbi =
  prefixRelativeComponentInstallDirs pkg_descr lbi (localUnitId lbi)

-- | See 'InstallDirs.prefixRelativeInstallDirs'
prefixRelativeComponentInstallDirs
  :: PackageId
  -> LocalBuildInfo
  -> UnitId
  -> InstallDirs (Maybe FilePath)
prefixRelativeComponentInstallDirs
  pkg_descr
  (LocalBuildInfo{compiler = comp, hostPlatform = plat, installDirTemplates = installDirs})
  uid =
    InstallDirs.prefixRelativeInstallDirs
      (packageId pkg_descr)
      uid
      (compilerInfo comp)
      plat
      installDirs

substPathTemplate
  :: PackageId
  -> LocalBuildInfo
  -> UnitId
  -> PathTemplate
  -> FilePath
substPathTemplate
  pkgid
  (LocalBuildInfo{compiler = comp, hostPlatform = plat})
  uid =
    fromPathTemplate
      . (InstallDirs.substPathTemplate env)
    where
      env =
        initialPathTemplateEnv
          pkgid
          uid
          (compilerInfo comp)
          plat
