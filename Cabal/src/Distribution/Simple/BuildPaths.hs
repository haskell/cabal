{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.BuildPaths
-- Copyright   :  Isaac Jones 2003-2004,
--                Duncan Coutts 2008
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A bunch of dirs, paths and file names used for intermediate build steps.
module Distribution.Simple.BuildPaths
  ( defaultDistPref
  , srcPref
  , buildInfoPref
  , haddockDirName
  , hscolourPref
  , haddockPref
  , autogenPackageModulesDir
  , autogenComponentModulesDir
  , autogenPathsModuleName
  , autogenPackageInfoModuleName
  , cppHeaderName
  , haddockName
  , mkGenericStaticLibName
  , mkLibName
  , mkProfLibName
  , mkGenericSharedLibName
  , mkSharedLibName
  , mkStaticLibName
  , mkGenericSharedBundledLibName
  , exeExtension
  , objExtension
  , dllExtension
  , staticLibExtension

    -- * Source files & build directories
  , getSourceFiles
  , getLibSourceFiles
  , getExeSourceFiles
  , getFLibSourceFiles
  , exeBuildDir
  , flibBuildDir
  , stubName
  , testBuildDir
  , benchmarkBuildDir
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.List (stripPrefix)
import Distribution.Compiler
import Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Errors
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess.Types (builtinHaskellSuffixes)
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Haddock (HaddockTarget (..))
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Utils.Path
import Distribution.Verbosity

-- ---------------------------------------------------------------------------
-- Build directories and files

srcPref :: FilePath -> FilePath
srcPref distPref = distPref </> "src"

hscolourPref
  :: HaddockTarget
  -> SymbolicPath root (Dir Dist)
  -> PackageDescription
  -> SymbolicPath root (Dir Artifacts)
hscolourPref = haddockPref

-- | Build info json file, generated in every build
buildInfoPref
  :: SymbolicPath root (Dir Dist)
  -> SymbolicPath root File
buildInfoPref distPref = distPref </> makeRelativePathEx "build-info.json"

-- | This is the name of the directory in which the generated haddocks
-- should be stored. It does not include the @<dist>/doc/html@ prefix.
haddockDirName :: HaddockTarget -> PackageDescription -> FilePath
haddockDirName ForDevelopment = prettyShow . packageName
haddockDirName ForHackage = (++ "-docs") . prettyShow . packageId

-- | The directory to which generated haddock documentation should be written.
haddockPref
  :: HaddockTarget
  -> SymbolicPath root (Dir Dist)
  -> PackageDescription
  -> SymbolicPath root (Dir Artifacts)
haddockPref haddockTarget distPref pkg_descr =
  distPref </> makeRelativePathEx ("doc" </> "html" </> haddockDirName haddockTarget pkg_descr)

-- | The directory in which we put auto-generated modules for EVERY
-- component in the package.
autogenPackageModulesDir :: LocalBuildInfo -> SymbolicPath Pkg (Dir Source)
autogenPackageModulesDir lbi = buildDir lbi </> makeRelativePathEx "global-autogen"

-- | The directory in which we put auto-generated modules for a
-- particular component.
autogenComponentModulesDir :: LocalBuildInfo -> ComponentLocalBuildInfo -> SymbolicPath Pkg (Dir Source)
autogenComponentModulesDir lbi clbi = componentBuildDir lbi clbi </> makeRelativePathEx "autogen"

-- NB: Look at 'checkForeignDeps' for where a simplified version of this
-- has been copy-pasted.

cppHeaderName :: String
cppHeaderName = "cabal_macros.h"

-- | The name of the auto-generated Paths_* module associated with a package
autogenPathsModuleName :: PackageDescription -> ModuleName
autogenPathsModuleName pkg_descr =
  ModuleName.fromString $
    "Paths_" ++ map fixchar (prettyShow (packageName pkg_descr))
  where
    fixchar '-' = '_'
    fixchar c = c

-- | The name of the auto-generated PackageInfo_* module associated with a package
autogenPackageInfoModuleName :: PackageDescription -> ModuleName
autogenPackageInfoModuleName pkg_descr =
  ModuleName.fromString $
    "PackageInfo_" ++ map fixchar (prettyShow (packageName pkg_descr))
  where
    fixchar '-' = '_'
    fixchar c = c

haddockName :: PackageDescription -> FilePath
haddockName pkg_descr = prettyShow (packageName pkg_descr) <.> "haddock"

-- -----------------------------------------------------------------------------
-- Source File helper

getLibSourceFiles
  :: Verbosity
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO [(ModuleName.ModuleName, SymbolicPath Pkg File)]
getLibSourceFiles verbosity lbi lib clbi =
  getSourceFiles verbosity mbWorkDir searchpaths modules
  where
    bi = libBuildInfo lib
    modules = allLibModules lib clbi
    mbWorkDir = mbWorkDirLBI lbi
    searchpaths =
      coerceSymbolicPath (componentBuildDir lbi clbi)
        : hsSourceDirs bi
        ++ [ autogenComponentModulesDir lbi clbi
           , autogenPackageModulesDir lbi
           ]

getExeSourceFiles
  :: Verbosity
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO [(ModuleName.ModuleName, SymbolicPath Pkg 'File)]
getExeSourceFiles verbosity lbi exe clbi = do
  moduleFiles <- getSourceFiles verbosity mbWorkDir searchpaths modules
  srcMainPath <- findFileCwd verbosity mbWorkDir (hsSourceDirs bi) (modulePath exe)
  return ((ModuleName.main, srcMainPath) : moduleFiles)
  where
    mbWorkDir = mbWorkDirLBI lbi
    bi = buildInfo exe
    modules = otherModules bi
    searchpaths =
      autogenComponentModulesDir lbi clbi
        : autogenPackageModulesDir lbi
        : coerceSymbolicPath (exeBuildDir lbi exe)
        : hsSourceDirs bi

getFLibSourceFiles
  :: Verbosity
  -> LocalBuildInfo
  -> ForeignLib
  -> ComponentLocalBuildInfo
  -> IO [(ModuleName.ModuleName, SymbolicPath Pkg File)]
getFLibSourceFiles verbosity lbi flib clbi =
  getSourceFiles verbosity mbWorkDir searchpaths modules
  where
    bi = foreignLibBuildInfo flib
    modules = otherModules bi
    mbWorkDir = mbWorkDirLBI lbi
    searchpaths =
      autogenComponentModulesDir lbi clbi
        : autogenPackageModulesDir lbi
        : coerceSymbolicPath (flibBuildDir lbi flib)
        : hsSourceDirs bi

getSourceFiles
  :: Verbosity
  -> Maybe (SymbolicPath CWD ('Dir Pkg))
  -> [SymbolicPathX allowAbsolute Pkg (Dir Source)]
  -> [ModuleName.ModuleName]
  -> IO [(ModuleName.ModuleName, SymbolicPathX allowAbsolute Pkg File)]
getSourceFiles verbosity mbWorkDir dirs modules = flip traverse modules $ \m ->
  fmap ((,) m) $
    findFileCwdWithExtension
      mbWorkDir
      builtinHaskellSuffixes
      dirs
      (moduleNameSymbolicPath m)
      >>= maybe (notFound m) (return . normaliseSymbolicPath)
  where
    notFound module_ =
      dieWithException verbosity $ CantFindSourceModule module_

-- | The directory where we put build results for an executable
exeBuildDir :: LocalBuildInfo -> Executable -> SymbolicPath Pkg (Dir Build)
exeBuildDir lbi exe = buildDir lbi </> makeRelativePathEx (nm </> nm ++ "-tmp")
  where
    nm = unUnqualComponentName $ exeName exe

-- | The directory where we put build results for a foreign library
flibBuildDir :: LocalBuildInfo -> ForeignLib -> SymbolicPath Pkg (Dir Build)
flibBuildDir lbi flib = buildDir lbi </> makeRelativePathEx (nm </> nm ++ "-tmp")
  where
    nm = unUnqualComponentName $ foreignLibName flib

-- | The name of the stub executable associated with a library 'TestSuite'.
stubName :: TestSuite -> FilePath
stubName t = unUnqualComponentName (testName t) ++ "Stub"

-- | The directory where we put build results for a test suite
testBuildDir :: LocalBuildInfo -> TestSuite -> SymbolicPath Pkg (Dir Build)
testBuildDir lbi tst =
  buildDir lbi </> makeRelativePathEx testDir
  where
    testDir = case testInterface tst of
      TestSuiteLibV09{} ->
        stubName tst </> stubName tst ++ "-tmp"
      _ -> nm </> nm ++ "-tmp"
    nm = unUnqualComponentName $ testName tst

-- | The directory where we put build results for a benchmark suite
benchmarkBuildDir :: LocalBuildInfo -> Benchmark -> SymbolicPath Pkg (Dir Build)
benchmarkBuildDir lbi bm =
  buildDir lbi </> makeRelativePathEx (nm </> nm ++ "-tmp")
  where
    nm = unUnqualComponentName $ benchmarkName bm

-- ---------------------------------------------------------------------------
-- Library file names

-- | Create a library name for a static library from a given name.
-- Prepends @lib@ and appends the static library extension (@.a@).
mkGenericStaticLibName :: String -> String
mkGenericStaticLibName lib = "lib" ++ lib <.> "a"

mkLibName :: UnitId -> String
mkLibName lib = mkGenericStaticLibName (getHSLibraryName lib)

mkProfLibName :: UnitId -> String
mkProfLibName lib = mkGenericStaticLibName (getHSLibraryName lib ++ "_p")

-- | Create a library name for a shared library from a given name.
-- Prepends @lib@ and appends the @-\<compilerFlavour\>\<compilerVersion\>@
-- as well as the shared library extension.
mkGenericSharedLibName :: Platform -> CompilerId -> String -> String
mkGenericSharedLibName platform (CompilerId compilerFlavor compilerVersion) lib =
  mconcat ["lib", lib, "-", comp <.> dllExtension platform]
  where
    comp = prettyShow compilerFlavor ++ prettyShow compilerVersion

-- Implement proper name mangling for dynamical shared objects
-- @libHS\<packagename\>-\<compilerFlavour\>\<compilerVersion\>@
-- e.g. @libHSbase-2.1-ghc6.6.1.so@
mkSharedLibName :: Platform -> CompilerId -> UnitId -> String
mkSharedLibName platform comp lib =
  mkGenericSharedLibName platform comp (getHSLibraryName lib)

-- Static libs are named the same as shared libraries, only with
-- a different extension.
mkStaticLibName :: Platform -> CompilerId -> UnitId -> String
mkStaticLibName platform (CompilerId compilerFlavor compilerVersion) lib =
  "lib" ++ getHSLibraryName lib ++ "-" ++ comp <.> staticLibExtension platform
  where
    comp = prettyShow compilerFlavor ++ prettyShow compilerVersion

-- | Create a library name for a bundled shared library from a given name.
-- This matches the naming convention for shared libraries as implemented in
-- GHC's packageHsLibs function in the Packages module.
-- If the given name is prefixed with HS, then this prepends 'lib' and appends
-- the compiler flavour/version and shared library extension e.g.:
--     "HSrts-1.0" -> "libHSrts-1.0-ghc8.7.20190109.so"
-- Otherwise the given name should be prefixed with 'C', then this strips the
-- 'C', prepends 'lib' and appends the shared library extension e.g.:
--     "Cffi" -> "libffi.so"
mkGenericSharedBundledLibName :: Platform -> CompilerId -> String -> String
mkGenericSharedBundledLibName platform comp lib
  | "HS" `isPrefixOf` lib =
      mkGenericSharedLibName platform comp lib
  | Just lib' <- stripPrefix "C" lib =
      "lib" ++ lib' <.> dllExtension platform
  | otherwise =
      error ("Don't understand library name " ++ lib)

-- ------------------------------------------------------------

-- * Platform file extensions

-- ------------------------------------------------------------

-- | Default extension for executable files on the current platform.
-- (typically @\"\"@ on Unix and @\"exe\"@ on Windows or OS\/2)
exeExtension :: Platform -> String
exeExtension platform = case platform of
  Platform _ Windows -> "exe"
  Platform Wasm32 _ -> "wasm"
  _ -> ""

-- | Extension for object files. For GHC the extension is @\"o\"@.
objExtension :: String
objExtension = "o"

-- | Extension for dynamically linked (or shared) libraries
-- (typically @\"so\"@ on Unix and @\"dll\"@ on Windows)
dllExtension :: Platform -> String
dllExtension (Platform _arch os) = case os of
  Windows -> "dll"
  OSX -> "dylib"
  _ -> "so"

-- | Extension for static libraries
--
-- TODO: Here, as well as in dllExtension, it's really the target OS that we're
-- interested in, not the build OS.
staticLibExtension :: Platform -> String
staticLibExtension (Platform _arch os) = case os of
  Windows -> "lib"
  _ -> "a"
