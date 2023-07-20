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
import Distribution.Simple.Setup.Common (defaultDistPref)
import Distribution.Simple.Setup.Haddock (HaddockTarget (..))
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Utils.Path
import Distribution.Verbosity
import System.FilePath (normalise, (<.>), (</>))

-- ---------------------------------------------------------------------------
-- Build directories and files

srcPref :: FilePath -> FilePath
srcPref distPref = distPref </> "src"

hscolourPref :: HaddockTarget -> FilePath -> PackageDescription -> FilePath
hscolourPref = haddockPref

-- | Build info json file, generated in every build
buildInfoPref :: FilePath -> FilePath
buildInfoPref distPref = distPref </> "build-info.json"

-- | This is the name of the directory in which the generated haddocks
-- should be stored. It does not include the @<dist>/doc/html@ prefix.
haddockDirName :: HaddockTarget -> PackageDescription -> FilePath
haddockDirName ForDevelopment = prettyShow . packageName
haddockDirName ForHackage = (++ "-docs") . prettyShow . packageId

-- | The directory to which generated haddock documentation should be written.
haddockPref :: HaddockTarget -> FilePath -> PackageDescription -> FilePath
haddockPref haddockTarget distPref pkg_descr =
  distPref </> "doc" </> "html" </> haddockDirName haddockTarget pkg_descr

-- | The directory in which we put auto-generated modules for EVERY
-- component in the package.
autogenPackageModulesDir :: LocalBuildInfo -> String
autogenPackageModulesDir lbi = buildDir lbi </> "global-autogen"

-- | The directory in which we put auto-generated modules for a
-- particular component.
autogenComponentModulesDir :: LocalBuildInfo -> ComponentLocalBuildInfo -> String
autogenComponentModulesDir lbi clbi = componentBuildDir lbi clbi </> "autogen"

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
  -> IO [(ModuleName.ModuleName, FilePath)]
getLibSourceFiles verbosity lbi lib clbi = getSourceFiles verbosity searchpaths modules
  where
    bi = libBuildInfo lib
    modules = allLibModules lib clbi
    searchpaths =
      componentBuildDir lbi clbi
        : map getSymbolicPath (hsSourceDirs bi)
        ++ [ autogenComponentModulesDir lbi clbi
           , autogenPackageModulesDir lbi
           ]

getExeSourceFiles
  :: Verbosity
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO [(ModuleName.ModuleName, FilePath)]
getExeSourceFiles verbosity lbi exe clbi = do
  moduleFiles <- getSourceFiles verbosity searchpaths modules
  srcMainPath <- findFileEx verbosity (map getSymbolicPath $ hsSourceDirs bi) (modulePath exe)
  return ((ModuleName.main, srcMainPath) : moduleFiles)
  where
    bi = buildInfo exe
    modules = otherModules bi
    searchpaths =
      autogenComponentModulesDir lbi clbi
        : autogenPackageModulesDir lbi
        : exeBuildDir lbi exe
        : map getSymbolicPath (hsSourceDirs bi)

getFLibSourceFiles
  :: Verbosity
  -> LocalBuildInfo
  -> ForeignLib
  -> ComponentLocalBuildInfo
  -> IO [(ModuleName.ModuleName, FilePath)]
getFLibSourceFiles verbosity lbi flib clbi = getSourceFiles verbosity searchpaths modules
  where
    bi = foreignLibBuildInfo flib
    modules = otherModules bi
    searchpaths =
      autogenComponentModulesDir lbi clbi
        : autogenPackageModulesDir lbi
        : flibBuildDir lbi flib
        : map getSymbolicPath (hsSourceDirs bi)

getSourceFiles
  :: Verbosity
  -> [FilePath]
  -> [ModuleName.ModuleName]
  -> IO [(ModuleName.ModuleName, FilePath)]
getSourceFiles verbosity dirs modules = flip traverse modules $ \m ->
  fmap ((,) m) $
    findFileWithExtension ["hs", "lhs", "hsig", "lhsig"] dirs (ModuleName.toFilePath m)
      >>= maybe (notFound m) (return . normalise)
  where
    notFound module_ =
      dieWithException verbosity $ CantFindSourceModule module_

-- | The directory where we put build results for an executable
exeBuildDir :: LocalBuildInfo -> Executable -> FilePath
exeBuildDir lbi exe = buildDir lbi </> nm </> nm ++ "-tmp"
  where
    nm = unUnqualComponentName $ exeName exe

-- | The directory where we put build results for a foreign library
flibBuildDir :: LocalBuildInfo -> ForeignLib -> FilePath
flibBuildDir lbi flib = buildDir lbi </> nm </> nm ++ "-tmp"
  where
    nm = unUnqualComponentName $ foreignLibName flib

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
