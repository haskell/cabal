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
--

module Distribution.Simple.BuildPaths (
    defaultDistPref, srcPref,
    hscolourPref, haddockPref,
    autogenModulesDir,

    autogenModuleName,
    cppHeaderName,
    haddockName,

    mkLibName,
    mkProfLibName,
    mkSharedLibName,

    exeExtension,
    objExtension,
    dllExtension,

  ) where


import Distribution.Package
import Distribution.ModuleName as ModuleName
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Text
import Distribution.System

import System.FilePath ((</>), (<.>))

-- ---------------------------------------------------------------------------
-- Build directories and files

srcPref :: FilePath -> FilePath
srcPref distPref = distPref </> "src"

hscolourPref :: FilePath -> PackageDescription -> FilePath
hscolourPref = haddockPref

haddockPref :: FilePath -> PackageDescription -> FilePath
haddockPref distPref pkg_descr
    = distPref </> "doc" </> "html" </> display (packageName pkg_descr)

-- |The directory in which we put auto-generated modules
autogenModulesDir :: LocalBuildInfo -> String
autogenModulesDir lbi = buildDir lbi </> "autogen"

cppHeaderName :: String
cppHeaderName = "cabal_macros.h"

-- |The name of the auto-generated module associated with a package
autogenModuleName :: PackageDescription -> ModuleName
autogenModuleName pkg_descr =
  ModuleName.fromString $
    "Paths_" ++ map fixchar (display (packageName pkg_descr))
  where fixchar '-' = '_'
        fixchar c   = c

haddockName :: PackageDescription -> FilePath
haddockName pkg_descr = display (packageName pkg_descr) <.> "haddock"

-- ---------------------------------------------------------------------------
-- Library file names

mkLibName :: UnitId -> String
mkLibName lib = "lib" ++ getHSLibraryName lib <.> "a"

mkProfLibName :: UnitId -> String
mkProfLibName lib =  "lib" ++ getHSLibraryName lib ++ "_p" <.> "a"

-- Implement proper name mangling for dynamical shared objects
-- libHS<packagename>-<compilerFlavour><compilerVersion>
-- e.g. libHSbase-2.1-ghc6.6.1.so
mkSharedLibName :: CompilerId -> UnitId -> String
mkSharedLibName (CompilerId compilerFlavor compilerVersion) lib
  = "lib" ++ getHSLibraryName lib ++ "-" ++ comp <.> dllExtension
  where comp = display compilerFlavor ++ display compilerVersion

-- ------------------------------------------------------------
-- * Platform file extensions
-- ------------------------------------------------------------

-- | Default extension for executable files on the current platform.
-- (typically @\"\"@ on Unix and @\"exe\"@ on Windows or OS\/2)
exeExtension :: String
exeExtension = case buildOS of
                   Windows -> "exe"
                   _       -> ""

-- | Extension for object files. For GHC the extension is @\"o\"@.
objExtension :: String
objExtension = "o"

-- | Extension for dynamically linked (or shared) libraries
-- (typically @\"so\"@ on Unix and @\"dll\"@ on Windows)
dllExtension :: String
dllExtension = case buildOS of
                   Windows -> "dll"
                   OSX     -> "dylib"
                   _       -> "so"
