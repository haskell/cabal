-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: Perform the \"@.\/setup install@\" action.  Move files into
-- place based on the prefix argument.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Install (
	install,
	mkBinDir,
	mkLibDir,
	hugsPackageDir,
	hugsProgramsDirs,
	hugsMainFilename,
#ifdef DEBUG        
        hunitTests
#endif
  ) where

#if __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

import Distribution.PackageDescription (
	PackageDescription(..), BuildInfo(..), Executable(..), Library (..),
	setupMessage, hasLibs, withLib, libModules, withExe,
	hcOptions)
import Distribution.Package (showPackageId, PackageIdentifier(pkgName))
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import Distribution.Simple.Utils(smartCopySources, copyFileVerbose, mkLibName, die)
import Distribution.Setup (CompilerFlavor(..), Compiler(..))

import Control.Monad(when)
import Data.Maybe(fromMaybe)
import Distribution.Compat.Directory(createDirectoryIfMissing,removeDirectoryRecursive)
import Distribution.Compat.FilePath(joinFileName, dllExtension,
				    splitFileExt, joinFileExt)
import System.IO.Error(try)
import System.Directory(Permissions(..), getPermissions, setPermissions)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |FIX: nhc isn't implemented yet.
install :: PackageDescription
        -> LocalBuildInfo
        -> (Maybe FilePath,Int) -- ^install-prefix, verbose
        -> IO ()
install pkg_descr lbi (install_prefixM,verbose) = do
  let buildPref = buildDir lbi
  let libPref = mkLibDir pkg_descr lbi install_prefixM
  let targetLibPref = mkLibDir pkg_descr lbi Nothing
  let binPref = mkBinDir pkg_descr lbi install_prefixM
  setupMessage ("Installing: " ++ libPref ++ " & " ++ binPref) pkg_descr
  case compilerFlavor (compiler lbi) of
     GHC  -> do when (hasLibs pkg_descr) (installLibGHC verbose libPref buildPref pkg_descr)
                installExeGhc verbose binPref buildPref pkg_descr
     Hugs -> installHugs verbose libPref binPref targetLibPref buildPref pkg_descr
     _    -> die ("only installing with GHC or Hugs is implemented")
  return ()
  -- register step should be performed by caller.

-- |Install executables for GHC.
installExeGhc :: Int      -- ^verbose
              -> FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installExeGhc verbose pref buildPref pkg_descr
    = do createDirectoryIfMissing True pref
         withExe pkg_descr $ \ (Executable e _ b) ->
             copyFileVerbose verbose (buildPref `joinFileName` (hsSourceDir b) `joinFileName` e) (pref `joinFileName` e)

-- |Install for ghc, .hi and .a
installLibGHC :: Int      -- ^verbose
              -> FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installLibGHC verbose pref buildPref pd@PackageDescription{library=Just l,
                                                   package=p}
    = do smartCopySources verbose (buildPref `joinFileName` (hsSourceDir $ libBuildInfo l)) pref (libModules pd) ["hi"]
         copyFileVerbose verbose (mkLibName buildPref (showPackageId p)) (mkLibName pref (showPackageId p))
installLibGHC _ _ _ PackageDescription{library=Nothing}
    = die $ "Internal Error. installLibGHC called with no library."

-- Install for Hugs
-- The library goes in <libPref>/hugs/packages/<pkgname>
-- Each executable goes in <libPref>/hugs/programs/<exename>
-- with a script <binPref>/<exename> pointing at it.
installHugs
    :: Int      -- ^verbose
    -> FilePath -- ^Library install location
    -> FilePath -- ^Executable install location
    -> FilePath -- ^Library location on target system
    -> FilePath -- ^Build location
    -> PackageDescription
    -> IO ()
installHugs verbose libPref binPref targetLibPref buildPref pkg_descr = do
    let pkg_name = pkgName (package pkg_descr)
    withLib pkg_descr () $ \ libInfo -> do
        let pkgDir = libPref `joinFileName` "packages"
                    `joinFileName` pkg_name
        try $ removeDirectoryRecursive pkgDir
        smartCopySources verbose buildPref pkgDir (libModules pkg_descr) hugsInstallSuffixes
    let progBuildDir = buildPref `joinFileName` "programs"
    let progInstallDir = libPref `joinFileName` "programs"
    withExe pkg_descr $ \ exe -> do
        let buildDir = progBuildDir `joinFileName` exeName exe
        let installDir = progInstallDir `joinFileName` exeName exe
        let targetDir = progInstallDir `joinFileName` exeName exe
        try $ removeDirectoryRecursive installDir
        smartCopySources verbose buildDir installDir
            (otherModules (buildInfo exe)) hugsInstallSuffixes
        let fname = hugsMainFilename exe
        copyFileVerbose verbose (buildDir `joinFileName` fname)
            (installDir `joinFileName` fname)
#ifndef mingw32_TARGET_OS
        -- FIX (HUGS): works for Unix only
        let targetName = targetDir `joinFileName` fname
        let exeFile = binPref `joinFileName` exeName exe
        -- FIX (HUGS): use extensions, and options from file too?
        let hugsOptions = hcOptions Hugs (options (buildInfo exe))
        let script = unlines [
                "#! /bin/sh", 
                unwords ("runhugs" : hugsOptions ++ [targetName, "\"$@\""])]
        writeFile exeFile script
        perms <- getPermissions exeFile
        setPermissions exeFile perms { executable = True, readable = True }
#endif

hugsInstallSuffixes :: [String]
hugsInstallSuffixes = ["hs", "lhs", dllExtension]

-- |Hugs library directory for a package
hugsPackageDir :: PackageDescription -> LocalBuildInfo -> FilePath
hugsPackageDir pkg_descr lbi =
    mkLibDir pkg_descr lbi Nothing
	`joinFileName` "packages" `joinFileName` pkgName (package pkg_descr)

-- |Hugs program directories for a package
hugsProgramsDirs :: PackageDescription -> LocalBuildInfo -> [FilePath]
hugsProgramsDirs pkg_descr lbi =
    [exeDir `joinFileName` exeName exe |
         exe <- executables pkg_descr, buildable (buildInfo exe)]
  where exeDir = mkLibDir pkg_descr lbi Nothing `joinFileName` "programs"

-- |Filename used by Hugs for the main module of an executable.
-- This is a simple filename, so that Hugs will look for any auxiliary
-- modules it uses relative to the directory it's in.
hugsMainFilename :: Executable -> FilePath
hugsMainFilename exe = "Main" `joinFileExt` ext
  where (_, ext) = splitFileExt (modulePath exe)

-- -----------------------------------------------------------------------------
-- Installation policies

mkLibDir :: PackageDescription -> LocalBuildInfo -> Maybe FilePath -> FilePath
mkLibDir pkg_descr lbi install_prefixM = 
  case compilerFlavor (compiler lbi) of
    Hugs -> libDir `joinFileName` "hugs"
    _ -> libDir `joinFileName` showPackageId (package pkg_descr)
  where libDir = (fromMaybe (prefix lbi) install_prefixM)
#ifndef mingw32_TARGET_OS
                 `joinFileName` "lib"
#endif

mkBinDir :: PackageDescription -> LocalBuildInfo -> Maybe FilePath -> FilePath
mkBinDir _ lbi install_prefixM = 
  (fromMaybe (prefix lbi) install_prefixM) `joinFileName` "bin"

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
