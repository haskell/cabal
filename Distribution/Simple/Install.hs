-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: Perform the ".\/setup install" action.  Move files into
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
	hugsProgramsDir,
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
	setupMessage, hasLibs, withLib, libModules, exeModules,
	hcOptions)
import Distribution.Package (showPackageId, PackageIdentifier(pkgName))
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import Distribution.Simple.Utils(moveSources, mkLibName, die)
import Distribution.Setup (CompilerFlavor(..), Compiler(..))

import Control.Monad(when, unless)
import Data.Maybe(fromMaybe)
import Distribution.Compat.Directory(copyFile,createDirectoryIfMissing,removeDirectoryRecursive)
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
        -> Maybe FilePath -- ^install-prefix
        -> IO ()
install pkg_descr lbi install_prefixM = do
  let buildPref = buildDir lbi
  let libPref = mkLibDir pkg_descr lbi install_prefixM
  let targetLibPref = mkLibDir pkg_descr lbi Nothing
  let binPref = mkBinDir pkg_descr lbi install_prefixM
  setupMessage ("Installing: " ++ libPref ++ " & " ++ binPref) pkg_descr
  case compilerFlavor (compiler lbi) of
     GHC  -> do when (hasLibs pkg_descr) (installLibGHC libPref buildPref pkg_descr)
                installExeGhc binPref buildPref pkg_descr
     Hugs -> installHugs libPref binPref targetLibPref buildPref pkg_descr
     _    -> die ("only installing with GHC or Hugs is implemented")
  return ()
  -- register step should be performed by caller.

-- |Install executables for GHC.
installExeGhc :: FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installExeGhc pref buildPref pkg_descr
    = do createDirectoryIfMissing True pref
         sequence_ [copyFile (buildPref `joinFileName` (hsSourceDir b) `joinFileName` e) (pref `joinFileName` e)
                    | Executable e _ _ b <- executables pkg_descr]

-- |Install for ghc, .hi and .a
installLibGHC :: FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installLibGHC pref buildPref pd@PackageDescription{library=Just l,
                                                   package=p}
    = do moveSources (buildPref `joinFileName` (hsSourceDir $ libBuildInfo l)) pref (libModules pd) ["hi"]
         copyFile (mkLibName buildPref (showPackageId p))
                    (mkLibName pref (showPackageId p))
installLibGHC _ _ PackageDescription{library=Nothing}
    = die $ "Internal Error. installLibGHC called with no library."

-- |Install for Hugs
installHugs
    :: FilePath -- ^Library install location
    -> FilePath -- ^Executable install location
    -> FilePath -- ^Library location on target system
    -> FilePath -- ^Build location
    -> PackageDescription
    -> IO ()
installHugs libPref binPref targetLibPref buildPref pkg_descr = do
    let hugsInstallDir = libPref `joinFileName` "hugs"
    let hugsTargetDir = targetLibPref `joinFileName` "hugs"
    let pkg_name = pkgName (package pkg_descr)
    withLib pkg_descr () $ \ libInfo -> do
	let pkgDir = hugsInstallDir `joinFileName` "packages"
		    `joinFileName` pkg_name
	try $ removeDirectoryRecursive pkgDir
	moveSources buildPref pkgDir (libModules pkg_descr) hugsInstallSuffixes
    unless (null (executables pkg_descr)) $ do
	let progBuildDir = buildPref `joinFileName` "programs"
	let progInstallDir = hugsInstallDir `joinFileName` "programs"
		    `joinFileName` pkg_name
	let progTargetDir = hugsTargetDir `joinFileName` "programs"
		    `joinFileName` pkg_name
	try $ removeDirectoryRecursive progInstallDir
	moveSources progBuildDir progInstallDir
	    (exeModules pkg_descr) hugsInstallSuffixes
	flip mapM_ (executables pkg_descr) $ \ exe -> do
	    let fname = hugsMainFilename exe
	    let installName = progInstallDir `joinFileName` fname
	    copyFile (progBuildDir `joinFileName` fname) installName
#ifndef mingw32_TARGET_OS
	    -- FIX (HUGS): works for Unix only
	    let targetName = progTargetDir `joinFileName` fname
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

-- |Prefix for Hugs package directories
hugsPackageDir :: PackageDescription -> LocalBuildInfo -> FilePath
hugsPackageDir pkg_descr lbi =
    prefix lbi `joinFileName` "lib" `joinFileName` "hugs"
	`joinFileName` "packages" `joinFileName` pkgName (package pkg_descr)

-- |Prefix for Hugs program directories
hugsProgramsDir :: PackageDescription -> LocalBuildInfo -> FilePath
hugsProgramsDir pkg_descr lbi =
    prefix lbi `joinFileName` "lib" `joinFileName` "hugs"
	`joinFileName` "programs" `joinFileName` pkgName (package pkg_descr)

-- |Filename used by Hugs for the main module of an executable.
-- This is a simple filename, so that Hugs will look for any auxiliary
-- modules it uses relative to the directory it's in.
hugsMainFilename :: Executable -> FilePath
hugsMainFilename exe = (exeName exe ++ "-Main") `joinFileExt` ext
  where (_, ext) = splitFileExt (modulePath exe)

-- -----------------------------------------------------------------------------
-- Installation policies

mkLibDir :: PackageDescription -> LocalBuildInfo -> Maybe FilePath -> FilePath
mkLibDir pkg_descr lbi install_prefixM = 
  (fromMaybe (prefix lbi) install_prefixM) `joinFileName`
#ifndef mingw32_TARGET_OS
                 "lib" `joinFileName`
#endif
	         showPackageId (package pkg_descr)

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
