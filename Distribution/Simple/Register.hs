-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Register
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC, Hugs
--
-- Explanation: Perform the \"@.\/setup register@\" action.
-- Uses a drop-file for HC-PKG.  See also "Distribution.InstalledPackageInfo".

{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

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

module Distribution.Simple.Register (
	register,
	unregister,
        writeInstalledConfig,
	removeInstalledConfig,
        installedPkgConfigFile,
        regScriptLocation,
        unregScriptLocation,
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

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Install (mkLibDir)
import Distribution.Setup (CompilerFlavor(..), Compiler(..), RegisterFlags)
import Distribution.PackageDescription (setupMessage, PackageDescription(..),
					BuildInfo(..), Library(..))
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.Version (Version(..))
import Distribution.InstalledPackageInfo
	(InstalledPackageInfo, showInstalledPackageInfo, 
	 emptyInstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Utils (rawSystemExit, copyFileVerbose, die)
import Distribution.Simple.Install (hugsPackageDir)
import Distribution.Simple.GHCPackageConfig (mkGHCPackageConfig, showGHCPackageConfig)
import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, canWriteLocalPackageConfig, maybeCreateLocalPackageConfig)
import Distribution.Compat.Directory
       (createDirectoryIfMissing,removeDirectoryRecursive,
        setPermissions, getPermissions, Permissions(executable)
       )
import Distribution.Compat.FilePath (joinFileName)

import System.Directory(doesFileExist, removeFile)
import System.IO.Error (try)

import Control.Monad (when, unless)
import Data.Maybe (isNothing, fromJust)

#ifdef DEBUG
import HUnit (Test)
#endif

regScriptLocation :: FilePath
#ifdef mingw32_TARGET_OS
regScriptLocation = "register.bat"
#else
regScriptLocation = "register.sh"
#endif

unregScriptLocation :: FilePath
#ifdef mingw32_TARGET_OS
unregScriptLocation = "unregister.bat"
#else
unregScriptLocation = "unregister.sh"
#endif

-- -----------------------------------------------------------------------------
-- Registration

-- |Be sure to call writeInstalledConfig first.  If the --user flag
-- was passed, and ~\/.ghc-packages is writable, or can be created,
-- then we use that file, perhaps creating it.

register :: PackageDescription -> LocalBuildInfo
         -> RegisterFlags -- ^Install in the user's database?; verbose
         -> IO ()
register pkg_descr lbi (userInst, genScript, verbose)
  | isNothing (library pkg_descr) = do
    setupMessage "No package to register" pkg_descr
    return ()
  | otherwise = do
    setupMessage (if genScript
                     then ("Writing registration script: " ++ regScriptLocation)
                     else "Registering")
                 pkg_descr
    case compilerFlavor (compiler lbi) of
      GHC -> do 
     	let ghc_63_plus = compilerVersion (compiler lbi) >= Version [6,3] []

	config_flags <-
	   if userInst
		then if ghc_63_plus
			then return ["--user"]
			else do 
			  GHC.maybeCreateLocalPackageConfig
		          localConf <- GHC.localPackageConfig
			  pkgConfWriteable <- GHC.canWriteLocalPackageConfig
		          when (not pkgConfWriteable && not genScript)
                                   $ userPkgConfErr localConf
			  return ["--config-file=" ++ localConf]
		else return []

        instConfExists <- doesFileExist installedPkgConfigFile
        when (not instConfExists && not genScript) $ do
          when (verbose > 0) $
            putStrLn ("create "++installedPkgConfigFile)
          writeInstalledConfig pkg_descr lbi

	let register_flags 
		| ghc_63_plus = "update":
#ifndef mingw32_TARGET_OS
		                 if genScript
                                    then []
                                    else 
#endif
                                      [installedPkgConfigFile]
		| otherwise   = "--update-package":
#ifndef mingw32_TARGET_OS
				 if genScript
                                    then []
                                    else
#endif
                                      ["--input-file="++installedPkgConfigFile]
        
	let allFlags = "--auto-ghci-libs":
		       (register_flags
                        ++ config_flags)
                       ++ if ghc_63_plus && genScript then ["-"] else []
        let pkgTool = compilerPkgTool (compiler lbi)

        if genScript
         then rawSystemPipe regScriptLocation verbose
                           (showInstalledConfig pkg_descr lbi)
                           pkgTool allFlags
         else rawSystemExit verbose pkgTool allFlags

      -- FIX (HUGS):
      Hugs -> do
	createDirectoryIfMissing True (hugsPackageDir pkg_descr lbi)
	copyFileVerbose verbose installedPkgConfigFile
	    (hugsPackageDir pkg_descr lbi `joinFileName` "package.conf")
      _   -> die ("only registering with GHC is implemented")

userPkgConfErr :: String -> IO a
userPkgConfErr local_conf = 
  die ("--user flag passed, but cannot write to local package config: "
    	++ local_conf )

-- |Register doesn't drop the register info file, it must be done in a separate step.
writeInstalledConfig :: PackageDescription -> LocalBuildInfo -> IO ()
writeInstalledConfig pkg_descr lbi = do
  let pkg_config = showInstalledConfig pkg_descr lbi
  writeFile installedPkgConfigFile (pkg_config ++ "\n")

-- |Create a string suitable for writing out to the package config file
showInstalledConfig :: PackageDescription -> LocalBuildInfo -> String
showInstalledConfig pkg_descr lbi
    = let hc = compiler lbi
      in case compilerFlavor hc of
          GHC | compilerVersion hc < Version [6,3] [] ->
	          showGHCPackageConfig (mkGHCPackageConfig pkg_descr lbi)
 	  _ -> showInstalledPackageInfo (mkInstalledPackageInfo pkg_descr lbi)

removeInstalledConfig :: IO ()
removeInstalledConfig = try (removeFile installedPkgConfigFile) >> return ()

installedPkgConfigFile :: String
installedPkgConfigFile = ".installed-pkg-config"

-- -----------------------------------------------------------------------------
-- Making the InstalledPackageInfo

mkInstalledPackageInfo
	:: PackageDescription
	-> LocalBuildInfo
	-> InstalledPackageInfo
mkInstalledPackageInfo pkg_descr lbi
  = let 
	lib = fromJust (library pkg_descr) -- checked for Nothing earlier
        bi = libBuildInfo lib
    in
    emptyInstalledPackageInfo{
        IPI.package           = package pkg_descr,
        IPI.license           = license pkg_descr,
        IPI.copyright         = copyright pkg_descr,
        IPI.maintainer        = maintainer pkg_descr,
	IPI.author	      = author pkg_descr,
        IPI.stability         = stability pkg_descr,
	IPI.homepage	      = homepage pkg_descr,
	IPI.pkgUrl	      = pkgUrl pkg_descr,
	IPI.description	      = description pkg_descr,
	IPI.category	      = category pkg_descr,
        IPI.exposed           = True,
	IPI.exposedModules    = exposedModules lib,
	IPI.hiddenModules     = otherModules bi,
        IPI.importDirs        = [mkLibDir pkg_descr lbi Nothing],
        IPI.libraryDirs       = [mkLibDir pkg_descr lbi Nothing],
        IPI.hsLibraries       = ["HS" ++ showPackageId (package pkg_descr)],
        IPI.extraLibraries    = extraLibs bi,
        IPI.includeDirs       = includeDirs bi,
        IPI.includes	      = includes bi,
        IPI.depends           = packageDeps lbi,
        IPI.hugsOptions       = concat [opts | (Hugs,opts) <- options bi],
        IPI.ccOptions         = ccOptions bi,
        IPI.ldOptions         = ldOptions bi,
        IPI.frameworkDirs     = [],
        IPI.frameworks        = frameworks bi,
	IPI.haddockInterfaces = [],
	IPI.haddockHTMLs      = []
  }

-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> RegisterFlags -> IO ()
unregister pkg_descr lbi (user_unreg, genScript, verbose) = do
  setupMessage "Unregistering" pkg_descr
  let ghc_63_plus = compilerVersion (compiler lbi) >= Version [6,3] []
  let theName = pkgName (package pkg_descr)
  case compilerFlavor (compiler lbi) of
    GHC -> do
	config_flags <-
	   if user_unreg
		then if ghc_63_plus
			then return ["--user"]
			else do
                          instConfExists <- doesFileExist installedPkgConfigFile
		          localConf <- GHC.localPackageConfig
		          unless instConfExists (userPkgConfErr localConf)
			  return ["--config-file=" ++ localConf]
		else return []
        let removeCmd = if ghc_63_plus
                        then ["unregister",theName]
                        else ["--remove-package="++theName]
	rawSystemEmit unregScriptLocation genScript verbose (compilerPkgTool (compiler lbi))
	    (removeCmd++config_flags)
    Hugs -> do
        try $ removeDirectoryRecursive (hugsPackageDir pkg_descr lbi)
	return ()
    _ ->
	die ("only unregistering with GHC and Hugs is implemented")

-- |Like rawSystemExit, but optionally emits to a script instead of
-- exiting. FIX: chmod +x?
rawSystemEmit :: FilePath -- ^Script name
              -> Bool     -- ^if true, emit, if false, run
              -> Int      -- ^Verbosity
              -> FilePath -- ^Program to run
              -> [String] -- ^Args
              -> IO ()
rawSystemEmit _ False verbosity path args
    = rawSystemExit verbosity path args
rawSystemEmit scriptName True verbosity path args = do
#ifdef mingw32_TARGET_OS
  writeFile scriptName ("@" ++ path ++ concatMap (' ':) args)
#else
  writeFile scriptName ("#!/bin/sh\n\n"
                        ++ (path ++ concatMap (' ':) args)
                        ++ "\n")
  p <- getPermissions scriptName
  setPermissions scriptName p{executable=True}
#endif

-- |Like rawSystemEmit, except it has string for pipeFrom. FIX: chmod +x
rawSystemPipe :: FilePath -- ^Script location
              -> Int      -- ^Verbosity
              -> String   -- ^where to pipe from
              -> FilePath -- ^Program to run
              -> [String] -- ^Args
              -> IO ()
rawSystemPipe scriptName verbose pipeFrom path args = do
#ifdef mingw32_TARGET_OS
  writeFile scriptName ("@" ++ path ++ concatMap (' ':) args)
#else
  writeFile scriptName ("#!/bin/sh\n\n"
                        ++ "echo '" ++ pipeFrom
                        ++ "' | " 
                        ++ (path ++ concatMap (' ':) args)
                        ++ "\n")
  p <- getPermissions scriptName
  setPermissions scriptName p{executable=True}
#endif

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
