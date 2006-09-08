{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Register
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
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

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 604
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), mkLibDir, mkHaddockDir,
					   mkIncludeDir)
import Distribution.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Setup (RegisterFlags(..), CopyDest(..), userOverride)
import Distribution.PackageDescription (setupMessage, PackageDescription(..),
					BuildInfo(..), Library(..), haddockName)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.Version (Version(..))
import Distribution.InstalledPackageInfo
	(InstalledPackageInfo, showInstalledPackageInfo, 
	 emptyInstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Utils (rawSystemExit, copyFileVerbose, die)
import Distribution.Simple.Hugs (hugsPackageDir)
import Distribution.Simple.GHCPackageConfig (mkGHCPackageConfig, showGHCPackageConfig)
import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, canWriteLocalPackageConfig, maybeCreateLocalPackageConfig)
import Distribution.Compat.Directory
       (createDirectoryIfMissing,removeDirectoryRecursive,
        setPermissions, getPermissions, Permissions(executable)
       )

import Distribution.Compat.FilePath (joinFileName, joinPaths, splitFileName,
				     isAbsolutePath)

import System.Directory(doesFileExist, removeFile, getCurrentDirectory)
import System.IO.Error (try)

import Control.Monad (when, unless)
import Data.Maybe (isNothing, fromJust)
import Data.List (partition)

#ifdef DEBUG
import HUnit (Test)
#endif

regScriptLocation :: FilePath
#if mingw32_HOST_OS || mingw32_TARGET_OS
regScriptLocation = "register.bat"
#else
regScriptLocation = "register.sh"
#endif

unregScriptLocation :: FilePath
#if mingw32_HOST_OS || mingw32_TARGET_OS
unregScriptLocation = "unregister.bat"
#else
unregScriptLocation = "unregister.sh"
#endif

-- -----------------------------------------------------------------------------
-- Registration

register :: PackageDescription -> LocalBuildInfo
         -> RegisterFlags -- ^Install in the user's database?; verbose
         -> IO ()
register pkg_descr lbi regFlags
  | isNothing (library pkg_descr) = do
    setupMessage "No package to register" pkg_descr
    return ()
  | otherwise = do
    let ghc_63_plus = compilerVersion (compiler lbi) >= Version [6,3] []
        genScript = regGenScript regFlags
        verbose = regVerbose regFlags
        user = regUser regFlags `userOverride` userConf lbi
	inplace = regInPlace regFlags
    setupMessage (if genScript
                     then ("Writing registration script: " ++ regScriptLocation)
                     else "Registering")
                 pkg_descr
    case compilerFlavor (compiler lbi) of
      GHC -> do 
	config_flags <-
	   if user
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

	let instConf = if inplace then inplacePkgConfigFile 
				  else installedPkgConfigFile

        instConfExists <- doesFileExist instConf
        when (not instConfExists && not genScript) $ do
          when (verbose > 0) $
            putStrLn ("create " ++ instConf)
          writeInstalledConfig pkg_descr lbi inplace

	let register_flags
		| ghc_63_plus = "update":
#if !(mingw32_HOST_OS || mingw32_TARGET_OS)
		                 if genScript
                                    then []
                                    else 
#endif
                                      [instConf]
		| otherwise   = "--update-package":
#if !(mingw32_HOST_OS || mingw32_TARGET_OS)
				 if genScript
                                    then []
                                    else
#endif
                                      ["--input-file="++instConf]
        
	let allFlags = register_flags
                       ++ config_flags
                       ++ if ghc_63_plus && genScript then ["-"] else []
        let pkgTool = case regWithHcPkg regFlags of
			 Just f  -> f
			 Nothing -> compilerPkgTool (compiler lbi)

        if genScript
         then do cfg <- showInstalledConfig pkg_descr lbi inplace
	         rawSystemPipe regScriptLocation verbose cfg
                           pkgTool allFlags
         else rawSystemExit verbose pkgTool allFlags

      Hugs -> do
	when inplace $ die "--inplace is not supported with Hugs"
	createDirectoryIfMissing True (hugsPackageDir pkg_descr lbi)
	copyFileVerbose verbose installedPkgConfigFile
	    (hugsPackageDir pkg_descr lbi `joinFileName` "package.conf")
      JHC -> when (verbose > 0) $ putStrLn "registering for JHC (nothing to do)"
      _   -> die ("only registering with GHC is implemented")

userPkgConfErr :: String -> IO a
userPkgConfErr local_conf = 
  die ("--user flag passed, but cannot write to local package config: "
    	++ local_conf )

-- -----------------------------------------------------------------------------
-- The installed package config

-- |Register doesn't drop the register info file, it must be done in a
-- separate step.
writeInstalledConfig :: PackageDescription -> LocalBuildInfo -> Bool -> IO ()
writeInstalledConfig pkg_descr lbi inplace = do
  pkg_config <- showInstalledConfig pkg_descr lbi inplace
  writeFile (if inplace then inplacePkgConfigFile else installedPkgConfigFile)
	    (pkg_config ++ "\n")

-- |Create a string suitable for writing out to the package config file
showInstalledConfig :: PackageDescription -> LocalBuildInfo -> Bool
  -> IO String
showInstalledConfig pkg_descr lbi inplace
  | (case compilerFlavor hc of GHC -> True; _ -> False) &&
    compilerVersion hc < Version [6,3] [] 
    = if inplace then
	  error "--inplace not supported for GHC < 6.3"
      else
	  return (showGHCPackageConfig (mkGHCPackageConfig pkg_descr lbi))
  | otherwise 
    = do cfg <- mkInstalledPackageInfo pkg_descr lbi inplace
         return (showInstalledPackageInfo cfg)
  where
  	hc = compiler lbi

removeInstalledConfig :: IO ()
removeInstalledConfig = do
  try (removeFile installedPkgConfigFile) >> return ()
  try (removeFile inplacePkgConfigFile) >> return ()

installedPkgConfigFile :: String
installedPkgConfigFile = ".installed-pkg-config"

inplacePkgConfigFile :: String
inplacePkgConfigFile = ".inplace-pkg-config"

-- -----------------------------------------------------------------------------
-- Making the InstalledPackageInfo

mkInstalledPackageInfo
	:: PackageDescription
	-> LocalBuildInfo
	-> Bool
	-> IO InstalledPackageInfo
mkInstalledPackageInfo pkg_descr lbi inplace = do 
  pwd <- getCurrentDirectory
  let 
	lib = fromJust (library pkg_descr) -- checked for Nothing earlier
        bi = libBuildInfo lib
	build_dir = pwd `joinFileName` buildDir lbi
	libdir = mkLibDir pkg_descr lbi NoCopyDest
	incdir = mkIncludeDir libdir
	(absinc,relinc) = partition isAbsolutePath (includeDirs bi)
        haddockDir = mkHaddockDir pkg_descr lbi NoCopyDest
        haddockFile = joinPaths haddockDir (haddockName pkg_descr)
    in
    return emptyInstalledPackageInfo{
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
        IPI.importDirs        = [if inplace then build_dir else libdir],
        IPI.libraryDirs       = (if inplace then build_dir else libdir)
				: extraLibDirs bi,
        IPI.hsLibraries       = ["HS" ++ showPackageId (package pkg_descr)],
        IPI.extraLibraries    = extraLibs bi,
        IPI.includeDirs       = absinc 
				 ++ if inplace 
					then map (pwd `joinFileName`) relinc
					else [incdir],
        IPI.includes	      = includes bi ++ map (snd.splitFileName)
						   (installIncludes bi),
        IPI.depends           = packageDeps lbi,
        IPI.hugsOptions       = concat [opts | (Hugs,opts) <- options bi],
        IPI.ccOptions         = ccOptions bi,
        IPI.ldOptions         = ldOptions bi,
        IPI.frameworkDirs     = [],
        IPI.frameworks        = frameworks bi,
	IPI.haddockInterfaces = [haddockFile],
	IPI.haddockHTMLs      = [haddockDir]
        }

-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> RegisterFlags -> IO ()
unregister pkg_descr lbi regFlags = do
  setupMessage "Unregistering" pkg_descr
  let ghc_63_plus = compilerVersion (compiler lbi) >= Version [6,3] []
      genScript = regGenScript regFlags
      verbose = regVerbose regFlags
      user = regUser regFlags `userOverride` userConf lbi
  case compilerFlavor (compiler lbi) of
    GHC -> do
	config_flags <-
	   if user
		then if ghc_63_plus
			then return ["--user"]
			else do
                          instConfExists <- doesFileExist installedPkgConfigFile
		          localConf <- GHC.localPackageConfig
		          unless instConfExists (userPkgConfErr localConf)
			  return ["--config-file=" ++ localConf]
		else return []
        let removeCmd = if ghc_63_plus
                        then ["unregister",showPackageId (package pkg_descr)]
                        else ["--remove-package="++(pkgName $ package pkg_descr)]
        let pkgTool = case regWithHcPkg regFlags of
			 Just f  -> f
			 Nothing -> compilerPkgTool (compiler lbi)
	rawSystemEmit unregScriptLocation genScript verbose pkgTool
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
#if mingw32_HOST_OS || mingw32_TARGET_OS
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
#if mingw32_HOST_OS || mingw32_TARGET_OS
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
