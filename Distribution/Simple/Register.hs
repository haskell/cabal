{-# OPTIONS -cpp #-}
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
        removeRegScripts,
#ifdef DEBUG
        hunitTests, installedPkgConfigFile
#endif
  ) where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 604
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), distPref,
                                           InstallDirs(..), haddockdir,
                                           InstallDirTemplates(..),
					   absoluteInstallDirs, toPathTemplate)
import Distribution.Compiler (CompilerFlavor(..), Compiler(..),
                              compilerPkgToolPath, compilerVersion)
import Distribution.Program (Program(..), ProgramLocation(..))
import Distribution.Setup (RegisterFlags(..), CopyDest(..), userOverride)
import Distribution.PackageDescription (setupMessage, PackageDescription(..),
					BuildInfo(..), Library(..), haddockName)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.Version (Version(..))
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
	(InstalledPackageInfo, showInstalledPackageInfo, 
	 emptyInstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose,
                                  rawSystemExit, copyFileVerbose, die)
import Distribution.Simple.GHCPackageConfig (mkGHCPackageConfig, showGHCPackageConfig)
import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, canWriteLocalPackageConfig, maybeCreateLocalPackageConfig)
import Distribution.System
import Distribution.Compat.Directory
       (removeDirectoryRecursive,
        setPermissions, getPermissions, Permissions(executable)
       )

import System.FilePath ((</>), (<.>), isAbsolute)

import System.Directory(doesFileExist, removeFile, getCurrentDirectory)
import System.IO.Error (try)

import Control.Monad (when, unless)
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.List (partition)

#ifdef DEBUG
import Test.HUnit (Test)
#endif

regScriptLocation :: FilePath
regScriptLocation = case os of
                        Windows _ -> "register.bat"
                        _         -> "register.sh"

unregScriptLocation :: FilePath
unregScriptLocation = case os of
                          Windows _ -> "unregister.bat"
                          _         -> "unregister.sh"

-- -----------------------------------------------------------------------------
-- Registration

register :: PackageDescription -> LocalBuildInfo
         -> RegisterFlags -- ^Install in the user's database?; verbose
         -> IO ()
register pkg_descr lbi regFlags
  | isNothing (library pkg_descr) = do
    setupMessage (regVerbose regFlags) "No package to register" pkg_descr
    return ()
  | otherwise = do
    let ghc_63_plus = compilerVersion (compiler lbi) >= Version [6,3] []
        genScript = regGenScript regFlags
        genPkgConf = regGenPkgConf regFlags
        genPkgConfigDefault = showPackageId (package pkg_descr) <.> "conf"
        genPkgConfigFile = fromMaybe genPkgConfigDefault
                                     (regPkgConfFile regFlags)
        verbosity = regVerbose regFlags
        user = regUser regFlags `userOverride` userConf lbi
	inplace = regInPlace regFlags
        hc = updateCompilerPkgTool (regWithHcPkg regFlags) (compiler lbi)
        message | genPkgConf = "Writing package registration file: "
                            ++ genPkgConfigFile ++ " for"
                | genScript = "Writing registration script: "
                           ++ regScriptLocation ++ " for"
                | otherwise = "Registering"
    setupMessage (regVerbose regFlags) message pkg_descr

    case compilerFlavor hc of
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

	let instConf | genPkgConf = genPkgConfigFile
                     | inplace    = inplacePkgConfigFile
		     | otherwise  = installedPkgConfigFile

        when (genPkgConf || not genScript) $ do
          when (verbosity >= verbose) $
            putStrLn ("create " ++ instConf)
          writeInstalledConfig pkg_descr lbi inplace (Just instConf)

        let register_flags
                | ghc_63_plus = let conf = case os of
                                               Windows MingW
                                                | genScript -> []
                                               _ ->            [instConf]
                                in "update" : conf
                | otherwise   = let conf = case os of
                                               Windows MingW
                                                | genScript -> []
                                               _ -> ["--input-file="++instConf]
                                in "--update-package" : conf

        let allFlags = register_flags
                       ++ config_flags
                       ++ if ghc_63_plus && genScript then ["-"] else []
        let pkgTool = compilerPkgToolPath hc

        case () of
          _ | genPkgConf -> return ()
            | genScript ->
              do cfg <- showInstalledConfig pkg_descr lbi inplace
                 rawSystemPipe regScriptLocation verbosity cfg
                           pkgTool allFlags
          _ -> rawSystemExit verbosity pkgTool allFlags

      Hugs -> do
	when inplace $ die "--inplace is not supported with Hugs"
        let installDirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
	createDirectoryIfMissingVerbose verbosity True (libdir installDirs)
	copyFileVerbose verbosity installedPkgConfigFile
	    (libdir installDirs </> "package.conf")
      JHC -> when (verbosity >= normal) $ putStrLn "registering for JHC (nothing to do)"
      NHC -> when (verbosity >= normal) $ putStrLn "registering nhc98 (nothing to do)"
      _   -> die ("only registering with GHC/Hugs/jhc/nhc98 is implemented")

userPkgConfErr :: String -> IO a
userPkgConfErr local_conf = 
  die ("--user flag passed, but cannot write to local package config: "
    	++ local_conf )

-- -----------------------------------------------------------------------------
-- The installed package config

-- |Register doesn't drop the register info file, it must be done in a
-- separate step.
writeInstalledConfig :: PackageDescription -> LocalBuildInfo -> Bool
                     -> Maybe FilePath -> IO ()
writeInstalledConfig pkg_descr lbi inplace instConfOverride = do
  pkg_config <- showInstalledConfig pkg_descr lbi inplace
  let instConfDefault | inplace   = inplacePkgConfigFile
                      | otherwise = installedPkgConfigFile
      instConf = fromMaybe instConfDefault instConfOverride
  writeFile instConf (pkg_config ++ "\n")

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
  try $ removeFile installedPkgConfigFile
  try $ removeFile inplacePkgConfigFile
  return ()

removeRegScripts :: IO ()
removeRegScripts = do
  try $ removeFile regScriptLocation
  try $ removeFile unregScriptLocation
  return ()

installedPkgConfigFile :: FilePath
installedPkgConfigFile = distPref </> "installed-pkg-config"

inplacePkgConfigFile :: FilePath
inplacePkgConfigFile = distPref </> "inplace-pkg-config"

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
	build_dir = pwd </> buildDir lbi
        installDirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
        inplaceDirs = absoluteInstallDirs pkg_descr lbi {
                        installDirTemplates = (installDirTemplates lbi) {
                          dataDirTemplate    = toPathTemplate pwd,
                          dataSubdirTemplate = toPathTemplate distPref
                        }
                      } NoCopyDest
	(absinc,relinc) = partition isAbsolute (includeDirs bi)
        haddockDir  | inplace   = haddockdir installDirs pkg_descr
                    | otherwise = haddockdir inplaceDirs pkg_descr
        libraryDir  | inplace   = build_dir
                    | otherwise = libdir installDirs
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
        IPI.importDirs        = [libraryDir],
        IPI.libraryDirs       = libraryDir : extraLibDirs bi,
        IPI.hsLibraries       = ["HS" ++ showPackageId (package pkg_descr)],
        IPI.extraLibraries    = extraLibs bi,
        IPI.includeDirs       = absinc ++ if inplace
                                            then map (pwd </>) relinc
                                            else [includedir installDirs],
        IPI.includes	      = includes bi,
        IPI.depends           = packageDeps lbi,
        IPI.hugsOptions       = concat [opts | (Hugs,opts) <- options bi],
        IPI.ccOptions         = ccOptions bi,
        IPI.ldOptions         = ldOptions bi,
        IPI.frameworkDirs     = [],
        IPI.frameworks        = frameworks bi,
	IPI.haddockInterfaces = [haddockDir </> haddockName pkg_descr],
	IPI.haddockHTMLs      = [haddockDir]
        }

-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> RegisterFlags -> IO ()
unregister pkg_descr lbi regFlags = do
  setupMessage (regVerbose regFlags) "Unregistering" pkg_descr
  let ghc_63_plus = compilerVersion (compiler lbi) >= Version [6,3] []
      genScript = regGenScript regFlags
      verbosity = regVerbose regFlags
      user = regUser regFlags `userOverride` userConf lbi
      hc = updateCompilerPkgTool (regWithHcPkg regFlags) (compiler lbi)
      installDirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
  case compilerFlavor hc of
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
        let pkgTool = compilerPkgToolPath hc
	rawSystemEmit unregScriptLocation genScript verbosity pkgTool
	    (removeCmd++config_flags)
    Hugs -> do
        try $ removeDirectoryRecursive (libdir installDirs)
	return ()
    NHC -> do
        try $ removeDirectoryRecursive (libdir installDirs)
	return ()
    _ ->
	die ("only unregistering with GHC and Hugs is implemented")

updateCompilerPkgTool :: Maybe FilePath -> Compiler -> Compiler
updateCompilerPkgTool Nothing comp = comp
updateCompilerPkgTool (Just hcPkgPath) comp =
  comp {
    compilerPkgTool = (compilerPkgTool comp) {
      programLocation = UserSpecified hcPkgPath
    }
  }


-- |Like rawSystemExit, but optionally emits to a script instead of
-- exiting. FIX: chmod +x?
rawSystemEmit :: FilePath  -- ^Script name
              -> Bool      -- ^if true, emit, if false, run
              -> Verbosity -- ^Verbosity
              -> FilePath  -- ^Program to run
              -> [String]  -- ^Args
              -> IO ()
rawSystemEmit _ False verbosity path args
    = rawSystemExit verbosity path args
rawSystemEmit scriptName True _ path args
 = case os of
       Windows _ ->
           writeFile scriptName ("@" ++ path ++ concatMap (' ':) args)
       _ -> do writeFile scriptName ("#!/bin/sh\n\n"
                                  ++ (path ++ concatMap (' ':) args)
                                  ++ "\n")
               p <- getPermissions scriptName
               setPermissions scriptName p{executable=True}

-- |Like rawSystemEmit, except it has string for pipeFrom. FIX: chmod +x
rawSystemPipe :: FilePath  -- ^Script location
              -> Verbosity -- ^Verbosity
              -> String    -- ^where to pipe from
              -> FilePath  -- ^Program to run
              -> [String]  -- ^Args
              -> IO ()
rawSystemPipe scriptName _ pipeFrom path args
 = case os of
       Windows _ ->
           writeFile scriptName ("@" ++ path ++ concatMap (' ':) args)
       _ -> do writeFile scriptName ("#!/bin/sh\n\n"
                                  ++ "echo '" ++ escapeForShell pipeFrom
                                  ++ "' | "
                                  ++ (path ++ concatMap (' ':) args)
                                  ++ "\n")
               p <- getPermissions scriptName
               setPermissions scriptName p{executable=True}
  where escapeForShell [] = []
        escapeForShell ('\'':cs) = "'\\''" ++ escapeForShell cs
        escapeForShell (c   :cs) = c        : escapeForShell cs

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
