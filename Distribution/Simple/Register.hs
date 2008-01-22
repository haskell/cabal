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
                                           InstallDirs(..),
					   absoluteInstallDirs)
import Distribution.Simple.Compiler (CompilerFlavor(..), Compiler(..),
                                     PackageDB(..))
import Distribution.Simple.Program (ConfiguredProgram, programPath,
                                    programArgs, rawSystemProgram,
                                    lookupProgram, ghcPkgProgram)
import Distribution.Simple.Setup (RegisterFlags(..), CopyDest(..),
                                  fromFlag, fromFlagOrDefault)
import Distribution.PackageDescription (setupMessage, PackageDescription(..),
					BuildInfo(..), Library(..), haddockName)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
	(InstalledPackageInfo, showInstalledPackageInfo, 
	 emptyInstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose,
                                  copyFileVerbose, die, info)
import Distribution.System

import System.FilePath ((</>), (<.>), isAbsolute)
import System.Directory (removeFile, getCurrentDirectory,
                         removeDirectoryRecursive,
                         setPermissions, getPermissions,
			 Permissions(executable))
import System.IO.Error (try)

import Control.Monad (when)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
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
    setupMessage (fromFlag $ regVerbose regFlags) "No package to register" pkg_descr
    return ()
  | otherwise = do
    let isWindows = case os of Windows _ -> True; _ -> False
        genScript = fromFlag (regGenScript regFlags)
        genPkgConf = isJust (fromFlag (regGenPkgConf regFlags))
        genPkgConfigDefault = showPackageId (package pkg_descr) <.> "conf"
        genPkgConfigFile = fromMaybe genPkgConfigDefault
                                     (fromFlag (regGenPkgConf regFlags))
        verbosity = fromFlag (regVerbose regFlags)
        packageDB = fromFlagOrDefault (withPackageDB lbi) (regPackageDB regFlags)
	inplace  = fromFlag (regInPlace regFlags)
        message | genPkgConf = "Writing package registration file: "
                            ++ genPkgConfigFile ++ " for"
                | genScript = "Writing registration script: "
                           ++ regScriptLocation ++ " for"
                | otherwise = "Registering"
    setupMessage verbosity message pkg_descr

    case compilerFlavor (compiler lbi) of
      GHC -> do 
	config_flags <- case packageDB of
          GlobalPackageDB      -> return []
          UserPackageDB        -> return ["--user"]
          SpecificPackageDB db -> return ["-package-conf", db]

	let instConf | genPkgConf = genPkgConfigFile
                     | inplace    = inplacePkgConfigFile
		     | otherwise  = installedPkgConfigFile

        when (genPkgConf || not genScript) $ do
          info verbosity ("create " ++ instConf)
          writeInstalledConfig pkg_descr lbi inplace (Just instConf)

        let register_flags   = let conf = if genScript && not isWindows
		                             then ["-"]
		                             else [instConf]
                                in "update" : conf

        let allFlags = config_flags ++ register_flags
        let Just pkgTool = lookupProgram ghcPkgProgram (withPrograms lbi)

        case () of
          _ | genPkgConf -> return ()
            | genScript ->
              do cfg <- showInstalledConfig pkg_descr lbi inplace
                 rawSystemPipe pkgTool regScriptLocation cfg allFlags
          _ -> rawSystemProgram verbosity pkgTool allFlags

      Hugs -> do
	when inplace $ die "--inplace is not supported with Hugs"
        let installDirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
	createDirectoryIfMissingVerbose verbosity True (libdir installDirs)
	copyFileVerbose verbosity installedPkgConfigFile
	    (libdir installDirs </> "package.conf")
      JHC -> when (verbosity >= normal) $ putStrLn "registering for JHC (nothing to do)"
      NHC -> when (verbosity >= normal) $ putStrLn "registering nhc98 (nothing to do)"
      _   -> die ("only registering with GHC/Hugs/jhc/nhc98 is implemented")

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
    = do cfg <- mkInstalledPackageInfo pkg_descr lbi inplace
         return (showInstalledPackageInfo cfg)

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
	inplaceDirs = (absoluteInstallDirs pkg_descr lbi NoCopyDest) {
                        datadir    = pwd,
                        datasubdir = distPref,
                        docdir     = inplaceDocdir,
                        htmldir    = inplaceHtmldir,
                        haddockdir = inplaceHtmldir
                      }
	  where inplaceDocdir  = pwd </> distPref </> "doc"
	        inplaceHtmldir = inplaceDocdir </> "html"
		                               </> pkgName (package pkg_descr)
        (absinc,relinc) = partition isAbsolute (includeDirs bi)
        installIncludeDir | null (installIncludes bi) = []
                          | otherwise = [includedir installDirs]
        haddockInterfaceDir
         | inplace   = haddockdir inplaceDirs
         | otherwise = haddockdir installDirs
        haddockHtmlDir
         | inplace   = htmldir inplaceDirs
         | otherwise = htmldir installDirs
        libraryDir
         | inplace   = build_dir
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
                                            else installIncludeDir,
        IPI.includes	      = includes bi,
        IPI.depends           = packageDeps lbi,
        IPI.hugsOptions       = concat [opts | (Hugs,opts) <- options bi],
        IPI.ccOptions         = ccOptions bi,
        IPI.ldOptions         = ldOptions bi,
        IPI.frameworkDirs     = [],
        IPI.frameworks        = frameworks bi,
	IPI.haddockInterfaces = [haddockInterfaceDir </> haddockName pkg_descr],
	IPI.haddockHTMLs      = [haddockHtmlDir]
        }

-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> RegisterFlags -> IO ()
unregister pkg_descr lbi regFlags = do
  let genScript = fromFlag (regGenScript regFlags)
      verbosity = fromFlag (regVerbose regFlags)
      packageDB = fromFlagOrDefault (withPackageDB lbi) (regPackageDB regFlags)
      installDirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
  setupMessage verbosity "Unregistering" pkg_descr
  case compilerFlavor (compiler lbi) of
    GHC -> do
	config_flags <- case packageDB of
          GlobalPackageDB      -> return []
          UserPackageDB        -> return ["--user"]
          SpecificPackageDB db -> return ["-package-conf", db]

        let removeCmd = ["unregister",showPackageId (package pkg_descr)]
        let Just pkgTool = lookupProgram ghcPkgProgram (withPrograms lbi)
            allArgs      = removeCmd ++ config_flags
	if genScript
          then rawSystemEmit pkgTool unregScriptLocation allArgs
          else rawSystemProgram verbosity pkgTool allArgs
    Hugs -> do
        try $ removeDirectoryRecursive (libdir installDirs)
	return ()
    NHC -> do
        try $ removeDirectoryRecursive (libdir installDirs)
	return ()
    _ ->
	die ("only unregistering with GHC and Hugs is implemented")

-- |Like rawSystemProgram, but emits to a script instead of exiting.
-- FIX: chmod +x?
rawSystemEmit :: ConfiguredProgram  -- ^Program to run
              -> FilePath  -- ^Script name
              -> [String]  -- ^Args
              -> IO ()
rawSystemEmit prog scriptName extraArgs
 = case os of
       Windows _ ->
           writeFile scriptName ("@" ++ path ++ concatMap (' ':) args)
       _ -> do writeFile scriptName ("#!/bin/sh\n\n"
                                  ++ (path ++ concatMap (' ':) args)
                                  ++ "\n")
               p <- getPermissions scriptName
               setPermissions scriptName p{executable=True}
  where args = programArgs prog ++ extraArgs
        path = programPath prog

-- |Like rawSystemEmit, except it has string for pipeFrom. FIX: chmod +x
rawSystemPipe :: ConfiguredProgram
              -> FilePath  -- ^Script location
              -> String    -- ^where to pipe from
              -> [String]  -- ^Args
              -> IO ()
rawSystemPipe prog scriptName pipeFrom extraArgs
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
        args = programArgs prog ++ extraArgs
        path = programPath prog

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
