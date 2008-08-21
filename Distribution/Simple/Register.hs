-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Register
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with registering and unregistering packages. There are a
-- couple ways it can do this, one is to do it directly. Another is to generate
-- a script that can be run later to do it. The idea here being that the user
-- is shielded from the details of what command to use for package registration
-- for a particular compiler. In practice this aspect was not especially
-- popular so we also provide a way to simply generate the package registration
-- file which then must be manually passed to @ghc-pkg@. It is possible to
-- generate registration information for where the package is to be installed,
-- or alternatively to register the package inplace in the build tree. The
-- latter is occasionally handy, and will become more important when we try to
-- build multi-package systems.
--
-- This module does not delegate anything to the per-compiler modules but just
-- mixes it all in in this module, which is rather unsatisfactory. The script
-- generation and the unregister feature are not well used or tested.

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
  ) where

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..),
                                           InstallDirs(..),
                                           absoluteInstallDirs)
import Distribution.Simple.BuildPaths (haddockName)
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor, PackageDB(..) )
import Distribution.Simple.Program (ConfiguredProgram, programPath,
                                    programArgs, rawSystemProgram,
                                    lookupProgram, ghcPkgProgram)
import Distribution.Simple.Setup
         ( RegisterFlags(..), CopyDest(..)
         , fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.PackageDescription (PackageDescription(..),
                                              BuildInfo(..), Library(..))
import Distribution.Package
         ( Package(..), packageName )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, InstalledPackageInfo_(InstalledPackageInfo)
         , showInstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, copyFileVerbose, writeFileAtomic
         , die, info, notice, setupMessage )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Text
         ( display )

import System.FilePath ((</>), (<.>), isAbsolute)
import System.Directory (removeFile, getCurrentDirectory,
                         removeDirectoryRecursive,
                         setPermissions, getPermissions,
                         Permissions(executable))
import System.IO.Error (try)

import Control.Monad (when)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.List (partition)

regScriptLocation :: FilePath
regScriptLocation = case buildOS of
                        Windows -> "register.bat"
                        _       -> "register.sh"

unregScriptLocation :: FilePath
unregScriptLocation = case buildOS of
                          Windows -> "unregister.bat"
                          _       -> "unregister.sh"

-- -----------------------------------------------------------------------------
-- Registration

register :: PackageDescription -> LocalBuildInfo
         -> RegisterFlags -- ^Install in the user's database?; verbose
         -> IO ()
register pkg_descr lbi regFlags
  | isNothing (library pkg_descr) = do
    setupMessage (fromFlag $ regVerbosity regFlags) "No package to register" (packageId pkg_descr)
    return ()
  | otherwise = do
    let distPref = fromFlag $ regDistPref regFlags
        isWindows = case buildOS of Windows -> True; _ -> False
        genScript = fromFlag (regGenScript regFlags)
        genPkgConf = isJust (flagToMaybe (regGenPkgConf regFlags))
        genPkgConfigDefault = display (packageId pkg_descr) <.> "conf"
        genPkgConfigFile = fromMaybe genPkgConfigDefault
                                     (fromFlag (regGenPkgConf regFlags))
        verbosity = fromFlag (regVerbosity regFlags)
        packageDB = fromFlagOrDefault (withPackageDB lbi) (regPackageDB regFlags)
        inplace  = fromFlag (regInPlace regFlags)
        message | genPkgConf = "Writing package registration file: "
                            ++ genPkgConfigFile ++ " for"
                | genScript = "Writing registration script: "
                           ++ regScriptLocation ++ " for"
                | otherwise = "Registering"
    setupMessage verbosity message (packageId pkg_descr)

    case compilerFlavor (compiler lbi) of
      GHC -> do
        config_flags <- case packageDB of
          GlobalPackageDB      -> return []
          UserPackageDB        -> return ["--user"]
          SpecificPackageDB db -> return ["--package-conf=" ++ db]

        let instConf | genPkgConf = genPkgConfigFile
                     | inplace    = inplacePkgConfigFile distPref
                     | otherwise  = installedPkgConfigFile distPref

        when (genPkgConf || not genScript) $ do
          info verbosity ("create " ++ instConf)
          writeInstalledConfig distPref pkg_descr lbi inplace (Just instConf)

        let register_flags   = let conf = if genScript && not isWindows
                                             then ["-"]
                                             else [instConf]
                                in "update" : conf

        let allFlags = config_flags ++ register_flags
        let Just pkgTool = lookupProgram ghcPkgProgram (withPrograms lbi)

        case () of
          _ | genPkgConf -> return ()
            | genScript ->
              do cfg <- showInstalledConfig distPref pkg_descr lbi inplace
                 rawSystemPipe pkgTool regScriptLocation cfg allFlags
          _ -> rawSystemProgram verbosity pkgTool allFlags

      Hugs -> do
        when inplace $ die "--inplace is not supported with Hugs"
        let installDirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
        createDirectoryIfMissingVerbose verbosity True (libdir installDirs)
        copyFileVerbose verbosity (installedPkgConfigFile distPref)
            (libdir installDirs </> "package.conf")
      JHC -> notice verbosity "registering for JHC (nothing to do)"
      NHC -> notice verbosity "registering nhc98 (nothing to do)"
      _   -> die "only registering with GHC/Hugs/jhc/nhc98 is implemented"

-- -----------------------------------------------------------------------------
-- The installed package config

-- |Register doesn't drop the register info file, it must be done in a
-- separate step.
writeInstalledConfig :: FilePath -> PackageDescription -> LocalBuildInfo
                     -> Bool -> Maybe FilePath -> IO ()
writeInstalledConfig distPref pkg_descr lbi inplace instConfOverride = do
  pkg_config <- showInstalledConfig distPref pkg_descr lbi inplace
  let instConfDefault | inplace   = inplacePkgConfigFile distPref
                      | otherwise = installedPkgConfigFile distPref
      instConf = fromMaybe instConfDefault instConfOverride
  writeFileAtomic instConf (pkg_config ++ "\n")

-- |Create a string suitable for writing out to the package config file
showInstalledConfig :: FilePath -> PackageDescription -> LocalBuildInfo -> Bool
  -> IO String
showInstalledConfig distPref pkg_descr lbi inplace
    = do cfg <- mkInstalledPackageInfo distPref pkg_descr lbi inplace
         return (showInstalledPackageInfo cfg)

removeInstalledConfig :: FilePath -> IO ()
removeInstalledConfig distPref = do
  try $ removeFile $ installedPkgConfigFile distPref
  try $ removeFile $ inplacePkgConfigFile distPref
  return ()

removeRegScripts :: IO ()
removeRegScripts = do
  try $ removeFile regScriptLocation
  try $ removeFile unregScriptLocation
  return ()

installedPkgConfigFile :: FilePath -> FilePath
installedPkgConfigFile distPref = distPref </> "installed-pkg-config"

inplacePkgConfigFile :: FilePath -> FilePath
inplacePkgConfigFile distPref = distPref </> "inplace-pkg-config"

-- -----------------------------------------------------------------------------
-- Making the InstalledPackageInfo

mkInstalledPackageInfo
        :: FilePath
        -> PackageDescription
        -> LocalBuildInfo
        -> Bool
        -> IO InstalledPackageInfo
mkInstalledPackageInfo distPref pkg_descr lbi inplace = do
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
                                               </> display (packageName pkg_descr)
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
        hasModules = not $ null (exposedModules lib)
                        && null (otherModules bi)
        hasLibrary = hasModules || not (null (cSources bi))
    in
    return InstalledPackageInfo {
        IPI.package           = packageId pkg_descr,
        IPI.license           = license pkg_descr,
        IPI.copyright         = copyright pkg_descr,
        IPI.maintainer        = maintainer pkg_descr,
        IPI.author            = author pkg_descr,
        IPI.stability         = stability pkg_descr,
        IPI.homepage          = homepage pkg_descr,
        IPI.pkgUrl            = pkgUrl pkg_descr,
        IPI.description       = description pkg_descr,
        IPI.category          = category pkg_descr,
        IPI.exposed           = libExposed lib,
        IPI.exposedModules    = exposedModules lib,
        IPI.hiddenModules     = otherModules bi,
        IPI.importDirs        = [ libraryDir | hasModules ],
        IPI.libraryDirs       = if hasLibrary
                                  then libraryDir : extraLibDirs bi
                                  else              extraLibDirs bi,
        IPI.hsLibraries       = ["HS" ++ display (packageId pkg_descr)
                                | hasLibrary ],
        IPI.extraLibraries    = extraLibs bi,
        IPI.extraGHCiLibraries= [],
        IPI.includeDirs       = absinc ++ if inplace
                                            then map (pwd </>) relinc
                                            else installIncludeDir,
        IPI.includes          = includes bi,
        IPI.depends           = packageDeps lbi,
        IPI.hugsOptions       = concat [opts | (Hugs,opts) <- options bi],
        IPI.ccOptions         = [], -- NB. NOT ccOptions bi!
                                    -- We don't want cc-options to be
                                    -- propagated to C ompilations in other
                                    -- packages.
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
      verbosity = fromFlag (regVerbosity regFlags)
      packageDB = fromFlagOrDefault (withPackageDB lbi) (regPackageDB regFlags)
      installDirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
  setupMessage verbosity "Unregistering" (packageId pkg_descr)
  case compilerFlavor (compiler lbi) of
    GHC -> do
        config_flags <- case packageDB of
          GlobalPackageDB      -> return []
          UserPackageDB        -> return ["--user"]
          SpecificPackageDB db -> return ["--package-conf=" ++ db]

        let removeCmd = ["unregister", display (packageId pkg_descr)]
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
 = case buildOS of
       Windows ->
           writeFileAtomic scriptName ("@" ++ path ++ concatMap (' ':) args)
       _ -> do writeFileAtomic scriptName ("#!/bin/sh\n\n"
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
 = case buildOS of
       Windows ->
           writeFileAtomic scriptName ("@" ++ path ++ concatMap (' ':) args)
       _ -> do writeFileAtomic scriptName ("#!/bin/sh\n\n"
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
