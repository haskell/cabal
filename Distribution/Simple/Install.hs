-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
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
	hugsPackageDir,
	hugsProgramsDirs,
	hugsMainFilename,
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

import Distribution.PackageDescription (
	PackageDescription(..), BuildInfo(..), Executable(..), Library (..),
	setupMessage, hasLibs, withLib, libModules, withExe,
	hcOptions, autogenModuleName)
import Distribution.Package (showPackageId, PackageIdentifier(pkgName))
import Distribution.Program(ProgramConfiguration, Program(..), ProgramLocation(..),
                            rawSystemProgram, ranlibProgram,
                            lookupProgram, arProgram)
import Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..), mkLibDir, mkBinDir, mkDataDir, mkProgDir)
import Distribution.Simple.Utils(smartCopySources, copyFileVerbose, mkLibName,
                                 mkProfLibName, mkGHCiLibName, die, rawSystemVerbose)
import Distribution.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Setup (CopyFlags(..), CopyDest(..))

import Control.Monad(when)
import Data.List(any)
import Data.Maybe(fromMaybe)
import Distribution.Compat.Directory(createDirectoryIfMissing, removeDirectoryRecursive,
                                     findExecutable)
import Distribution.Compat.FilePath(splitFileName,joinFileName, dllExtension, exeExtension,
				    splitFileExt, joinFileExt)
import System.IO.Error(try)
import System.Directory(Permissions(..), getPermissions, setPermissions)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |FIX: nhc isn't implemented yet.
install :: PackageDescription
        -> LocalBuildInfo
        -> CopyFlags
        -> IO ()
install pkg_descr lbi (CopyFlags copydest verbose) = do
  when (not (null (dataFiles pkg_descr))) $ do
    let dataPref = mkDataDir pkg_descr lbi copydest
    createDirectoryIfMissing True dataPref
    flip mapM_ (dataFiles pkg_descr) $ \ file -> do
      let (dir, _) = splitFileName file
      createDirectoryIfMissing True (dataPref `joinFileName` dir)
      copyFileVerbose verbose file (dataPref `joinFileName` file)
  let buildPref = buildDir lbi
  let libPref = mkLibDir pkg_descr lbi copydest
  let binPref = mkBinDir pkg_descr lbi copydest
  setupMessage ("Installing: " ++ libPref ++ " & " ++ binPref) pkg_descr
  case compilerFlavor (compiler lbi) of
     GHC  -> do when (hasLibs pkg_descr) (installLibGHC verbose (withPrograms lbi) (withProfLib lbi) (withGHCiLib lbi) libPref buildPref pkg_descr)
                installExeGHC verbose binPref buildPref pkg_descr
     JHC  -> do withLib pkg_descr () $ installLibJHC verbose libPref buildPref pkg_descr
                withExe pkg_descr $ installExeJHC verbose binPref buildPref pkg_descr
     Hugs -> do
       let progPref = mkProgDir pkg_descr lbi copydest
       let targetProgPref = mkProgDir pkg_descr lbi NoCopyDest
       installHugs verbose libPref progPref binPref targetProgPref buildPref pkg_descr
     _    -> die ("only installing with GHC or Hugs is implemented")
  return ()
  -- register step should be performed by caller.

-- |Install executables for GHC.
installExeGHC :: Int      -- ^verbose
              -> FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installExeGHC verbose pref buildPref pkg_descr
    = do createDirectoryIfMissing True pref
         withExe pkg_descr $ \ (Executable e _ b) -> do
             let exeName = e `joinFileExt` exeExtension
             copyFileVerbose verbose (buildPref `joinFileName` e `joinFileName` exeName) (pref `joinFileName` exeName)

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLibGHC :: Int      -- ^verbose
              -> ProgramConfiguration
              -> Bool     -- ^has profiling library
	      -> Bool     -- ^has GHCi libs
              -> FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installLibGHC verbose programConf hasProf hasGHCi pref buildPref
              pd@PackageDescription{library=Just l,
                                    package=p}
    = do smartCopySources verbose [buildPref] pref (libModules pd) ["hi"] True False
         ifProf $ smartCopySources verbose [buildPref] pref (libModules pd) ["p_hi"] True False
         let libTargetLoc = mkLibName pref (showPackageId p)
             profLibTargetLoc = mkProfLibName pref (showPackageId p)
	     libGHCiTargetLoc = mkGHCiLibName pref (showPackageId p)
         copyFileVerbose verbose (mkLibName buildPref (showPackageId p)) libTargetLoc
         ifProf $ copyFileVerbose verbose (mkProfLibName buildPref (showPackageId p)) profLibTargetLoc
	 ifGHCi $ copyFileVerbose verbose (mkGHCiLibName buildPref (showPackageId p)) libGHCiTargetLoc

         -- use ranlib or ar -s to build an index. this is necessary
         -- on some systems like MacOS X.  If we can't find those,
         -- don't worry too much about it.
         let progName = programName $ ranlibProgram
         mProg <- lookupProgram progName programConf
         case foundProg mProg of
           Just rl  -> do rawSystemProgram verbose rl [libTargetLoc]
                          ifProf $ rawSystemProgram verbose rl [profLibTargetLoc]

           Nothing -> do let progName = programName $ arProgram
                         mProg <- lookupProgram progName programConf
                         case mProg of
                          Just ar  -> do rawSystemProgram verbose ar ["-s", libTargetLoc]
                                         ifProf $ rawSystemProgram verbose ar ["-s", profLibTargetLoc]
                          Nothing -> setupMessage  "Warning: Unable to generate index for library (missing ranlib and ar)" pd
         return ()
    where ifProf action = when hasProf (action >> return ())
	  ifGHCi action = when hasGHCi (action >> return ())
installLibGHC _ _ _ _ _ _ PackageDescription{library=Nothing}
    = die $ "Internal Error. installLibGHC called with no library."

-- Also checks whether the program was actually found.
foundProg :: Maybe Program -> Maybe Program
foundProg Nothing = Nothing
foundProg (Just Program{programLocation=EmptyLocation}) = Nothing
foundProg x = x

installLibJHC :: Int -> FilePath -> FilePath -> PackageDescription -> Library -> IO ()
installLibJHC verb dest build pkg_descr _ = do
    let p = showPackageId (package pkg_descr)++".hl"
    createDirectoryIfMissing True dest
    copyFileVerbose verb (joinFileName build p) (joinFileName dest p)

installExeJHC :: Int -> FilePath -> FilePath -> PackageDescription -> Executable -> IO ()
installExeJHC verb dest build pkg_descr exe = do
    let out   = exeName exe `joinFileName` exeExtension
    createDirectoryIfMissing True dest
    copyFileVerbose verb (joinFileName build out) (joinFileName dest out)


-- Install for Hugs
-- For install, copy-prefix = prefix, but for copy they're different.
-- The library goes in <copy-prefix>/lib/hugs/packages/<pkgname>
-- (i.e. <prefix>/lib/hugs/packages/<pkgname> on the target system).
-- Each executable goes in <copy-prefix>/lib/hugs/programs/<exename>
-- (i.e. <prefix>/lib/hugs/programs/<exename> on the target system)
-- with a script <copy-prefix>/bin/<exename> pointing at
-- <prefix>/lib/hugs/programs/<exename>
installHugs
    :: Int      -- ^verbose
    -> FilePath -- ^Library install location
    -> FilePath -- ^Program install location
    -> FilePath -- ^Executable install location
    -> FilePath -- ^Program location on target system
    -> FilePath -- ^Build location
    -> PackageDescription
    -> IO ()
installHugs verbose libDir installProgDir binDir targetProgDir buildPref pkg_descr = do
    let pkg_name = pkgName (package pkg_descr)
    withLib pkg_descr () $ \ libInfo -> do
        try $ removeDirectoryRecursive libDir
        smartCopySources verbose [buildPref] libDir (libModules pkg_descr) hugsInstallSuffixes True False
    let buildProgDir = buildPref `joinFileName` "programs"
    when (any (buildable . buildInfo) (executables pkg_descr)) $
        createDirectoryIfMissing True binDir
    withExe pkg_descr $ \ exe -> do
        let buildDir = buildProgDir `joinFileName` exeName exe
        let installDir = installProgDir `joinFileName` exeName exe
        let targetDir = targetProgDir `joinFileName` exeName exe
        try $ removeDirectoryRecursive installDir
        smartCopySources verbose [buildDir] installDir
            ("Main" : autogenModuleName pkg_descr : otherModules (buildInfo exe)) hugsInstallSuffixes True False
        let targetName = "\"" ++ (targetDir `joinFileName` hugsMainFilename exe) ++ "\""
        -- FIX (HUGS): use extensions, and options from file too?
        -- see http://hackage.haskell.org/trac/hackage/ticket/43
        let hugsOptions = hcOptions Hugs (options (buildInfo exe))
#if mingw32_HOST_OS || mingw32_TARGET_OS
        let exeFile = binDir `joinFileName` exeName exe `joinFileExt` "bat"
        let script = unlines [
                "@echo off",
                unwords ("runhugs" : hugsOptions ++ [targetName, "%*"])]
#else
        let exeFile = binDir `joinFileName` exeName exe
        let script = unlines [
                "#! /bin/sh",
                unwords ("runhugs" : hugsOptions ++ [targetName, "\"$@\""])]
#endif
        writeFile exeFile script
        perms <- getPermissions exeFile
        setPermissions exeFile perms { executable = True, readable = True }

hugsInstallSuffixes :: [String]
hugsInstallSuffixes = ["hs", "lhs", dllExtension]

-- |Hugs library directory for a package
hugsPackageDir :: PackageDescription -> LocalBuildInfo -> FilePath
hugsPackageDir pkg_descr lbi =
    mkLibDir pkg_descr lbi NoCopyDest
	`joinFileName` "packages" `joinFileName` pkgName (package pkg_descr)

-- |Hugs program directories for a package
hugsProgramsDirs :: PackageDescription -> LocalBuildInfo -> [FilePath]
hugsProgramsDirs pkg_descr lbi =
    [exeDir `joinFileName` exeName exe |
         exe <- executables pkg_descr, buildable (buildInfo exe)]
  where exeDir = mkLibDir pkg_descr lbi NoCopyDest `joinFileName` "programs"

-- |Filename used by Hugs for the main module of an executable.
-- This is a simple filename, so that Hugs will look for any auxiliary
-- modules it uses relative to the directory it's in.
hugsMainFilename :: Executable -> FilePath
hugsMainFilename exe = "Main" `joinFileExt` ext
  where (_, ext) = splitFileExt (modulePath exe)

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
