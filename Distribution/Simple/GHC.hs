{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC
-- Copyright   :  Isaac Jones 2003-2006
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--

{- Copyright (c) 2003-2005, Isaac Jones
All rights reserved.

Redistribution and use in source and binary forms, with or without
modiication, are permitted provided that the following conditions are
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

module Distribution.Simple.GHC (
	build, installLib, installExe
 ) where

import Distribution.PackageDescription
				( PackageDescription(..), BuildInfo(..),
				  withLib, setupMessage,
				  Executable(..), withExe, Library(..),
				  libModules, hcOptions )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..), autogenModulesDir,
				  mkLibDir, mkIncludeDir )
import Distribution.Simple.Utils( rawSystemExit, rawSystemPath,
				  rawSystemVerbose, maybeExit, xargs,
				  die, dirOf, moduleToFilePath,
				  smartCopySources, findFile, copyFileVerbose,
                                  mkLibName, mkProfLibName, dotToSep )
import Distribution.Package  	( PackageIdentifier(..), showPackageId )
import Distribution.Program	( rawSystemProgram, ranlibProgram,
				  Program(..), ProgramConfiguration(..),
				  ProgramLocation(..),
				  lookupProgram, arProgram )
import Distribution.Compiler 	( Compiler(..), CompilerFlavor(..),
				  extensionsToGHCFlag )
import Distribution.Version	( Version(..) )
import Distribution.Compat.FilePath
				( joinFileName, exeExtension, joinFileExt,
				  splitFilePath, objExtension, joinPaths,
                                  isAbsolutePath, splitFileExt )
import Distribution.Compat.Directory 
				( createDirectoryIfMissing )
import qualified Distribution.Simple.GHCPackageConfig as GHC
				( localPackageConfig,
				  canReadLocalPackageConfig )
import Language.Haskell.Extension (Extension(..))

import Control.Monad		( unless, when )
import Data.List		( isSuffixOf, nub )
import System.Directory		( removeFile, renameFile,
				  getDirectoryContents, doesFileExist )
import System.Exit              (ExitCode(..))

#ifdef mingw32_HOST_OS
import Distribution.Compat.FilePath ( splitFileName )
#endif

#ifndef __NHC__
import Control.Exception (try)
#else
import IO (try)
#endif

-- -----------------------------------------------------------------------------
-- Building

-- |Building for GHC.  If .ghc-packages exists and is readable, add
-- it to the command-line.
build :: PackageDescription -> LocalBuildInfo -> Int -> IO ()
build pkg_descr lbi verbose = do
  let pref = buildDir lbi
  let ghcPath = compilerPath (compiler lbi)
      ifVanillaLib forceVanilla = when (forceVanilla || withVanillaLib lbi)
      ifProfLib = when (withProfLib lbi)
      ifGHCiLib = when (withGHCiLib lbi)

  -- GHC versions prior to 6.4 didn't have the user package database,
  -- so we fake it.  TODO: This can go away in due course.
  pkg_conf <- if versionBranch (compilerVersion (compiler lbi)) >= [6,4]
		then return []
		else do  pkgConf <- GHC.localPackageConfig
			 pkgConfReadable <- GHC.canReadLocalPackageConfig
			 if pkgConfReadable 
				then return ["-package-conf", pkgConf]
				else return []
	       
  -- Build lib
  withLib pkg_descr () $ \lib -> do
      when (verbose > 3) (putStrLn "Building library...")
      let libBi = libBuildInfo lib
          libTargetDir = pref
	  forceVanillaLib = TemplateHaskell `elem` extensions libBi
	  -- TH always needs vanilla libs, even when building for profiling

      createDirectoryIfMissing True libTargetDir
      -- put hi-boot files into place for mutually recurive modules
      smartCopySources verbose (hsSourceDirs libBi)
                       libTargetDir (libModules pkg_descr) ["hi-boot"] False False
      let ghcArgs = 
                 pkg_conf
              ++ ["-package-name", showPackageId (package pkg_descr) ]
	      ++ (if splitObjs lbi then ["-split-objs"] else [])
              ++ constructGHCCmdLine lbi libBi libTargetDir verbose
              ++ (libModules pkg_descr)
          ghcArgsProf = ghcArgs
              ++ ["-prof",
                  "-hisuf", "p_hi",
                  "-osuf", "p_o"
                 ]
              ++ ghcProfOptions libBi
      unless (null (libModules pkg_descr)) $
        do ifVanillaLib forceVanillaLib (rawSystemExit verbose ghcPath ghcArgs)
           ifProfLib (rawSystemExit verbose ghcPath ghcArgsProf)

      -- build any C sources
      unless (null (cSources libBi)) $ do
         when (verbose > 3) (putStrLn "Building C Sources...")
         -- FIX: similar 'versionBranch' logic duplicated below. refactor for code sharing
         sequence_ [do let ghc_vers = compilerVersion (compiler lbi)
			   odir | versionBranch ghc_vers >= [6,4,1] = pref
				| otherwise = pref `joinFileName` dirOf c
				-- ghc 6.4.1 fixed a bug in -odir handling
				-- for C compilations.
                       createDirectoryIfMissing True odir
		       let cArgs = ["-I" ++ dir | dir <- includeDirs libBi]
			       ++ ["-optc" ++ opt | opt <- ccOptions libBi]
			       ++ ["-odir", odir, "-hidir", pref, "-c"]
			       ++ (if verbose > 4 then ["-v"] else [])
                       rawSystemExit verbose ghcPath (cArgs ++ [c])
                                   | c <- cSources libBi]

      -- link:
      when (verbose > 3) (putStrLn "cabal-linking...")
      let cObjs = [ path `joinFileName` file `joinFileExt` objExtension
                  | (path, file, _) <- (map splitFilePath (cSources libBi)) ]
          libName  = mkLibName pref (showPackageId (package pkg_descr))
          profLibName  = mkProfLibName pref (showPackageId (package pkg_descr))
	  ghciLibName = mkGHCiLibName pref (showPackageId (package pkg_descr))

      stubObjs <- sequence [moduleToFilePath [libTargetDir] (x ++"_stub") [objExtension]
                           |  x <- libModules pkg_descr ]  >>= return . concat
      stubProfObjs <- sequence [moduleToFilePath [libTargetDir] (x ++"_stub") ["p_" ++ objExtension]
                           |  x <- libModules pkg_descr ]  >>= return . concat

      hObjs     <- getHaskellObjects pkg_descr libBi lbi
			pref objExtension
      hProfObjs <- 
	if (withProfLib lbi)
		then getHaskellObjects pkg_descr libBi lbi
			pref ("p_" ++ objExtension)
		else return []

      unless (null hObjs && null cObjs && null stubObjs) $ do
        try (removeFile libName) -- first remove library if it exists
        try (removeFile profLibName) -- first remove library if it exists
	try (removeFile ghciLibName) -- first remove library if it exists
        let arArgs = ["q"++ (if verbose > 4 then "v" else "")]
                ++ [libName]
            arObjArgs =
		   hObjs
                ++ map (pref `joinFileName`) cObjs
                ++ stubObjs
            arProfArgs = ["q"++ (if verbose > 4 then "v" else "")]
                ++ [profLibName]
            arProfObjArgs =
		   hProfObjs
                ++ map (pref `joinFileName`) cObjs
                ++ stubProfObjs
	    ldArgs = ["-r"]
                ++ ["-x"] -- FIXME: only some systems's ld support the "-x" flag
	        ++ ["-o", ghciLibName `joinFileExt` "tmp"]
            ldObjArgs =
		   hObjs
                ++ map (pref `joinFileName`) cObjs
		++ stubObjs

#if defined(mingw32_TARGET_OS) || defined(mingw32_HOST_OS)
            (compilerDir, _) = splitFileName $ compilerPath (compiler lbi)
            (baseDir, _)     = splitFileName compilerDir
            ld = baseDir `joinFileName` "gcc-lib\\ld.exe"
            rawSystemLd = rawSystemVerbose
            maxCommandLineSize = 30 * 1024
#else
            ld = "ld"
            rawSystemLd = rawSystemPath
             --TODO: discover this at configure time on unix
            maxCommandLineSize = 30 * 1024
#endif
            runLd ld args = do
              exists <- doesFileExist ghciLibName
              status <- rawSystemLd verbose ld
                          (args ++ if exists then [ghciLibName] else [])
              when (status == ExitSuccess)
                   (renameFile (ghciLibName `joinFileExt` "tmp") ghciLibName)
              return status

        ifVanillaLib False $ maybeExit $ xargs maxCommandLineSize
          (rawSystemPath verbose) "ar" arArgs arObjArgs

        ifProfLib $ maybeExit $ xargs maxCommandLineSize
          (rawSystemPath verbose) "ar" arProfArgs arProfObjArgs

        ifGHCiLib $ maybeExit $ xargs maxCommandLineSize
          runLd ld ldArgs ldObjArgs

  -- build any executables
  withExe pkg_descr $ \ (Executable exeName' modPath exeBi) -> do
                 when (verbose > 3)
                      (putStrLn $ "Building executable: " ++ exeName' ++ "...")

                 -- exeNameReal, the name that GHC really uses (with .exe on Windows)
                 let exeNameReal = exeName' `joinFileExt`
                                   (if null $ snd $ splitFileExt exeName' then exeExtension else "")

		 let targetDir = pref `joinFileName` exeName'
                 let exeDir = joinPaths targetDir (exeName' ++ "-tmp")
                 createDirectoryIfMissing True targetDir
                 createDirectoryIfMissing True exeDir
                 -- put hi-boot files into place for mutually recursive modules
                 -- FIX: what about exeName.hi-boot?
                 smartCopySources verbose (hsSourceDirs exeBi)
                                  exeDir (otherModules exeBi) ["hi-boot"] False False

                 -- build executables
                 unless (null (cSources exeBi)) $ do
                  when (verbose > 3) (putStrLn "Building C Sources.")
                  sequence_ [do let cSrcODir |versionBranch (compilerVersion (compiler lbi))
                                                    >= [6,4,1] = exeDir
                                             | otherwise 
                                                 = exeDir `joinFileName` (dirOf c)
                                createDirectoryIfMissing True cSrcODir
		                let cArgs = ["-I" ++ dir | dir <- includeDirs exeBi]
			                    ++ ["-optc" ++ opt | opt <- ccOptions exeBi]
			                    ++ ["-odir", cSrcODir, "-hidir", pref, "-c"]
			                    ++ (if verbose > 4 then ["-v"] else [])
                                rawSystemExit verbose ghcPath (cArgs ++ [c])
                                    | c <- cSources exeBi]
                 srcMainFile <- findFile (hsSourceDirs exeBi) modPath

                 let cObjs = [ path `joinFileName` file `joinFileExt` objExtension
                                   | (path, file, _) <- (map splitFilePath (cSources exeBi)) ]
                 let binArgs linkExe profExe =
                            pkg_conf
                         ++ ["-I"++pref]
			 ++ (if linkExe
			        then ["-o", targetDir `joinFileName` exeNameReal]
                                else ["-c"])
                         ++ constructGHCCmdLine lbi exeBi exeDir verbose
                         ++ [exeDir `joinFileName` x | x <- cObjs]
                         ++ [srcMainFile]
			 ++ ldOptions exeBi
			 ++ ["-l"++lib | lib <- extraLibs exeBi]
			 ++ ["-L"++libDir | libDir <- extraLibDirs exeBi]
                         ++ if profExe
                               then "-prof":ghcProfOptions exeBi
                               else []

		 -- For building exe's for profiling that use TH we actually
		 -- have to build twice, once without profiling and the again
		 -- with profiling. This is because the code that TH needs to
		 -- run at compile time needs to be the vanilla ABI so it can
		 -- be loaded up and run by the compiler.
		 when (withProfExe lbi && TemplateHaskell `elem` extensions exeBi)
		    (rawSystemExit verbose ghcPath (binArgs False False))

		 rawSystemExit verbose ghcPath (binArgs True (withProfExe lbi))


-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: PackageDescription -> BuildInfo -> LocalBuildInfo
 	-> FilePath -> String -> IO [FilePath]
getHaskellObjects pkg_descr libBi lbi pref obj_ext
  | splitObjs lbi = do
	let dirs = [ pref `joinFileName` (dotToSep x ++ "_split") 
		   | x <- libModules pkg_descr ]
	objss <- mapM getDirectoryContents dirs
	let objs = [ dir `joinFileName` obj
		   | (objs,dir) <- zip objss dirs, obj <- objs,
		     obj_ext `isSuffixOf` obj ]
	return objs
  | otherwise  = 
	return [ pref `joinFileName` (dotToSep x) `joinFileExt` obj_ext
               | x <- libModules pkg_descr ]


constructGHCCmdLine
	:: LocalBuildInfo
        -> BuildInfo
	-> FilePath
	-> Int				-- verbosity level
        -> [String]
constructGHCCmdLine lbi bi odir verbose = 
        ["--make"]
     ++ (if verbose > 4 then ["-v"] else [])
	    -- Unsupported extensions have already been checked by configure
     ++ (if compilerVersion (compiler lbi) > Version [6,4] []
            then ["-hide-all-packages"]
            else [])
     ++ ["-i"]
     ++ ["-i" ++ autogenModulesDir lbi]
     ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
     ++ ["-I" ++ dir | dir <- includeDirs bi]
     ++ ["-optc" ++ opt | opt <- ccOptions bi]
     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes bi ++ installIncludes bi ]
     ++ [ "-odir",  odir, "-hidir", odir ]
     ++ (concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ])
     ++ hcOptions GHC (options bi)
     ++ snd (extensionsToGHCFlag (extensions bi))

mkGHCiLibName :: FilePath -- ^file Prefix
              -> String   -- ^library name.
              -> String
mkGHCiLibName pref lib = pref `joinFileName` ("HS" ++ lib ++ ".o")

-- -----------------------------------------------------------------------------
-- Installing

-- |Install executables for GHC.
installExe :: Int      -- ^verbose
              -> FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installExe verbose pref buildPref pkg_descr
    = do createDirectoryIfMissing True pref
         withExe pkg_descr $ \ (Executable e _ b) -> do
             let exeName = e `joinFileExt` exeExtension
             copyFileVerbose verbose (buildPref `joinFileName` e `joinFileName` exeName) (pref `joinFileName` exeName)

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Int      -- ^verbose
              -> ProgramConfiguration
              -> Bool     -- ^has vanilla library
              -> Bool     -- ^has profiling library
	      -> Bool     -- ^has GHCi libs
              -> FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installLib verbose programConf hasVanilla hasProf hasGHCi pref buildPref
              pd@PackageDescription{library=Just l,
                                    package=p}
    = do ifVanilla $ smartCopySources verbose [buildPref] pref (libModules pd) ["hi"] True False
         ifProf $ smartCopySources verbose [buildPref] pref (libModules pd) ["p_hi"] True False
         let libTargetLoc = mkLibName pref (showPackageId p)
             profLibTargetLoc = mkProfLibName pref (showPackageId p)
	     libGHCiTargetLoc = mkGHCiLibName pref (showPackageId p)
         ifVanilla $ copyFileVerbose verbose (mkLibName buildPref (showPackageId p)) libTargetLoc
         ifProf $ copyFileVerbose verbose (mkProfLibName buildPref (showPackageId p)) profLibTargetLoc
	 ifGHCi $ copyFileVerbose verbose (mkGHCiLibName buildPref (showPackageId p)) libGHCiTargetLoc

	 installIncludeFiles verbose pd pref

         -- use ranlib or ar -s to build an index. this is necessary
         -- on some systems like MacOS X.  If we can't find those,
         -- don't worry too much about it.
         let progName = programName $ ranlibProgram
         mProg <- lookupProgram progName programConf
         case foundProg mProg of
           Just rl  -> do ifVanilla $ rawSystemProgram verbose rl [libTargetLoc]
                          ifProf $ rawSystemProgram verbose rl [profLibTargetLoc]

           Nothing -> do let progName = programName $ arProgram
                         mProg <- lookupProgram progName programConf
                         case mProg of
                          Just ar  -> do ifVanilla $ rawSystemProgram verbose ar ["-s", libTargetLoc]
                                         ifProf $ rawSystemProgram verbose ar ["-s", profLibTargetLoc]
                          Nothing -> setupMessage  "Warning: Unable to generate index for library (missing ranlib and ar)" pd
         return ()
    where ifVanilla action = when hasVanilla (action >> return ())
          ifProf action = when hasProf (action >> return ())
	  ifGHCi action = when hasGHCi (action >> return ())
installLib _ _ _ _ _ _ _ PackageDescription{library=Nothing}
    = die $ "Internal Error. installLibGHC called with no library."

-- | Install the files listed in install-includes
installIncludeFiles :: Int -> PackageDescription -> FilePath -> IO ()
installIncludeFiles verbose pkg_descr@PackageDescription{library=Just l} libdir
 = do
   createDirectoryIfMissing True incdir
   incs <- mapM (findInc relincdirs) (installIncludes lbi)
   sequence_ [ copyFileVerbose verbose path (incdir `joinFileName` f)
	     | (f,path) <- incs ]
  where
   relincdirs = filter (not.isAbsolutePath) (includeDirs lbi)
   lbi = libBuildInfo l
   incdir = mkIncludeDir libdir

   findInc [] f = die ("can't find include file " ++ f)
   findInc (d:ds) f = do
     let path = (d `joinFileName` f)
     b <- doesFileExist path
     if b then return (f,path) else findInc ds f

-- Also checks whether the program was actually found.
foundProg :: Maybe Program -> Maybe Program
foundProg Nothing = Nothing
foundProg (Just Program{programLocation=EmptyLocation}) = Nothing
foundProg x = x
