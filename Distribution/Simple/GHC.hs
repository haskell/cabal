{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC
-- Copyright   :  Isaac Jones 2003-2007
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Build and Install implementations for GHC.  See
-- 'Distribution.Simple.GHCPackageConfig.GHCPackageConfig' for
-- registration-related stuff.

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
	build, makefile, installLib, installExe
 ) where

import Distribution.Setup       ( MakefileFlags(..) )
import Distribution.PackageDescription
				( PackageDescription(..), BuildInfo(..),
				  withLib, setupMessage,
				  Executable(..), withExe, Library(..),
				  libModules, hcOptions )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..), autogenModulesDir )
import Distribution.Simple.Utils( rawSystemExit, rawSystemPathExit,
                  xargs, die, dirOf, moduleToFilePath,
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
import Distribution.Verbosity
import Language.Haskell.Extension (Extension(..))

import Control.Monad		( unless, when )
import Data.List		( nub )
import System.Directory		( removeFile, renameFile,
				  getDirectoryContents, doesFileExist )
import System.IO

#ifdef mingw32_HOST_OS
import Distribution.Compat.FilePath ( splitFileName )
#endif

-- System.IO used to export a different try, so we can't use try unqualified
#ifndef __NHC__
import Control.Exception as Try
#else
import IO as Try
#endif

-- -----------------------------------------------------------------------------
-- Building

-- |Building for GHC.  If .ghc-packages exists and is readable, add
-- it to the command-line.
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
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
      when (verbosity >= verbose) (putStrLn "Building library...")
      let libBi = libBuildInfo lib
          libTargetDir = pref
	  forceVanillaLib = TemplateHaskell `elem` extensions libBi
	  -- TH always needs vanilla libs, even when building for profiling

      createDirectoryIfMissing True libTargetDir
      -- put hi-boot files into place for mutually recurive modules
      smartCopySources verbosity (hsSourceDirs libBi)
                       libTargetDir (libModules pkg_descr) ["hi-boot"] False False
      let ghc_vers = compilerVersion (compiler lbi)
          packageId | versionBranch ghc_vers >= [6,4]
                                = showPackageId (package pkg_descr)
                    | otherwise = pkgName (package pkg_descr)
          -- Only use the version number with ghc-6.4 and later
          ghcArgs =
                 pkg_conf
              ++ ["-package-name", packageId ]
	      ++ (if splitObjs lbi then ["-split-objs"] else [])
              ++ constructGHCCmdLine lbi libBi libTargetDir verbosity
              ++ (libModules pkg_descr)
          ghcArgsProf = ghcArgs
              ++ ["-prof",
                  "-hisuf", "p_hi",
                  "-osuf", "p_o"
                 ]
              ++ ghcProfOptions libBi
      unless (null (libModules pkg_descr)) $
        do ifVanillaLib forceVanillaLib (rawSystemExit verbosity ghcPath ghcArgs)
           ifProfLib (rawSystemExit verbosity ghcPath ghcArgsProf)

      -- build any C sources
      unless (null (cSources libBi)) $ do
         when (verbosity >= verbose) (putStrLn "Building C Sources...")
         -- FIX: similar 'versionBranch' logic duplicated below. refactor for code sharing
         sequence_ [do let odir | versionBranch ghc_vers >= [6,4,1] = pref
				| otherwise = pref `joinFileName` dirOf c
				-- ghc 6.4.1 fixed a bug in -odir handling
				-- for C compilations.
                       createDirectoryIfMissing True odir
		       let cArgs = ["-I" ++ dir | dir <- includeDirs libBi]
			       ++ ["-optc" ++ opt | opt <- ccOptions libBi]
			       ++ ["-odir", odir, "-hidir", pref, "-c"]
			       ++ (if verbosity >= deafening then ["-v"] else [])
                       rawSystemExit verbosity ghcPath (cArgs ++ [c])
                                   | c <- cSources libBi]

      -- link:
      when (verbosity > verbose) (putStrLn "cabal-linking...")
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
        Try.try (removeFile libName) -- first remove library if it exists
        Try.try (removeFile profLibName) -- first remove library if it exists
	Try.try (removeFile ghciLibName) -- first remove library if it exists
        let arArgs = ["q"++ (if verbosity >= deafening then "v" else "")]
                ++ [libName]
            arObjArgs =
		   hObjs
                ++ map (pref `joinFileName`) cObjs
                ++ stubObjs
            arProfArgs = ["q"++ (if verbosity >= deafening then "v" else "")]
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

            runLd ld args = do
              exists <- doesFileExist ghciLibName
              rawSystemLd verbosity ld
                          (args ++ if exists then [ghciLibName] else [])
              renameFile (ghciLibName `joinFileExt` "tmp") ghciLibName

#if defined(mingw32_TARGET_OS) || defined(mingw32_HOST_OS)
            rawSystemLd = rawSystemExit
            maxCommandLineSize = 30 * 1024
#else
            rawSystemLd = rawSystemPathExit
             --TODO: discover this at configure time on unix
            maxCommandLineSize = 30 * 1024
#endif
        ld <- 
#if defined(mingw32_TARGET_OS) || defined(mingw32_HOST_OS)
              let
               (compilerDir, _) = splitFileName $ compilerPath (compiler lbi)
               (baseDir, _)     = splitFileName compilerDir
               binInstallLd     = baseDir `joinFileName` "gcc-lib\\ld.exe"
	      in do
              mb <- lookupProgram "ld" (withPrograms lbi)
	      case fmap programLocation mb of
	       Just (UserSpecified s) -> return s
		     -- assume we're using an installed copy of GHC..
	       _ -> return binInstallLd
#else
            return "ld"
#endif
        mbAr <- lookupProgram "ar" (withPrograms lbi) 
	let arProg = case fmap programLocation mbAr of { Just (UserSpecified x) -> x ; _ -> "ar" }
        ifVanillaLib False $ xargs maxCommandLineSize
          (rawSystemPathExit verbosity) arProg arArgs arObjArgs

        ifProfLib $ xargs maxCommandLineSize
          (rawSystemPathExit verbosity) arProg arProfArgs arProfObjArgs

        ifGHCiLib $ xargs maxCommandLineSize
          runLd ld ldArgs ldObjArgs

  -- build any executables
  withExe pkg_descr $ \ (Executable exeName' modPath exeBi) -> do
                 when (verbosity >= verbose)
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
                 smartCopySources verbosity (hsSourceDirs exeBi)
                                  exeDir (otherModules exeBi) ["hi-boot"] False False

                 -- build executables
                 unless (null (cSources exeBi)) $ do
                  when (verbosity >= verbose) (putStrLn "Building C Sources.")
                  sequence_ [do let cSrcODir |versionBranch (compilerVersion (compiler lbi))
                                                    >= [6,4,1] = exeDir
                                             | otherwise 
                                                 = exeDir `joinFileName` (dirOf c)
                                createDirectoryIfMissing True cSrcODir
		                let cArgs = ["-I" ++ dir | dir <- includeDirs exeBi]
			                    ++ ["-optc" ++ opt | opt <- ccOptions exeBi]
			                    ++ ["-odir", cSrcODir, "-hidir", pref, "-c"]
			                    ++ (if verbosity >= deafening then ["-v"] else [])
                                rawSystemExit verbosity ghcPath (cArgs ++ [c])
                                    | c <- cSources exeBi]
                 srcMainFile <- findFile (hsSourceDirs exeBi) modPath

                 let cObjs = [ path `joinFileName` file `joinFileExt` objExtension
                                   | (path, file, _) <- (map splitFilePath (cSources exeBi)) ]
                 let binArgs linkExe profExe =
                            pkg_conf
			 ++ (if linkExe
			        then ["-o", targetDir `joinFileName` exeNameReal]
                                else ["-c"])
                         ++ constructGHCCmdLine lbi exeBi exeDir verbosity
                         ++ [exeDir `joinFileName` x | x <- cObjs]
                         ++ [srcMainFile]
			 ++ ldOptions exeBi
			 ++ ["-l"++lib | lib <- extraLibs exeBi]
			 ++ ["-L"++libDir | libDir <- extraLibDirs exeBi]
                         ++ if profExe
                               then ["-prof",
                                     "-hisuf", "p_hi",
                                     "-osuf", "p_o"
                                    ] ++ ghcProfOptions exeBi
                               else []

		 -- For building exe's for profiling that use TH we actually
		 -- have to build twice, once without profiling and the again
		 -- with profiling. This is because the code that TH needs to
		 -- run at compile time needs to be the vanilla ABI so it can
		 -- be loaded up and run by the compiler.
		 when (withProfExe lbi && TemplateHaskell `elem` extensions exeBi)
		    (rawSystemExit verbosity ghcPath (binArgs False False))

		 rawSystemExit verbosity ghcPath (binArgs True (withProfExe lbi))


-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: PackageDescription -> BuildInfo -> LocalBuildInfo
 	-> FilePath -> String -> IO [FilePath]
getHaskellObjects pkg_descr _ lbi pref wanted_obj_ext
  | splitObjs lbi = do
	let dirs = [ pref `joinFileName` (dotToSep x ++ "_split") 
		   | x <- libModules pkg_descr ]
	objss <- mapM getDirectoryContents dirs
	let objs = [ dir `joinFileName` obj
		   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let (_,obj_ext) = splitFileExt obj,
		     wanted_obj_ext == obj_ext ]
	return objs
  | otherwise  = 
	return [ pref `joinFileName` (dotToSep x) `joinFileExt` wanted_obj_ext
               | x <- libModules pkg_descr ]


constructGHCCmdLine
        :: LocalBuildInfo
        -> BuildInfo
        -> FilePath
        -> Verbosity
        -> [String]
constructGHCCmdLine lbi bi odir verbosity =
        ["--make"]
     ++ (     if verbosity >= deafening then ["-v"]
         else if verbosity >= normal    then []
         else                                ["-w", "-v0"])
        -- Unsupported extensions have already been checked by configure
     ++ ghcOptions lbi bi odir

ghcOptions :: LocalBuildInfo -> BuildInfo -> FilePath -> [String]
ghcOptions lbi bi odir
     =  (if compilerVersion (compiler lbi) > Version [6,4] []
            then ["-hide-all-packages"]
            else [])
     ++ ["-i"]
     ++ ["-i" ++ autogenModulesDir lbi]
     ++ ["-i" ++ buildDir lbi]
     ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
     ++ ["-I" ++ buildDir lbi]
     ++ ["-I" ++ dir | dir <- includeDirs bi]
     ++ ["-optc" ++ opt | opt <- ccOptions bi]
     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes bi ]
     ++ [ "-odir",  odir, "-hidir", odir ]
     ++ (concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ])
     ++ (if withOptimization lbi then ["-O"] else [])
     ++ hcOptions GHC (options bi)
     ++ snd (extensionsToGHCFlag (extensions bi))

mkGHCiLibName :: FilePath -- ^file Prefix
              -> String   -- ^library name.
              -> String
mkGHCiLibName pref lib = pref `joinFileName` ("HS" ++ lib ++ ".o")

-- -----------------------------------------------------------------------------
-- Building a Makefile

makefile :: PackageDescription -> LocalBuildInfo -> MakefileFlags -> IO ()
makefile pkg_descr lbi flags = do
  let file = case makefileFile flags of
                Just f ->  f
                _otherwise -> "Makefile"
  targetExists <- doesFileExist file
  when targetExists $
    die ("Not overwriting existing copy of " ++ file)
  h <- openFile file WriteMode

  let Just lib = library pkg_descr
      bi = libBuildInfo lib
  
      ghc_vers = compilerVersion (compiler lbi)
      packageId | versionBranch ghc_vers >= [6,4]
                                = showPackageId (package pkg_descr)
                 | otherwise = pkgName (package pkg_descr)
  let decls = [
        ("modules", unwords (exposedModules lib ++ otherModules bi)),
        ("GHC", compilerPath (compiler lbi)),
        ("WAYS", if withProfLib lbi then "p" else ""),
        ("odir", buildDir lbi),
        ("srcdir", case hsSourceDirs bi of
                        [one] -> one
                        _     -> error "makefile: can't cope with multiple hs-source-dirs yet, sorry"),
        ("package", packageId),
        ("GHC_OPTS", unwords ( 
                           ["-package-name", packageId ]
	                ++ (if splitObjs lbi then ["-split-objs"] else [])
                        ++ ghcOptions lbi bi (buildDir lbi))),
        ("MAKEFILE", file)
        ]
  hPutStrLn h (unlines (map (\(a,b)-> a ++ " = " ++ munge b) decls))
  hPutStrLn h makefileTemplate
  hClose h
 where
  munge "" = ""
  munge ('#':s) = '\\':'#':munge s
  munge (c:s) = c : munge s

-- -----------------------------------------------------------------------------
-- Installing

-- |Install executables for GHC.
installExe :: Verbosity -- ^verbosity
           -> FilePath  -- ^install location
           -> FilePath  -- ^Build location
           -> PackageDescription
           -> IO ()
installExe verbosity pref buildPref pkg_descr
    = do createDirectoryIfMissing True pref
         withExe pkg_descr $ \ (Executable e _ _) -> do
             let exeFileName = e `joinFileExt` exeExtension
             copyFileVerbose verbosity (buildPref `joinFileName` e `joinFileName` exeFileName) (pref `joinFileName` exeFileName)

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity -- ^verbosity
              -> ProgramConfiguration
              -> Bool      -- ^has vanilla library
              -> Bool      -- ^has profiling library
              -> Bool      -- ^has GHCi libs
              -> FilePath  -- ^install location
              -> FilePath  -- ^Build location
              -> PackageDescription -> IO ()
installLib verbosity programConf hasVanilla hasProf hasGHCi pref buildPref
              pd@PackageDescription{library=Just _,
                                    package=p}
    = do ifVanilla $ smartCopySources verbosity [buildPref] pref (libModules pd) ["hi"] True False
         ifProf $ smartCopySources verbosity [buildPref] pref (libModules pd) ["p_hi"] True False
         let libTargetLoc = mkLibName pref (showPackageId p)
             profLibTargetLoc = mkProfLibName pref (showPackageId p)
	     libGHCiTargetLoc = mkGHCiLibName pref (showPackageId p)
         ifVanilla $ copyFileVerbose verbosity (mkLibName buildPref (showPackageId p)) libTargetLoc
         ifProf $ copyFileVerbose verbosity (mkProfLibName buildPref (showPackageId p)) profLibTargetLoc
	 ifGHCi $ copyFileVerbose verbosity (mkGHCiLibName buildPref (showPackageId p)) libGHCiTargetLoc

         -- use ranlib or ar -s to build an index. this is necessary
         -- on some systems like MacOS X.  If we can't find those,
         -- don't worry too much about it.
         let ranlibProgName = programName $ ranlibProgram
         mRanlibProg <- lookupProgram ranlibProgName programConf
         case foundProg mRanlibProg of
           Just rl  -> do ifVanilla $ rawSystemProgram verbosity rl [libTargetLoc]
                          ifProf $ rawSystemProgram verbosity rl [profLibTargetLoc]

           Nothing -> do let arProgName = programName $ arProgram
                         mArProg <- lookupProgram arProgName programConf
                         case mArProg of
                          Just ar  -> do ifVanilla $ rawSystemProgram verbosity ar ["-s", libTargetLoc]
                                         ifProf $ rawSystemProgram verbosity ar ["-s", profLibTargetLoc]
                          Nothing -> setupMessage verbosity "Warning: Unable to generate index for library (missing ranlib and ar)" pd
         return ()
    where ifVanilla action = when hasVanilla (action >> return ())
          ifProf action = when hasProf (action >> return ())
	  ifGHCi action = when hasGHCi (action >> return ())
installLib _ _ _ _ _ _ _ PackageDescription{library=Nothing}
    = die $ "Internal Error. installLibGHC called with no library."

-- Also checks whether the program was actually found.
foundProg :: Maybe Program -> Maybe Program
foundProg Nothing = Nothing
foundProg (Just Program{programLocation=EmptyLocation}) = Nothing
foundProg x = x

-- -----------------------------------------------------------------------------
-- Makefile template

makefileTemplate :: String
makefileTemplate =
 "GHC_OPTS += -i$(odir)\n"++
 "\n"++
 "# For adding options on the command-line\n"++
 "GHC_OPTS += $(EXTRA_HC_OPTS)\n"++
 "\n"++
 "WAY_p_OPTS = -prof\n"++
 "\n"++
 "ifneq \"$(way)\" \"\"\n"++
 "way_ := $(way)_\n"++
 "_way := _$(way)\n"++
 "GHC_OPTS += $(WAY_$(way)_OPTS)\n"++
 "GHC_OPTS += -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(way_)o\n"++
 "endif\n"++
 "\n"++
 "OBJS = $(patsubst %,$(odir)/%.$(way_)o,$(subst .,/,$(modules)))\n"++
 "\n"++
 "all :: .depend $(OBJS)\n"++
 "\n"++
 ".depend : $(MAKEFILE)\n"++
 "\t$(GHC) -M -optdep-f -optdep.depend $(foreach way,$(WAYS),-optdep-s -optdep$(way)) $(foreach obj,$(MKDEPENDHS_OBJ_SUFFICES),-osuf $(obj)) $(filter-out -split-objs, $(GHC_OPTS)) $(modules)\n"++
 "\tfor dir in $(sort $(foreach mod,$(OBJS),$(dir $(mod)))); do \\\n"++
 "\t\tif test ! -d $$dir; then mkdir -p $$dir; fi \\\n"++
 "\tdone\n"++
 "\n"++
 "include .depend\n"++
 "\n"++
 "# suffix rules\n"++
 "\n"++
 "ifneq \"$(odir)\" \"\"\n"++
 "odir_ = $(odir)/\n"++
 "else\n"++
 "odir_ =\n"++
 "endif\n"++
 "\n"++
 "$(odir_)%.$(way_)o : $(srcdir)/%.hs\n"++
 "\t$(GHC) $(GHC_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi\n"++
 "\n"++
 "$(odir_)%.$(way_)o : $(srcdir)/%.lhs\t \n"++
 "\t$(GHC) $(GHC_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi\n"++
 "\n"++
 "$(odir_)%.$(way_)o : $(srcdir)/%.c\n"++
 "\t@$(RM) $@\n"++
 "\t$(GHC) $(GHC_CC_OPTS) -c $< -o $@\n"++
 "\n"++
 "$(odir_)%.$(way_)o : $(srcdir)/%.$(way_)s\n"++
 "\t@$(RM) $@\n"++
 "\t$(GHC) $(GHC_CC_OPTS) -c $< -o $@\n"++
 "\n"++
 "$(odir_)%.$(way_)o : $(srcdir)/%.S\n"++
 "\t@$(RM) $@\n"++
 "\t$(GHC) $(GHC_CC_OPTS) -c $< -o $@\n"++
 "\n"++
 "$(odir_)%.$(way_)s : $(srcdir)/%.c\n"++
 "\t@$(RM) $@\n"++
 "\t$(GHC) $(GHC_CC_OPTS) -S $< -o $@\n"++
 "\n"++
 "$(odir_)%.$(way_)o-boot : $(srcdir)/%.hs-boot\n"++
 "\t$(GHC) $(GHC_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi-boot\n"++
 "\n"++
 "$(odir_)%.$(way_)o-boot : $(srcdir)/%.lhs-boot\n"++
 "\t$(GHC) $(GHC_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi-boot\n"++
 "\n"++
 "%.$(way_)hi : %.$(way_)o\n"++
 "\t@if [ ! -f $@ ] ; then \\\n"++
 "\t    echo Panic! $< exists, but $@ does not.; \\\n"++
 "\t    exit 1; \\\n"++
 "\telse exit 0 ; \\\n"++
 "\tfi\n"++
 "\n"++
 "%.$(way_)hi-boot : %.$(way_)o-boot\n"++
 "\t@if [ ! -f $@ ] ; then \\\n"++
 "\t    echo Panic! $< exists, but $@ does not.; \\\n"++
 "\t    exit 1; \\\n"++
 "\telse exit 0 ; \\\n"++
 "\tfi\n"++
 "\n"++
 "$(odir_)%.$(way_)hi : %.$(way_)hc\n"++
 "\t@if [ ! -f $@ ] ; then \\\n"++
 "\t    echo Panic! $< exists, but $@ does not.; \\\n"++
 "\t    exit 1; \\\n"++
 "\telse exit 0 ; \\\n"++
 "\tfi\n"++
 "\n"++
 "show:\n"++
 "\t@echo '$(VALUE)=\"$($(VALUE))\"'\n"++
 "\n"++
 "\n"++
 "ifneq \"$(strip $(WAYS))\" \"\"\n"++
 "ifeq \"$(way)\" \"\"\n"++
 "all ::\n"++
 "# Don't rely on -e working, instead we check exit return codes from sub-makes.\n"++
 "\t@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \\\n"++
 "\tfor i in $(WAYS) ; do \\\n"++
 "\t  echo \"== $(MAKE) way=$$i -f $(MAKEFILE) $@;\"; \\\n"++
 "\t  $(MAKE) way=$$i -f $(MAKEFILE) --no-print-directory $(MFLAGS) $@ ; \\\n"++
 "\t  if [ $$? -eq 0 ] ; then true; else exit $$x_on_err; fi; \\\n"++
 "\tdone\n"++
 "\t@echo \"== Finished recursively making \\`$@' for ways: $(WAYS) ...\"\n"++
 "endif\n"++
 "endif\n"++
 "\n"++
 "# We could consider adding this: the idea would be to have 'make' do\n"++
 "# everything that 'setup build' does.\n"++
 "# ifeq \"$(way)\" \"\"\n"++
 "# all ::\n"++
 "# \t./Setup build\n"++
 "# endif\n"
