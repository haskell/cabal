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
	configure, build, makefile, installLib, installExe
 ) where

import Distribution.Simple.GHCMakefile
import Distribution.Setup       ( MakefileFlags(..) )
import Distribution.PackageDescription
				( PackageDescription(..), BuildInfo(..),
				  withLib, setupMessage,
				  Executable(..), withExe, Library(..),
				  libModules, hcOptions )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..), autogenModulesDir )
import Distribution.Simple.Utils( createDirectoryIfMissingVerbose,
                                  rawSystemExit, rawSystemPathExit,
                                  xargs, die, moduleToFilePath,
				  smartCopySources, findFile, copyFileVerbose,
                                  mkLibName, mkProfLibName, dotToSep,
                                  exeExtension, objExtension)
import Distribution.Package  	( PackageIdentifier(..), showPackageId )
import Distribution.Program	( rawSystemProgram, rawSystemProgramConf,
				  Program(..), ProgramConfiguration(..),
				  ProgramLocation(..), programPath,
				  findProgram, findProgramAndVersion,
				  simpleProgramAt, lookupProgram,
 				  arProgram, ranlibProgram )
import Distribution.Compiler 	( CompilerFlavor(..),
				  Compiler(..), extensionsToGHCFlag,
                                  compilerPath, compilerVersion )
import Distribution.Version	( Version(..) )
import qualified Distribution.Simple.GHCPackageConfig as GHC
				( localPackageConfig,
				  canReadLocalPackageConfig )
import Distribution.Verbosity
import Language.Haskell.Extension (Extension(..))

import Control.Monad		( unless, when )
import Data.List		( nub )
import System.Directory		( removeFile, renameFile,
				  getDirectoryContents, doesFileExist )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension, splitExtension )
import System.IO

-- System.IO used to export a different try, so we can't use try unqualified
#ifndef __NHC__
import Control.Exception as Try
#else
import IO as Try
#endif

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Maybe FilePath -> Maybe FilePath -> Verbosity -> IO Compiler
configure hcPath hcPkgPath verbosity = do
  
  -- find ghc and version number
  ghcProg <- findProgramAndVersion verbosity "ghc" hcPath "--numeric-version" id

  -- find ghc-pkg
  ghcPkgProg <- case hcPkgPath of
                      Just _  -> findProgram verbosity "ghc" hcPkgPath
                      Nothing -> guessGhcPkgFromGhcPath verbosity ghcProg
  -- TODO: santity check: the versions of ghc-pkg and ghc should be the same.
  
  let Just version = programVersion ghcProg
  return Compiler {
        compilerFlavor  = GHC,
        compilerId      = PackageIdentifier "ghc" version,
        compilerProg    = ghcProg,
        compilerPkgTool = ghcPkgProg
    }

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding ghc-pkg, we try looking for both a versioned and unversioned
-- ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
--
guessGhcPkgFromGhcPath :: Verbosity -> Program -> IO Program
guessGhcPkgFromGhcPath verbosity ghcProg
  = do let path            = programPath ghcProg
           dir             = takeDirectory path
           versionSuffix   = takeVersionSuffix (dropExeExtension path)
           guessNormal     = dir </> "ghc-pkg" <.> exeExtension
           guessVersioned  = dir </> ("ghc-pkg" ++ versionSuffix) <.> exeExtension 
           guesses | null versionSuffix = [guessNormal]
                   | otherwise          = [guessVersioned, guessNormal]
       when (verbosity >= verbose) $
         putStrLn $ "looking for package tool: ghc-pkg near compiler in " ++ dir
       exists <- mapM doesFileExist guesses
       case [ file | (file, True) <- zip guesses exists ] of
         [] -> die "Cannot find package tool: ghc-pkg"
         (pkgtool:_) -> do when (verbosity >= verbose) $
                             putStrLn $ "found package tool in " ++ pkgtool
                           return (simpleProgramAt "ghc-pkg" (FoundOnSystem pkgtool))

  where takeVersionSuffix :: FilePath -> String
        takeVersionSuffix = reverse . takeWhile (`elem ` "0123456789.-") . reverse

        dropExeExtension :: FilePath -> FilePath
        dropExeExtension filepath =
          case splitExtension filepath of
            (filepath', extension) | extension == exeExtension -> filepath'
                                   | otherwise                 -> filepath

-- -----------------------------------------------------------------------------
-- Building

-- |Building for GHC.  If .ghc-packages exists and is readable, add
-- it to the command-line.
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
  let pref = buildDir lbi
      ghcProg = compilerProg (compiler lbi)
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

      createDirectoryIfMissingVerbose verbosity True libTargetDir
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
              ++ constructGHCCmdLine lbi libBi libTargetDir verbosity
              ++ (libModules pkg_descr)
          ghcArgsProf = ghcArgs
              ++ ["-prof",
                  "-hisuf", "p_hi",
                  "-osuf", "p_o"
                 ]
              ++ ghcProfOptions libBi
      unless (null (libModules pkg_descr)) $
        do ifVanillaLib forceVanillaLib (rawSystemProgram verbosity ghcProg ghcArgs)
           ifProfLib (rawSystemProgram verbosity ghcProg ghcArgsProf)

      -- build any C sources
      unless (null (cSources libBi)) $ do
         when (verbosity >= verbose) (putStrLn "Building C Sources...")
         sequence_ [do let (odir,args) = constructCcCmdLine lbi libBi pref 
                                                            filename verbosity
                       createDirectoryIfMissingVerbose verbosity True odir
                       rawSystemProgram verbosity ghcProg args
                   | filename <- cSources libBi]

      -- link:
      when (verbosity > verbose) (putStrLn "cabal-linking...")
      let cObjs = map (`replaceExtension` objExtension) (cSources libBi)
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

        ld <- findLdProgram lbi
        let arArgs = ["q"++ (if verbosity >= deafening then "v" else "")]
                ++ [libName]
            arObjArgs =
		   hObjs
                ++ map (pref </>) cObjs
                ++ stubObjs
            arProfArgs = ["q"++ (if verbosity >= deafening then "v" else "")]
                ++ [profLibName]
            arProfObjArgs =
		   hProfObjs
                ++ map (pref </>) cObjs
                ++ stubProfObjs
	    ldArgs = ["-r"]
                ++ ["-x"] -- FIXME: only some systems's ld support the "-x" flag
	        ++ ["-o", ghciLibName <.> "tmp"]
            ldObjArgs =
		   hObjs
                ++ map (pref </>) cObjs
		++ stubObjs

            runLd args = do
              exists <- doesFileExist ghciLibName
                -- SDM: we always remove ghciLibName above, so isn't this
                -- always False?  What is this stuff for anyway?
              rawSystemLd verbosity ld
                          (args ++ if exists then [ghciLibName] else [])
              renameFile (ghciLibName <.> "tmp") ghciLibName

            runAr = rawSystemProgramConf verbosity "ar" (withPrograms lbi)

#if defined(mingw32_TARGET_OS) || defined(mingw32_HOST_OS)
            rawSystemLd = rawSystemExit
            maxCommandLineSize = 30 * 1024
#else
            rawSystemLd = rawSystemPathExit
             --TODO: discover this at configure time on unix
            maxCommandLineSize = 30 * 1024
#endif

        ifVanillaLib False $ xargs maxCommandLineSize
          runAr arArgs arObjArgs

        ifProfLib $ xargs maxCommandLineSize
          runAr arProfArgs arProfObjArgs

        ifGHCiLib $ xargs maxCommandLineSize
          runLd ldArgs ldObjArgs

  -- build any executables
  withExe pkg_descr $ \ (Executable exeName' modPath exeBi) -> do
                 when (verbosity >= verbose)
                      (putStrLn $ "Building executable: " ++ exeName' ++ "...")

                 -- exeNameReal, the name that GHC really uses (with .exe on Windows)
                 let exeNameReal = exeName' <.>
                                   (if null $ takeExtension exeName' then exeExtension else "")

		 let targetDir = pref </> exeName'
                 let exeDir    = targetDir </> (exeName' ++ "-tmp")
                 createDirectoryIfMissingVerbose verbosity True targetDir
                 createDirectoryIfMissingVerbose verbosity True exeDir
                 -- put hi-boot files into place for mutually recursive modules
                 -- FIX: what about exeName.hi-boot?
                 smartCopySources verbosity (hsSourceDirs exeBi)
                                  exeDir (otherModules exeBi) ["hi-boot"] False False

                 -- build executables
                 unless (null (cSources exeBi)) $ do
                  when (verbosity >= verbose) (putStrLn "Building C Sources.")
		  sequence_ [do let (odir,args) = constructCcCmdLine lbi exeBi
                                                         exeDir filename verbosity
                                createDirectoryIfMissingVerbose verbosity True odir
                                rawSystemProgram verbosity ghcProg args
                            | filename <- cSources exeBi]

                 srcMainFile <- findFile (hsSourceDirs exeBi) modPath

                 let cObjs = map (`replaceExtension` objExtension) (cSources exeBi)
                 let binArgs linkExe profExe =
                            pkg_conf
			 ++ (if linkExe
			        then ["-o", targetDir </> exeNameReal]
                                else ["-c"])
                         ++ constructGHCCmdLine lbi exeBi exeDir verbosity
                         ++ [exeDir </> x | x <- cObjs]
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
		    (rawSystemProgram verbosity ghcProg (binArgs False False))

		 rawSystemProgram verbosity ghcProg (binArgs True (withProfExe lbi))


-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: PackageDescription -> BuildInfo -> LocalBuildInfo
 	-> FilePath -> String -> IO [FilePath]
getHaskellObjects pkg_descr _ lbi pref wanted_obj_ext
  | splitObjs lbi = do
	let dirs = [ pref </> (dotToSep x ++ "_split") 
		   | x <- libModules pkg_descr ]
	objss <- mapM getDirectoryContents dirs
	let objs = [ dir </> obj
		   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
		     '.':wanted_obj_ext == obj_ext ]
	return objs
  | otherwise  = 
	return [ pref </> dotToSep x <.> wanted_obj_ext
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
     =  (if version > Version [6,4] []
            then ["-hide-all-packages"]
            else [])
     ++ (if splitObjs lbi then ["-split-objs"] else [])
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
     ++ snd (extensionsToGHCFlag version (extensions bi))
    where version = compilerVersion (compiler lbi)

constructCcCmdLine :: LocalBuildInfo -> BuildInfo -> FilePath
                   -> FilePath -> Verbosity -> (FilePath,[String])
constructCcCmdLine lbi bi pref filename verbosity
  =  let odir | compilerVersion (compiler lbi) >= Version [6,4,1] []  = pref
              | otherwise = pref </> takeDirectory filename
			-- ghc 6.4.1 fixed a bug in -odir handling
			-- for C compilations.
     in 
        (odir,
         ghcCcOptions lbi bi odir
         ++ (if verbosity > deafening then ["-v"] else [])
         ++ ["-c",filename])
         

ghcCcOptions :: LocalBuildInfo -> BuildInfo -> FilePath -> [String]
ghcCcOptions lbi bi odir
     =  ["-I" ++ dir | dir <- includeDirs bi]
     ++ concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ]
     ++ ["-optc" ++ opt | opt <- ccOptions bi]
     ++ ["-odir", odir]

mkGHCiLibName :: FilePath -- ^file Prefix
              -> String   -- ^library name.
              -> String
mkGHCiLibName pref lib = pref </> ("HS" ++ lib) <.> ".o"


findLdProgram :: LocalBuildInfo -> IO FilePath
#if defined(mingw32_TARGET_OS) || defined(mingw32_HOST_OS)
findLdProgram lbi =
   let
    compilerDir = takeDirectory $ compilerPath (compiler lbi)
    baseDir     = takeDirectory compilerDir
    binInstallLd     = baseDir </> "gcc-lib" </> "ld.exe"
   in do
   mb <- lookupProgram "ld" (withPrograms lbi)
   case fmap programLocation mb of
    Just (UserSpecified s) -> return s
          -- assume we're using an installed copy of GHC..
    _ -> return binInstallLd
#else
findLdProgram _ =
    return "ld"
#endif

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
  mbAr <- lookupProgram "ar" (withPrograms lbi) 
  let arProg = mbAr `programOrElse` "ar"
  ld <- findLdProgram lbi
  let builddir = buildDir lbi
  let decls = [
        ("modules", unwords (exposedModules lib ++ otherModules bi)),
        ("GHC", compilerPath (compiler lbi)),
        ("WAYS", if withProfLib lbi then "p" else ""),
        ("odir", builddir),
        ("srcdir", case hsSourceDirs bi of
                        [one] -> one
                        _     -> error "makefile: can't cope with multiple hs-source-dirs yet, sorry"),
        ("package", packageId),
        ("GHC_OPTS", unwords ( 
                           ["-package-name", packageId ]
                        ++ ghcOptions lbi bi (buildDir lbi))),
        ("MAKEFILE", file),
        ("C_SRCS", unwords (cSources bi)),
        ("GHC_CC_OPTS", unwords (ghcCcOptions lbi bi (buildDir lbi))),
        ("GHCI_LIB", mkGHCiLibName builddir (showPackageId (package pkg_descr))),
        ("AR", arProg),
        ("LD", ld)
        ]
  hPutStrLn h "# DO NOT EDIT!  Automatically generated by Cabal\n"
  hPutStrLn h (unlines (map (\(a,b)-> a ++ " = " ++ munge b) decls))
  hPutStrLn h makefileTemplate
  hClose h
 where
  munge "" = ""
  munge ('#':s) = '\\':'#':munge s
  munge ('\\':s) = '/':munge s
	-- for Windows, we want to use forward slashes in our pathnames in the Makefile
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
    = do createDirectoryIfMissingVerbose verbosity True pref
         withExe pkg_descr $ \ (Executable e _ _) -> do
             let exeFileName = e <.> exeExtension
             copyFileVerbose verbosity (buildPref </> e </> exeFileName) (pref </> exeFileName)

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

programOrElse :: Maybe Program -> FilePath -> FilePath
mb_prog `programOrElse` q =
  case mb_prog of
    Nothing -> q
    Just Program{programLocation=l} ->
        case l of
          UserSpecified x -> x
          FoundOnSystem x -> x
          EmptyLocation   -> q
