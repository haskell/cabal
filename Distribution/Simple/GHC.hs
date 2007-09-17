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
	configure, getInstalledPackages, build, makefile, installLib, installExe
 ) where

import Distribution.Simple.GHC.Makefile
import Distribution.Simple.Setup       ( MakefileFlags(..) )
import Distribution.PackageDescription
				( PackageDescription(..), BuildInfo(..),
				  withLib, setupMessage,
				  Executable(..), withExe, Library(..),
				  libModules, hcOptions )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..), autogenModulesDir )
import Distribution.Simple.Utils
import Distribution.Package  	( PackageIdentifier(..), showPackageId,
                                  parsePackageId )
import Distribution.Simple.Program ( rawSystemProgram, rawSystemProgramConf,
				  rawSystemProgramStdoutConf,
				  Program(..), ConfiguredProgram(..),
                                  ProgramConfiguration, addKnownProgram,
                                  userMaybeSpecifyPath, requireProgram,
                                  programPath, lookupProgram,
                                  ghcProgram, ghcPkgProgram,
                                  arProgram, ranlibProgram, ldProgram )
import Distribution.Simple.Compiler
import Distribution.Version	( Version(..), showVersion,
                                  VersionRange(..), orLaterVersion )
import qualified Distribution.Simple.GHC.PackageConfig as GHC
				( localPackageConfig,
				  canReadLocalPackageConfig )
import Distribution.System
import Distribution.Verbosity
import Language.Haskell.Extension (Extension(..))
import Distribution.Compat.ReadP
    ( readP_to_S, many, skipSpaces )

import Control.Monad		( unless, when )
import Data.Char
import Data.List		( nub, isPrefixOf )
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

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf = do

  (ghcProg, conf') <- requireProgram verbosity ghcProgram 
                        (orLaterVersion (Version [6,2] []))
                        (userMaybeSpecifyPath "ghc" hcPath conf)
  let Just ghcVersion = programVersion ghcProg

  -- This is slightly tricky, we have to configure ghc first, then we use the
  -- location of ghc to help find ghc-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcPkgProg, conf'') <- requireProgram verbosity ghcPkgProgram {
                            programFindLocation = guessGhcPkgFromGhcPath ghcProg
                          }
                          (orLaterVersion (Version [0] []))
                          (userMaybeSpecifyPath "ghc-pkg" hcPkgPath conf')
  let Just ghcPkgVersion = programVersion ghcPkgProg

  when (ghcVersion /= ghcPkgVersion) $ die $
       "Version mismatch between ghc and ghc-pkg:\n"
    ++ programPath ghcProg ++ " is version " ++ showVersion ghcVersion ++ "\n"
    ++ programPath ghcPkgProg ++ " is version " ++ showVersion ghcPkgVersion

  -- finding ghc's local ld is a bit tricky as it's not on the path:
  let conf''' = case os of
        Windows _ ->
          let compilerDir  = takeDirectory (programPath ghcProg)
              baseDir      = takeDirectory compilerDir
              binInstallLd = baseDir </> "gcc-lib" </> "ld.exe"
           in addKnownProgram ldProgram {
                  programFindLocation = \_ -> return (Just binInstallLd)
                } conf''
        _ -> conf''

  let isSep c = isSpace c || (c == ',')
  languageExtensions <-
    if ghcVersion >= Version [6,7] []
      then do exts <- rawSystemStdout verbosity (programPath ghcProg)
                        ["--supported-languages"]
              return [ (ext, "-X" ++ show ext)
                     | extStr <- breaks isSep exts
                     , (ext, "") <- reads extStr ++ reads ("No" ++ extStr) ]
      else return oldLanguageExtensions

  let comp = Compiler {
        compilerFlavor         = GHC,
        compilerId             = PackageIdentifier "ghc" ghcVersion,
        compilerExtensions     = languageExtensions
      }
  return (comp, conf''')

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding ghc-pkg, we try looking for both a versioned and unversioned
-- ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
--
guessGhcPkgFromGhcPath :: ConfiguredProgram -> Verbosity -> IO (Maybe FilePath)
guessGhcPkgFromGhcPath ghcProg verbosity
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
         [] -> return Nothing
         (pkgtool:_) -> do when (verbosity >= verbose) $
                             putStrLn $ "found package tool in " ++ pkgtool
                           return (Just pkgtool)

  where takeVersionSuffix :: FilePath -> String
        takeVersionSuffix = reverse . takeWhile (`elem ` "0123456789.-") . reverse

        dropExeExtension :: FilePath -> FilePath
        dropExeExtension filepath =
          case splitExtension filepath of
            (filepath', extension) | extension == exeExtension -> filepath'
                                   | otherwise                 -> filepath

-- | For GHC 6.6.x and earlier, the mapping from supported extensions to flags
oldLanguageExtensions :: [(Extension, Flag)]
oldLanguageExtensions =
    [(OverlappingInstances       , "-fallow-overlapping-instances")
    ,(TypeSynonymInstances       , "-fglasgow-exts")
    ,(TemplateHaskell            , "-fth")
    ,(ForeignFunctionInterface   , "-fffi")
    ,(NoMonomorphismRestriction  , "-fno-monomorphism-restriction")
    ,(UndecidableInstances       , "-fallow-undecidable-instances")
    ,(IncoherentInstances        , "-fallow-incoherent-instances")
    ,(Arrows                     , "-farrows")
    ,(Generics                   , "-fgenerics")
    ,(NoImplicitPrelude          , "-fno-implicit-prelude")
    ,(ImplicitParams             , "-fimplicit-params")
    ,(CPP                        , "-cpp")
    ,(BangPatterns               , "-fbang-patterns")
    ,(KindSignatures             , fglasgowExts)
    ,(RecursiveDo                , fglasgowExts)
    ,(ParallelListComp           , fglasgowExts)
    ,(MultiParamTypeClasses      , fglasgowExts)
    ,(FunctionalDependencies     , fglasgowExts)
    ,(Rank2Types                 , fglasgowExts)
    ,(RankNTypes                 , fglasgowExts)
    ,(PolymorphicComponents      , fglasgowExts)
    ,(ExistentialQuantification  , fglasgowExts)
    ,(ScopedTypeVariables        , fglasgowExts)
    ,(FlexibleContexts           , fglasgowExts)
    ,(FlexibleInstances          , fglasgowExts)
    ,(EmptyDataDecls             , fglasgowExts)
    ,(PatternGuards              , fglasgowExts)
    ,(GeneralizedNewtypeDeriving , fglasgowExts)
    ,(MagicHash                  , fglasgowExts)
    ]
    where
      fglasgowExts = "-fglasgow-exts"

getInstalledPackages :: Verbosity -> PackageDB -> ProgramConfiguration
                     -> IO [PackageIdentifier]
getInstalledPackages verbosity packagedb conf = do
   --TODO: use --simple-output flag for easier parsing
   str <- rawSystemProgramStdoutConf verbosity ghcPkgProgram conf
            [packageDbGhcPkgFlag packagedb, "list"]
   let keep_line s = ':' `notElem` s && not ("Creating" `isPrefixOf` s)
       str1 = unlines (filter keep_line (lines str))
       str2 = filter (`notElem` ",(){}") str1
       --
   case pCheck (readP_to_S (many (skipSpaces >> parsePackageId)) str2) of
     [ps] -> return ps
     _    -> die "cannot parse package list"
  where
    packageDbGhcPkgFlag GlobalPackageDB          = "--global"
    packageDbGhcPkgFlag UserPackageDB            = "--user"
    packageDbGhcPkgFlag (SpecificPackageDB path) = "--package-conf=" ++ path

    pCheck :: [(a, [Char])] -> [a]
    pCheck rs = [ r | (r,s) <- rs, all isSpace s ]

-- -----------------------------------------------------------------------------
-- Building

-- |Building for GHC.  If .ghc-packages exists and is readable, add
-- it to the command-line.
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
  let pref = buildDir lbi
      runGhcProg = rawSystemProgramConf verbosity ghcProgram (withPrograms lbi)
      ifVanillaLib forceVanilla = when (forceVanilla || withVanillaLib lbi)
      ifProfLib = when (withProfLib lbi)
      ifSharedLib = when (withSharedLib lbi)
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
          ghcArgsShared = ghcArgs
              ++ ["-dynamic",
                  "-hisuf", "dyn_hi",
                  "-osuf", "dyn_o", "-fPIC"
                 ]
              ++ ghcSharedOptions libBi
      unless (null (libModules pkg_descr)) $
        do ifVanillaLib forceVanillaLib (runGhcProg ghcArgs)
           ifProfLib (runGhcProg ghcArgsProf)
           ifSharedLib (runGhcProg ghcArgsShared)

      -- build any C sources
      unless (null (cSources libBi)) $ do
         when (verbosity >= verbose) (putStrLn "Building C Sources...")
         sequence_ [do let (odir,args) = constructCcCmdLine lbi libBi pref 
                                                            filename verbosity
                       createDirectoryIfMissingVerbose verbosity True odir
                       runGhcProg args
                       ifSharedLib (runGhcProg (args ++ ["-fPIC", "-osuf dyn_o"]))
                   | filename <- cSources libBi]

      -- link:
      when (verbosity > verbose) (putStrLn "cabal-linking...")
      let cObjs = map (`replaceExtension` objExtension) (cSources libBi)
	  cSharedObjs = map (`replaceExtension` ("dyn_" ++ objExtension)) (cSources libBi)
	  libName  = mkLibName pref (showPackageId (package pkg_descr))
	  profLibName  = mkProfLibName pref (showPackageId (package pkg_descr))
	  sharedLibName  = mkSharedLibName pref (showPackageId (package pkg_descr)) (compilerId (compiler lbi))
	  ghciLibName = mkGHCiLibName pref (showPackageId (package pkg_descr))

      stubObjs <- sequence [moduleToFilePath [libTargetDir] (x ++"_stub") [objExtension]
                           |  x <- libModules pkg_descr ]  >>= return . concat
      stubProfObjs <- sequence [moduleToFilePath [libTargetDir] (x ++"_stub") ["p_" ++ objExtension]
                           |  x <- libModules pkg_descr ]  >>= return . concat
      stubSharedObjs <- sequence [moduleToFilePath [libTargetDir] (x ++"_stub") ["dyn_" ++ objExtension]
                           |  x <- libModules pkg_descr ]  >>= return . concat

      hObjs     <- getHaskellObjects pkg_descr libBi lbi
			pref objExtension True
      hProfObjs <- 
	if (withProfLib lbi)
		then getHaskellObjects pkg_descr libBi lbi
			pref ("p_" ++ objExtension) True
		else return []
      hSharedObjs <-
	if (withSharedLib lbi)
		then getHaskellObjects pkg_descr libBi lbi
			pref ("dyn_" ++ objExtension) False
		else return []

      unless (null hObjs && null cObjs && null stubObjs) $ do
        Try.try (removeFile libName) -- first remove library if it exists
        Try.try (removeFile profLibName) -- first remove library if it exists
        Try.try (removeFile sharedLibName) -- first remove library if it exists
	Try.try (removeFile ghciLibName) -- first remove library if it exists

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
            ghcSharedObjArgs =
		   hSharedObjs
                ++ map (pref </>) cSharedObjs
		++ stubSharedObjs
	    -- After the relocation lib is created we invoke ghc -shared
	    -- with the dependencies spelled out as -package arguments
	    -- and ghc invokes the linker with the proper library paths
	    ghcSharedLinkArgs =
		[ "-shared",
		  "-dynamic",
		  "-o", sharedLibName ]
		++ ghcSharedObjArgs
		++ (concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ])

            runLd ldLibName args = do
              exists <- doesFileExist ldLibName
	        -- This method is called iteratively by xargs. The
	        -- output goes to <ldLibName>.tmp, and any existing file
	        -- named <ldLibName> is included when linking. The
	        -- output is renamed to <libName>.
              rawSystemProgramConf verbosity ldProgram (withPrograms lbi)
                (args ++ if exists then [ldLibName] else [])
              renameFile (ldLibName <.> "tmp") ldLibName

            runAr = rawSystemProgramConf verbosity arProgram (withPrograms lbi)

             --TODO: discover this at configure time on unix
            maxCommandLineSize = 30 * 1024

        ifVanillaLib False $ xargs maxCommandLineSize
          runAr arArgs arObjArgs

        ifProfLib $ xargs maxCommandLineSize
          runAr arProfArgs arProfObjArgs

        ifGHCiLib $ xargs maxCommandLineSize
          (runLd ghciLibName) ldArgs ldObjArgs

        ifSharedLib $ runGhcProg ghcSharedLinkArgs

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
                                runGhcProg args
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
		    (runGhcProg (binArgs False False))

		 runGhcProg (binArgs True (withProfExe lbi))


-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: PackageDescription -> BuildInfo -> LocalBuildInfo
 	-> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects pkg_descr _ lbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
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
     =  (if compilerVersion c > Version [6,4] []
            then ["-hide-all-packages"]
            else [])
     ++ (if splitObjs lbi then ["-split-objs"] else [])
     ++ ["-i"]
     ++ ["-i" ++ autogenModulesDir lbi]
     ++ ["-i" ++ odir]
     ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
     ++ ["-I" ++ odir]
     ++ ["-I" ++ dir | dir <- includeDirs bi]
     ++ ["-optc" ++ opt | opt <- ccOptions bi]
     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes bi ]
     ++ [ "-odir",  odir, "-hidir", odir ]
     ++ (if compilerVersion c > Version [6,6] []
           then ["-stubdir", odir] else [])
     ++ (concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ])
     ++ (if withOptimization lbi then ["-O"] else [])
     ++ hcOptions GHC (options bi)
     ++ extensionsToFlags c (extensions bi)
    where c = compiler lbi

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
  (arProg, _) <- requireProgram (makefileVerbose flags) arProgram AnyVersion
                   (withPrograms lbi)
  (ldProg, _) <- requireProgram (makefileVerbose flags) ldProgram AnyVersion
                   (withPrograms lbi)
  let builddir = buildDir lbi
      Just ghcProg = lookupProgram ghcProgram (withPrograms lbi)
  let decls = [
        ("modules", unwords (exposedModules lib ++ otherModules bi)),
        ("GHC", programPath ghcProg),
        ("GHC_VERSION", (showVersion (compilerVersion (compiler lbi)))),
        ("WAYS", (if withProfLib lbi then "p " else "") ++ (if withSharedLib lbi then "dyn" else "")),
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
        ("soext", dllExtension),
        ("LIB_LD_OPTS", unwords (concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ])),
        ("AR", programPath arProg),
        ("LD", programPath ldProg)
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
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic librarys
              -> FilePath  -- ^Build location
              -> PackageDescription -> IO ()
installLib verbosity lbi pref dynPref buildPref
              pd@PackageDescription{library=Just _,
                                    package=p}
    = do let programConf = withPrograms lbi
         ifVanilla $ smartCopySources verbosity [buildPref] pref (libModules pd) ["hi"] True False
         ifProf $ smartCopySources verbosity [buildPref] pref (libModules pd) ["p_hi"] True False
         let libTargetLoc = mkLibName pref (showPackageId p)
             profLibTargetLoc = mkProfLibName pref (showPackageId p)
	     libGHCiTargetLoc = mkGHCiLibName pref (showPackageId p)
	     sharedLibTargetLoc = mkSharedLibName dynPref (showPackageId p) (compilerId (compiler lbi))
         ifVanilla $ copyFileVerbose verbosity (mkLibName buildPref (showPackageId p)) libTargetLoc
         ifProf $ copyFileVerbose verbosity (mkProfLibName buildPref (showPackageId p)) profLibTargetLoc
	 ifGHCi $ copyFileVerbose verbosity (mkGHCiLibName buildPref (showPackageId p)) libGHCiTargetLoc
	 ifShared $ copyFileVerbose verbosity (mkSharedLibName buildPref (showPackageId p) (compilerId (compiler lbi))) sharedLibTargetLoc

         -- use ranlib or ar -s to build an index. this is necessary
         -- on some systems like MacOS X.  If we can't find those,
         -- don't worry too much about it.
         case lookupProgram ranlibProgram programConf of
           Just rl  -> do ifVanilla $ rawSystemProgram verbosity rl [libTargetLoc]
                          ifProf $ rawSystemProgram verbosity rl [profLibTargetLoc]

           Nothing -> case lookupProgram arProgram programConf of
                          Just ar  -> do ifVanilla $ rawSystemProgram verbosity ar ["-s", libTargetLoc]
                                         ifProf $ rawSystemProgram verbosity ar ["-s", profLibTargetLoc]
                          Nothing -> setupMessage verbosity "Warning: Unable to generate index for library (missing ranlib and ar)" pd
         return ()
    where ifVanilla action = when (withVanillaLib lbi) (action >> return ())
          ifProf action = when (withProfLib lbi) (action >> return ())
          ifGHCi action = when (withGHCiLib lbi) (action >> return ())
          ifShared action = when (withSharedLib lbi) (action >> return ())

installLib _ _ _ _ _ PackageDescription{library=Nothing}
    = die $ "Internal Error. installLibGHC called with no library."
