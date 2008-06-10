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
-- 'Distribution.Simple.GHC.PackageConfig.GHCPackageConfig' for
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
        configure, getInstalledPackages, build, makefile, installLib, installExe,
        ghcOptions,
        ghcVerbosityOptions
 ) where

import Distribution.Simple.GHC.Makefile
import Distribution.Simple.Setup ( MakefileFlags(..),
                                   fromFlag, fromFlagOrDefault)
import Distribution.PackageDescription
				( PackageDescription(..), BuildInfo(..),
				  withLib,
				  Executable(..), withExe, Library(..),
				  libModules, hcOptions )
import Distribution.InstalledPackageInfo
                                ( InstalledPackageInfo
                                , parseInstalledPackageInfo )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.ParseUtils  ( ParseResult(..) )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Package
         ( PackageIdentifier, Package(..) )
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration
         , rawSystemProgram, rawSystemProgramConf
         , rawSystemProgramStdout, rawSystemProgramStdoutConf, requireProgram
         , userMaybeSpecifyPath, programPath, lookupProgram, updateProgram
         , ghcProgram, ghcPkgProgram, arProgram, ranlibProgram, ldProgram
         , stripProgram )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..), compilerVersion
         , OptimisationLevel(..), PackageDB(..), Flag, extensionsToFlags )
import Distribution.Version
         ( Version(..), VersionRange(..), orLaterVersion )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Verbosity
import Distribution.Text
         ( display, simpleParse )
import Language.Haskell.Extension (Extension(..))

import Control.Monad		( unless, when )
import Data.Char
import Data.List		( nub )
import Data.Maybe               ( catMaybes )
import Data.Monoid              ( Monoid(mconcat) )
import System.Directory		( removeFile, renameFile,
				  getDirectoryContents, doesFileExist,
				  getTemporaryDirectory )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension, splitExtension )
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Control.Exception as Exception
         ( catch, handle, try )

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf = do

  (ghcProg, conf') <- requireProgram verbosity ghcProgram 
                        (orLaterVersion (Version [6,4] []))
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
       "Version mismatch between ghc and ghc-pkg: "
    ++ programPath ghcProg ++ " is version " ++ display ghcVersion ++ " "
    ++ programPath ghcPkgProg ++ " is version " ++ display ghcPkgVersion

  -- finding ghc's local ld is a bit tricky as it's not on the path:
  let ldProgram' = case buildOS of
        Windows ->
          let compilerDir  = takeDirectory (programPath ghcProg)
              baseDir      = takeDirectory compilerDir
              binInstallLd = baseDir </> "gcc-lib" </> "ld.exe"
           in ldProgram {
                  programFindLocation = \_ -> return (Just binInstallLd)
                }
        _ -> ldProgram

  -- we need to find out if ld supports the -x flag
  (ldProg, conf''') <- requireProgram verbosity ldProgram' AnyVersion conf''
  tempDir <- getTemporaryDirectory
  ldx <- withTempFile tempDir ".c" $ \testcfile testchnd ->
         withTempFile tempDir ".o" $ \testofile testohnd -> do
           hPutStrLn testchnd "int foo() {}"
           hClose testchnd; hClose testohnd
           rawSystemProgram verbosity ghcProg ["-c", testcfile,
                                               "-o", testofile]
           withTempFile tempDir ".o" $ \testofile' testohnd' ->
             handle (\_ -> return False) $ do
               hClose testohnd'
               rawSystemProgramStdout verbosity ldProg
                 ["-x", "-r", testofile, "-o", testofile']
               return True
  let conf'''' = updateProgram ldProg {
                  programArgs = if ldx then ["-x"] else []
		} conf'''
  -- Yeah yeah, so obviously conf''''' is totally rediculious and the program
  -- configuration needs to be in a state monad. That is exactly the plan
  -- (along with some other stuff to give Cabal a better DSL).

  languageExtensions <-
    if ghcVersion >= Version [6,7] []
      then do exts <- rawSystemStdout verbosity (programPath ghcProg)
                        ["--supported-languages"]
              -- GHC has the annoying habit of inverting some of the extensions
              -- so we have to try parsing ("No" ++ ghcExtensionName) first
              let readExtension str = do
                    ext <- simpleParse ("No" ++ str)
                    case ext of
                      UnknownExtension _ -> simpleParse str
                      _                  -> return ext
              return [ (ext, "-X" ++ display ext)
                     | Just ext <- map readExtension (lines exts) ]
      else return oldLanguageExtensions

  let comp = Compiler {
        compilerId             = CompilerId GHC ghcVersion,
        compilerExtensions     = languageExtensions
      }
  return (comp, conf'''')

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
       info verbosity $ "looking for package tool: ghc-pkg near compiler in " ++ dir
       exists <- mapM doesFileExist guesses
       case [ file | (file, True) <- zip guesses exists ] of
         [] -> return Nothing
         (pkgtool:_) -> do info verbosity $ "found package tool in " ++ pkgtool
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
    ,(NoMonoPatBinds             , "-fno-mono-pat-binds")
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
    ,(ScopedTypeVariables        , "-fscoped-type-variables")
    ,(FlexibleContexts           , fglasgowExts)
    ,(FlexibleInstances          , fglasgowExts)
    ,(EmptyDataDecls             , fglasgowExts)
    ,(PatternGuards              , fglasgowExts)
    ,(GeneralizedNewtypeDeriving , fglasgowExts)
    ,(MagicHash                  , fglasgowExts)
    ,(UnicodeSyntax              , fglasgowExts)
    ,(PatternSignatures          , fglasgowExts)
    ,(UnliftedFFITypes           , fglasgowExts)
    ,(LiberalTypeSynonyms        , fglasgowExts)
    ,(TypeOperators              , fglasgowExts)
    ,(GADTs                      , fglasgowExts)
    ,(RelaxedPolyRec             , fglasgowExts)
    ,(ExtendedDefaultRules       , "-fextended-default-rules")
    ,(UnboxedTuples              , fglasgowExts)
    ,(DeriveDataTypeable         , fglasgowExts)
    ,(ConstrainedClassMethods    , fglasgowExts)
    ]
    where
      fglasgowExts = "-fglasgow-exts"

getInstalledPackages :: Verbosity -> PackageDB -> ProgramConfiguration
                     -> IO (PackageIndex InstalledPackageInfo)
getInstalledPackages verbosity packagedb conf = do
  let packagedbs = case packagedb of
        GlobalPackageDB -> [GlobalPackageDB]
        _               -> [GlobalPackageDB, packagedb]
  pkgss <- getInstalledPackages' verbosity packagedbs conf
  return $ mconcat [ PackageIndex.fromList pkgs
                   | (_, pkgs) <- pkgss ]

-- | Get the packages from specific PackageDBs, not cumulative.
--
getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramConfiguration
                     -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs conf
  | ghcVersion >= Version [6,9] [] =
  sequence
    [ do str <- rawSystemProgramStdoutConf verbosity ghcPkgProgram conf
                  ["describe", "*", packageDbGhcPkgFlag packagedb]
           `Exception.catch` \_ -> die $
                  "ghc-pkg describe * failed. If you are using ghc-6.9 "
               ++ "and have an empty user package database then this "
               ++ "is probably due to ghc bug #2201. The workaround is to "
               ++ "register at least one package in the user package db."
         case parsePackages str of
	   Left ok -> return (packagedb, ok)
	   _       -> die "failed to parse output of 'ghc-pkg describe *'"
    | packagedb <- packagedbs ]

  where
    parsePackages str =
      let parsed = map parseInstalledPackageInfo (splitPkgs str)
       in case [ msg | ParseFailed msg <- parsed ] of
            []   -> Left [ pkg | ParseOk _ pkg <- parsed ]
            msgs -> Right msgs

    Just ghcProg = lookupProgram ghcProgram conf
    Just ghcVersion = programVersion ghcProg

    splitPkgs :: String -> [String]
    splitPkgs = map unlines . split [] . lines
      where split  [] [] = []
            split acc [] = [reverse acc]
            split acc (l@('n':'a':'m':'e':':':_):ls)
              | null acc     =               split (l:[])  ls
              | otherwise    = reverse acc : split (l:[])  ls
            split acc (l:ls) =               split (l:acc) ls

    packageDbGhcPkgFlag GlobalPackageDB          = "--global"
    packageDbGhcPkgFlag UserPackageDB            = "--user"
    packageDbGhcPkgFlag (SpecificPackageDB path) = "--package-conf=" ++ path

getInstalledPackages' verbosity packagedbs conf = do
    str <- rawSystemProgramStdoutConf verbosity ghcPkgProgram conf ["list"]
    let pkgFiles = [ init line | line <- lines str, last line == ':' ]
        dbFile packagedb = case (packagedb, pkgFiles) of
          (GlobalPackageDB, global:_)      -> return $ Just global
          (UserPackageDB,  _global:user:_) -> return $ Just user
          (UserPackageDB,  _global:_)      -> return $ Nothing
          (SpecificPackageDB specific, _)  -> return $ Just specific
          _ -> die "cannot read ghc-pkg package listing"
    pkgFiles' <- mapM dbFile packagedbs
    sequence [ withFileContents file $ \content ->
                  case reads content of
                    [(pkgs, _)] -> return (db, pkgs)
                    _ -> die $ "cannot read ghc package database " ++ file
             | (db , Just file) <- zip packagedbs pkgFiles' ]

-- -----------------------------------------------------------------------------
-- Building

-- |Building for GHC.  If .ghc-packages exists and is readable, add
-- it to the command-line.
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
  let pref = buildDir lbi
      pkgid = packageId pkg_descr
      runGhcProg = rawSystemProgramConf verbosity ghcProgram (withPrograms lbi)
      ifVanillaLib forceVanilla = when (forceVanilla || withVanillaLib lbi)
      ifProfLib = when (withProfLib lbi)
      ifSharedLib = when (withSharedLib lbi)
      ifGHCiLib = when (withGHCiLib lbi)

  -- Build lib
  withLib pkg_descr () $ \lib -> do
      info verbosity "Building library..."
      let libBi = libBuildInfo lib
          libTargetDir = pref
	  forceVanillaLib = TemplateHaskell `elem` extensions libBi
	  -- TH always needs vanilla libs, even when building for profiling

      createDirectoryIfMissingVerbose verbosity True libTargetDir
      -- TODO: do we need to put hs-boot files into place for mutually recurive modules?
      let ghcArgs =
                 ["-package-name", display pkgid ]
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
         info verbosity "Building C Sources..."
         sequence_ [do let (odir,args) = constructCcCmdLine lbi libBi pref 
                                                            filename verbosity
                       createDirectoryIfMissingVerbose verbosity True odir
                       runGhcProg args
                       ifSharedLib (runGhcProg (args ++ ["-fPIC", "-osuf dyn_o"]))
                   | filename <- cSources libBi]

      -- link:
      info verbosity "Linking..."
      let cObjs = map (`replaceExtension` objExtension) (cSources libBi)
	  cSharedObjs = map (`replaceExtension` ("dyn_" ++ objExtension)) (cSources libBi)
          vanillaLibFilePath = libTargetDir </> mkLibName pkgid
          profileLibFilePath = libTargetDir </> mkProfLibName pkgid
          sharedLibFilePath  = libTargetDir </> mkSharedLibName pkgid
                                                  (compilerId (compiler lbi))
          ghciLibFilePath    = libTargetDir </> mkGHCiLibName pkgid

      stubObjs <- fmap catMaybes $ sequence
        [ findFileWithExtension [objExtension] [libTargetDir]
            (dotToSep x ++"_stub")
        | x <- libModules pkg_descr ]
      stubProfObjs <- fmap catMaybes $ sequence
        [ findFileWithExtension ["p_" ++ objExtension] [libTargetDir]
            (dotToSep x ++"_stub")
        | x <- libModules pkg_descr ]
      stubSharedObjs <- fmap catMaybes $ sequence
        [ findFileWithExtension ["dyn_" ++ objExtension] [libTargetDir]
            (dotToSep x ++"_stub")
        | x <- libModules pkg_descr ]

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
        -- first remove library files if they exists
        sequence_
          [ try (removeFile libFilePath)
          | libFilePath <- [vanillaLibFilePath, profileLibFilePath
                           ,sharedLibFilePath,  ghciLibFilePath] ]

        let arVerbosity | verbosity >= deafening = "v"
                        | verbosity >= normal = ""
                        | otherwise = "c"
            arArgs = ["q"++ arVerbosity]
                ++ [vanillaLibFilePath]
            arObjArgs =
		   hObjs
                ++ map (pref </>) cObjs
                ++ stubObjs
            arProfArgs = ["q"++ arVerbosity]
                ++ [profileLibFilePath]
            arProfObjArgs =
		   hProfObjs
                ++ map (pref </>) cObjs
                ++ stubProfObjs
	    ldArgs = ["-r"]
	        ++ ["-o", ghciLibFilePath <.> "tmp"]
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
		  "-o", sharedLibFilePath ]
		++ ghcSharedObjArgs
		++ ["-package-name", display pkgid ]
		++ (concat [ ["-package", display pkg] | pkg <- packageDeps lbi ])
	        ++ ["-l"++extraLib | extraLib <- extraLibs libBi]
	        ++ ["-L"++extraLibDir | extraLibDir <- extraLibDirs libBi]

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

             --TODO: discover this at configure time or runtime on unix
             -- The value is 32k on Windows and posix specifies a minimum of 4k
             -- but all sensible unixes use more than 4k.
             -- we could use getSysVar ArgumentLimit but that's in the unix lib
            maxCommandLineSize = 30 * 1024

        ifVanillaLib False $ xargs maxCommandLineSize
          runAr arArgs arObjArgs

        ifProfLib $ xargs maxCommandLineSize
          runAr arProfArgs arProfObjArgs

        ifGHCiLib $ xargs maxCommandLineSize
          (runLd ghciLibFilePath) ldArgs ldObjArgs

        ifSharedLib $ runGhcProg ghcSharedLinkArgs

  -- build any executables
  withExe pkg_descr $ \Executable { exeName = exeName', modulePath = modPath,
                                    buildInfo = exeBi } -> do
                 info verbosity $ "Building executable: " ++ exeName' ++ "..."

                 -- exeNameReal, the name that GHC really uses (with .exe on Windows)
                 let exeNameReal = exeName' <.>
                                   (if null $ takeExtension exeName' then exeExtension else "")

		 let targetDir = pref </> exeName'
                 let exeDir    = targetDir </> (exeName' ++ "-tmp")
                 createDirectoryIfMissingVerbose verbosity True targetDir
                 createDirectoryIfMissingVerbose verbosity True exeDir
                 -- TODO: do we need to put hs-boot files into place for mutually recursive modules?
                 -- FIX: what about exeName.hi-boot?

                 -- build executables
                 unless (null (cSources exeBi)) $ do
                  info verbosity "Building C Sources."
		  sequence_ [do let (odir,args) = constructCcCmdLine lbi exeBi
                                                         exeDir filename verbosity
                                createDirectoryIfMissingVerbose verbosity True odir
                                runGhcProg args
                            | filename <- cSources exeBi]

                 srcMainFile <- findFile (exeDir : hsSourceDirs exeBi) modPath

                 let cObjs = map (`replaceExtension` objExtension) (cSources exeBi)
                 let binArgs linkExe profExe =
			    (if linkExe
			        then ["-o", targetDir </> exeNameReal]
                                else ["-c"])
                         ++ constructGHCCmdLine lbi exeBi exeDir verbosity
                         ++ [exeDir </> x | x <- cObjs]
                         ++ [srcMainFile]
			 ++ ldOptions exeBi
			 ++ ["-l"++lib | lib <- extraLibs exeBi]
			 ++ ["-L"++libDir | libDir <- extraLibDirs exeBi]
                         ++ concat [["-framework", f] | f <- frameworks exeBi]
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
     ++ ghcVerbosityOptions verbosity
        -- Unsupported extensions have already been checked by configure
     ++ ghcOptions lbi bi odir

ghcVerbosityOptions :: Verbosity -> [String]
ghcVerbosityOptions verbosity
     | verbosity >= deafening = ["-v"]
     | verbosity >= normal    = []
     | otherwise              = ["-w", "-v0"]

ghcOptions :: LocalBuildInfo -> BuildInfo -> FilePath -> [String]
ghcOptions lbi bi odir
     =  ["-hide-all-packages"]
     ++ (if splitObjs lbi then ["-split-objs"] else [])
     ++ ["-i"]
     ++ ["-i" ++ odir]
     ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
     ++ ["-i" ++ autogenModulesDir lbi]
     ++ ["-I" ++ odir]
     ++ ["-I" ++ dir | dir <- includeDirs bi]
     ++ ["-optP" ++ opt | opt <- cppOptions bi]
     ++ ["-optc" ++ opt | opt <- ccOptions bi]
     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes bi ]
     ++ [ "-odir",  odir, "-hidir", odir ]
     ++ (if compilerVersion c >= Version [6,8] []
           then ["-stubdir", odir] else [])
     ++ (concat [ ["-package", display pkg] | pkg <- packageDeps lbi ])
     ++ (case withOptimization lbi of
           NoOptimisation      -> []
           NormalOptimisation  -> ["-O"]
           MaximumOptimisation -> ["-O2"])
     ++ hcOptions GHC bi
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
     ++ concat [ ["-package", display pkg] | pkg <- packageDeps lbi ]
     ++ ["-optc" ++ opt | opt <- ccOptions bi]
     ++ (case withOptimization lbi of
           NoOptimisation -> []
           _              -> ["-optc-O2"])
     ++ ["-odir", odir]

mkGHCiLibName :: PackageIdentifier -> String
mkGHCiLibName lib = "HS" ++ display lib <.> "o"

-- -----------------------------------------------------------------------------
-- Building a Makefile

makefile :: PackageDescription -> LocalBuildInfo -> MakefileFlags -> IO ()
makefile pkg_descr lbi flags = do
  let file = fromFlagOrDefault "Makefile"(makefileFile flags)
      verbosity = fromFlag (makefileVerbosity flags)
  targetExists <- doesFileExist file
  when targetExists $
    die ("Not overwriting existing copy of " ++ file)
  h <- openFile file WriteMode

  let Just lib = library pkg_descr
      bi = libBuildInfo lib
  
      packageIdStr = display (packageId pkg_descr)
  (arProg, _) <- requireProgram verbosity arProgram AnyVersion
                   (withPrograms lbi)
  (ldProg, _) <- requireProgram verbosity ldProgram AnyVersion
                   (withPrograms lbi)
  let builddir = buildDir lbi
      Just ghcProg = lookupProgram ghcProgram (withPrograms lbi)
  let decls = [
        ("modules", unwords (exposedModules lib ++ otherModules bi)),
        ("GHC", programPath ghcProg),
        ("GHC_VERSION", (display (compilerVersion (compiler lbi)))),
        ("WAYS", (if withProfLib lbi then "p " else "") ++ (if withSharedLib lbi then "dyn" else "")),
        ("odir", builddir),
        ("srcdir", case hsSourceDirs bi of
                        [one] -> one
                        _     -> error "makefile: can't cope with multiple hs-source-dirs yet, sorry"),
        ("package", packageIdStr),
        ("GHC_OPTS", unwords ( 
                           ["-package-name", packageIdStr ]
                        ++ ghcOptions lbi bi (buildDir lbi))),
        ("MAKEFILE", file),
        ("C_SRCS", unwords (cSources bi)),
        ("GHC_CC_OPTS", unwords (ghcCcOptions lbi bi (buildDir lbi))),
        ("GHCI_LIB", builddir </> mkGHCiLibName (packageId pkg_descr)),
        ("soext", dllExtension),
        ("LIB_LD_OPTS", unwords (["-package-name", packageIdStr]
				 ++ concat [ ["-package", display pkg] | pkg <- packageDeps lbi ]
				 ++ ["-l"++libName | libName <- extraLibs bi]
				 ++ ["-L"++libDir | libDir <- extraLibDirs bi])),
        ("AR", programPath arProg),
        ("LD", programPath ldProg ++ concat [" " ++ arg | arg <- programArgs ldProg ])
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
           -> LocalBuildInfo
           -> FilePath  -- ^install location
           -> FilePath  -- ^Build location
           -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
           -> PackageDescription
           -> IO ()
installExe verbosity lbi pref buildPref (progprefix, progsuffix) pkg_descr
    = do createDirectoryIfMissingVerbose verbosity True pref
         withExe pkg_descr $ \Executable { exeName = e } -> do
             let exeFileName = e <.> exeExtension
                 fixedExeFileName = (progprefix ++ e ++ progsuffix) <.> exeExtension
             copyFileVerbose verbosity (buildPref </> e </> exeFileName) (pref </> fixedExeFileName)
             stripExe verbosity lbi exeFileName (pref </> fixedExeFileName)

stripExe :: Verbosity -> LocalBuildInfo -> FilePath -> FilePath -> IO ()
stripExe verbosity lbi name path = when (stripExes lbi) $
  case lookupProgram stripProgram (withPrograms lbi) of
    Just strip -> rawSystemProgram verbosity strip [path]
    Nothing    -> warn verbosity $ "Unable to strip executable '" ++ name
                                ++ "' (missing the 'strip' program)"

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity -- ^verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic librarys
              -> FilePath  -- ^Build location
              -> PackageDescription -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir
              pkg@PackageDescription{library=Just _} = do
  -- copy .hi files over:
  let copyModuleFiles ext =
        smartCopySources verbosity [builtDir] targetDir (libModules pkg) [ext]
  ifVanilla $ copyModuleFiles "hi"
  ifProf    $ copyModuleFiles "p_hi"

  -- copy the built library files over:
  ifVanilla $ copy builtDir targetDir vanillaLibName
  ifProf    $ copy builtDir targetDir profileLibName
  ifGHCi    $ copy builtDir targetDir ghciLibName
  ifShared  $ copy builtDir dynlibTargetDir sharedLibName

  -- run ranlib if necessary:
  ifVanilla $ updateLibArchive verbosity lbi (targetDir </> vanillaLibName)
  ifProf    $ updateLibArchive verbosity lbi (targetDir </> profileLibName)

  where
    vanillaLibName = mkLibName pkgid
    profileLibName = mkProfLibName pkgid
    ghciLibName    = mkGHCiLibName pkgid
    sharedLibName  = mkSharedLibName pkgid (compilerId (compiler lbi))

    pkgid          = packageId pkg
    copy src dst n = copyFileVerbose verbosity (src </> n) (dst </> n)

    ifVanilla = when (withVanillaLib lbi)
    ifProf    = when (withProfLib    lbi)
    ifGHCi    = when (withGHCiLib    lbi)
    ifShared  = when (withSharedLib  lbi)

installLib _ _ _ _ _ PackageDescription{library=Nothing}
    = die $ "Internal Error. installLibGHC called with no library."

-- | use @ranlib@ or @ar -s@ to build an index. This is necessary on systems
-- like MacOS X. If we can't find those, don't worry too much about it.
--
updateLibArchive :: Verbosity -> LocalBuildInfo -> FilePath -> IO ()
updateLibArchive verbosity lbi path =
  case lookupProgram ranlibProgram (withPrograms lbi) of
    Just ranlib -> rawSystemProgram verbosity ranlib [path]
    Nothing     -> case lookupProgram arProgram (withPrograms lbi) of
      Just ar   -> rawSystemProgram verbosity ar ["-s", path]
      Nothing   -> warn verbosity $
                        "Unable to generate a symbol index for the static "
                     ++ "library '" ++ path
                     ++ "' (missing the 'ranlib' and 'ar' programs)"
