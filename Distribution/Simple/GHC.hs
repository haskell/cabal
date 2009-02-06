-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC
-- Copyright   :  Isaac Jones 2003-2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is a fairly large module. It contains most of the GHC-specific code for
-- configuring, building and installing packages. It also exports a function
-- for finding out what packages are already installed. Configuring involves
-- finding the @ghc@ and @ghc-pkg@ programs, finding what language extensions
-- this version of ghc supports and returning a 'Compiler' value.
--
-- 'getInstalledPackages' involves calling the @ghc-pkg@ program to find out
-- what packages are installed.
--
-- Building is somewhat complex as there is quite a bit of information to take
-- into account. We have to build libs and programs, possibly for profiling and
-- shared libs. We have to support building libraries that will be usable by
-- GHCi and also ghc's @-split-objs@ feature. We have to compile any C files
-- using ghc. Linking, especially for @split-objs@ is remarkably complex,
-- partly because there tend to be 1,000's of @.o@ files and this can often be
-- more than we can pass to the @ld@ or @ar@ programs in one go.
--
-- There is also some code for generating @Makefiles@ but the less said about
-- that the better.
--
-- Installing for libs and exes involves finding the right files and copying
-- them to the right places. One of the more tricky things about this module is
-- remembering the layout of files in the build directory (which is not
-- explicitly documented) and thus what search dirs are used for various kinds
-- of files.

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
import qualified Distribution.Simple.GHC.IPI641 as IPI641
import qualified Distribution.Simple.GHC.IPI642 as IPI642
import Distribution.Simple.Setup ( CopyFlags(..), MakefileFlags(..),
                                   fromFlag, fromFlagOrDefault)
import Distribution.PackageDescription as PD
                                ( PackageDescription(..), BuildInfo(..),
                                  withLib,
                                  Executable(..), withExe, Library(..),
                                  libModules, hcOptions )
import Distribution.InstalledPackageInfo
                                ( InstalledPackageInfo
                                , parseInstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
                                ( InstalledPackageInfo_(..) )
import Distribution.Simple.PackageIndex
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.ParseUtils  ( ParseResult(..) )
import Distribution.Simple.LocalBuildInfo
                                ( LocalBuildInfo(..), InstallDirs(..) )
import Distribution.Simple.InstallDirs
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Package
         ( PackageIdentifier, Package(..), PackageName(..) )
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration, ProgArg
         , ProgramLocation(..), rawSystemProgram, rawSystemProgramConf
         , rawSystemProgramStdout, rawSystemProgramStdoutConf, requireProgram
         , userMaybeSpecifyPath, programPath, lookupProgram, addKnownProgram
         , ghcProgram, ghcPkgProgram, arProgram, ranlibProgram, ldProgram
         , gccProgram, stripProgram )
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

import Control.Monad            ( unless, when )
import Data.Char
import Data.List
import Data.Maybe               ( catMaybes )
import System.Directory         ( removeFile, renameFile,
                                  getDirectoryContents, doesFileExist,
                                  getTemporaryDirectory )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension, splitExtension )
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Distribution.Compat.Exception (catchExit, catchIO)
import Distribution.Compat.Permissions (copyPermissions)
import Distribution.Compat.CopyFile
         ( copyExecutableFile )

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

  languageExtensions <- getLanguageExtensions verbosity ghcProg

  let comp = Compiler {
        compilerId             = CompilerId GHC ghcVersion,
        compilerExtensions     = languageExtensions
      }
      conf''' = configureToolchain ghcProg conf'' -- configure gcc and ld
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

-- | Adjust the way we find and configure gcc and ld
--
configureToolchain :: ConfiguredProgram -> ProgramConfiguration
                                        -> ProgramConfiguration
configureToolchain ghcProg = 
    addKnownProgram gccProgram {
      programFindLocation = findProg gccProgram (baseDir </> "gcc.exe"),
      programPostConf     = configureGcc 
    }
  . addKnownProgram ldProgram {
      programFindLocation = findProg ldProgram (libDir </> "ld.exe"),
      programPostConf     = configureLd
    }
  where
    compilerDir = takeDirectory (programPath ghcProg)
    baseDir     = takeDirectory compilerDir
    libDir      = baseDir </> "gcc-lib"
    includeDir  = baseDir </> "include" </> "mingw"
    isWindows   = case buildOS of Windows -> True; _ -> False

    -- on Windows finding and configuring ghc's gcc and ld is a bit special
    findProg :: Program -> FilePath -> Verbosity -> IO (Maybe FilePath)
    findProg prog location | isWindows = \_ -> do
        exists <- doesFileExist location
        if exists then return (Just location) else return Nothing
      | otherwise = programFindLocation prog

    configureGcc :: Verbosity -> ConfiguredProgram -> IO [ProgArg]
    configureGcc
      | isWindows = \_ gccProg -> case programLocation gccProg of
          -- if it's found on system then it means we're using the result
          -- of programFindLocation above rather than a user-supplied path
          -- that means we should add this extra flag to tell ghc's gcc
          -- where it lives and thus where gcc can find its various files:
          FoundOnSystem {} -> return ["-B" ++ libDir, "-I" ++ includeDir]
          UserSpecified {} -> return []
      | otherwise = \_ _   -> return []

    -- we need to find out if ld supports the -x flag
    configureLd :: Verbosity -> ConfiguredProgram -> IO [ProgArg]
    configureLd verbosity ldProg = do
      tempDir <- getTemporaryDirectory
      ldx <- withTempFile tempDir ".c" $ \testcfile testchnd ->
             withTempFile tempDir ".o" $ \testofile testohnd -> do
               hPutStrLn testchnd "int foo() {}"
               hClose testchnd; hClose testohnd
               rawSystemProgram verbosity ghcProg ["-c", testcfile,
                                                   "-o", testofile]
               withTempFile tempDir ".o" $ \testofile' testohnd' ->
                 do
                   hClose testohnd'
                   rawSystemProgramStdout verbosity ldProg
                     ["-x", "-r", testofile, "-o", testofile']
                   return True
                 `catchIO`   (\_ -> return False)
                 `catchExit` (\_ -> return False)
      if ldx
        then return ["-x"]
        else return []

getLanguageExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Flag)]
getLanguageExtensions verbosity ghcProg
  | ghcVersion >= Version [6,7] [] = do

    exts <- rawSystemStdout verbosity (programPath ghcProg)
              ["--supported-languages"]
    -- GHC has the annoying habit of inverting some of the extensions
    -- so we have to try parsing ("No" ++ ghcExtensionName) first
    let readExtension str = do
          ext <- simpleParse ("No" ++ str)
          case ext of
            UnknownExtension _ -> simpleParse str
            _                  -> return ext
    return $ extensionHacks
          ++ [ (ext, "-X" ++ display ext)
             | Just ext <- map readExtension (lines exts) ]

  | otherwise = return oldLanguageExtensions

  where
    Just ghcVersion = programVersion ghcProg

    -- ghc-6.8 intorduced RecordPuns however it should have been
    -- NamedFieldPuns. We now encourage packages to use NamedFieldPuns so for
    -- compatability we fake support for it in ghc-6.8 by making it an alias
    -- for the old RecordPuns extension.
    extensionHacks = [ (NamedFieldPuns, "-XRecordPuns")
                     | ghcVersion >= Version [6,8]  []
                    && ghcVersion <  Version [6,10] [] ]

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
  let pkgs = concatMap snd pkgss
      -- On Windows, various fields have $topdir/foo rather than full
      -- paths. We need to substitute the right value in so that when
      -- we, for example, call gcc, we have proper paths to give it
      Just ghcProg = lookupProgram ghcProgram conf
      compilerDir  = takeDirectory (programPath ghcProg)
      topDir       = takeDirectory compilerDir
      pkgs'        = map (substTopDir topDir) pkgs
      pi1          = PackageIndex.fromList pkgs'
      rtsPackages  = lookupPackageName pi1 (PackageName "rts")
      rtsPackages' = map removeMingwIncludeDir rtsPackages
      pi2          = pi1 `merge` fromList rtsPackages'
  return pi2

-- GHC < 6.10 put "$topdir/include/mingw" in rts's installDirs. This
-- breaks when you want to use a different gcc, so we need to filter
-- it out.
removeMingwIncludeDir :: InstalledPackageInfo -> InstalledPackageInfo
removeMingwIncludeDir pkg =
    let ids = InstalledPackageInfo.includeDirs pkg
        ids' = filter (not . ("mingw" `isSuffixOf`)) ids
    in pkg { InstalledPackageInfo.includeDirs = ids' }

-- | Get the packages from specific PackageDBs, not cumulative.
--
getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramConfiguration
                     -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs conf
  | ghcVersion >= Version [6,9] [] =
  sequence
    [ do str <- rawSystemProgramStdoutConf verbosity ghcPkgProgram conf
                  ["dump", packageDbGhcPkgFlag packagedb]
           `catchExit` \_ -> die $ "ghc-pkg dump failed"
         case parsePackages str of
           Left ok -> return (packagedb, ok)
           _       -> die "failed to parse output of 'ghc-pkg dump'"
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
    splitPkgs = map unlines . splitWith ("---" ==) . lines
      where
        splitWith :: (a -> Bool) -> [a] -> [[a]]
        splitWith p xs = ys : case zs of
                           []   -> []
                           _:ws -> splitWith p ws
          where (ys,zs) = break p xs

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
    sequence [ withFileContents file $ \content -> do
                  pkgs <- readPackages file content
                  return (db, pkgs)
             | (db , Just file) <- zip packagedbs pkgFiles' ]
  where
    -- Depending on the version of ghc we use a different type's Read
    -- instance to parse the package file and then convert.
    -- It's a bit yuck. But that's what we get for using Read/Show.
    readPackages
      | ghcVersion >= Version [6,4,2] []
      = \file content -> case reads content of
          [(pkgs, _)] -> return (map IPI642.toCurrent pkgs)
          _           -> failToRead file
      | otherwise
      = \file content -> case reads content of
          [(pkgs, _)] -> return (map IPI641.toCurrent pkgs)
          _           -> failToRead file
    Just ghcProg = lookupProgram ghcProgram conf
    Just ghcVersion = programVersion ghcProg
    failToRead file = die $ "cannot read ghc package database " ++ file

substTopDir :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
substTopDir topDir ipo
 = ipo {
       InstalledPackageInfo.importDirs
           = map f (InstalledPackageInfo.importDirs ipo),
       InstalledPackageInfo.libraryDirs
           = map f (InstalledPackageInfo.libraryDirs ipo),
       InstalledPackageInfo.includeDirs
           = map f (InstalledPackageInfo.includeDirs ipo),
       InstalledPackageInfo.frameworkDirs
           = map f (InstalledPackageInfo.frameworkDirs ipo),
       InstalledPackageInfo.haddockInterfaces
           = map f (InstalledPackageInfo.haddockInterfaces ipo),
       InstalledPackageInfo.haddockHTMLs
           = map f (InstalledPackageInfo.haddockHTMLs ipo)
   }
    where f ('$':'t':'o':'p':'d':'i':'r':rest) = topDir ++ rest
          f x = x

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
      ifGHCiLib = when (withGHCiLib lbi && withVanillaLib lbi)

  -- Build lib
  withLib pkg_descr () $ \lib -> do
      info verbosity "Building library..."

      libBi <- hackThreadedFlag verbosity
                 (compiler lbi) (withProfLib lbi) (libBuildInfo lib)

      let libTargetDir = pref
          forceVanillaLib = TemplateHaskell `elem` extensions libBi
          -- TH always needs vanilla libs, even when building for profiling

      createDirectoryIfMissingVerbose verbosity True libTargetDir
      -- TODO: do we need to put hs-boot files into place for mutually recurive modules?
      let ghcArgs =
                 ["-package-name", display pkgid ]
              ++ constructGHCCmdLine lbi libBi libTargetDir verbosity
              ++ map display (libModules pkg_descr)
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
            (ModuleName.toFilePath x ++"_stub")
        | x <- libModules pkg_descr ]
      stubProfObjs <- fmap catMaybes $ sequence
        [ findFileWithExtension ["p_" ++ objExtension] [libTargetDir]
            (ModuleName.toFilePath x ++"_stub")
        | x <- libModules pkg_descr ]
      stubSharedObjs <- fmap catMaybes $ sequence
        [ findFileWithExtension ["dyn_" ++ objExtension] [libTargetDir]
            (ModuleName.toFilePath x ++"_stub")
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
          [ removeFile libFilePath `catchIO` \_ -> return ()
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
                [ "-no-auto-link-packages",
                  "-shared",
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
  withExe pkg_descr $ \exe@Executable { exeName = exeName', modulePath = modPath } -> do
                 info verbosity $ "Building executable: " ++ exeName' ++ "..."

                 exeBi <- hackThreadedFlag verbosity
                            (compiler lbi) (withProfExe lbi) (buildInfo exe)

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
                         ++ ["-optl" ++ opt | opt <- PD.ldOptions exeBi]
                         ++ ["-l"++lib | lib <- extraLibs exeBi]
                         ++ ["-L"++libDir | libDir <- extraLibDirs exeBi]
                         ++ concat [["-framework", f] | f <- PD.frameworks exeBi]
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

-- | Filter the "-threaded" flag when profiling as it does not
--   work with ghc-6.8 and older.
hackThreadedFlag :: Verbosity -> Compiler -> Bool -> BuildInfo -> IO BuildInfo
hackThreadedFlag verbosity comp prof bi
  | not mustFilterThreaded = return bi
  | otherwise              = do
    warn verbosity $ "The ghc flag '-threaded' is not compatible with "
                  ++ "profiling in ghc-6.8 and older. It will be disabled."
    return bi { options = filterHcOptions (/= "-threaded") (options bi) }
  where
    mustFilterThreaded = prof && compilerVersion comp < Version [6, 10] []
                      && "-threaded" `elem` hcOptions GHC bi
    filterHcOptions p hcoptss =
      [ (hc, if hc == GHC then filter p opts else opts)
      | (hc, opts) <- hcoptss ]

-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: PackageDescription -> BuildInfo -> LocalBuildInfo
        -> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects pkg_descr _ lbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
        let dirs = [ pref </> (ModuleName.toFilePath x ++ "_split")
                   | x <- libModules pkg_descr ]
        objss <- mapM getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
                     '.':wanted_obj_ext == obj_ext ]
        return objs
  | otherwise  =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
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
     ++ (case withPackageDB lbi of
           GlobalPackageDB      -> ["-no-user-package-conf"]
           UserPackageDB        -> []
           SpecificPackageDB db -> ["-no-user-package-conf"
                                   ,"-package-conf", db])
     ++ (if splitObjs lbi then ["-split-objs"] else [])
     ++ ["-i"]
     ++ ["-i" ++ odir]
     ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
     ++ ["-i" ++ autogenModulesDir lbi]
     ++ ["-I" ++ autogenModulesDir lbi]
     ++ ["-I" ++ odir]
     ++ ["-I" ++ dir | dir <- PD.includeDirs bi]
     ++ ["-optP" ++ opt | opt <- cppOptions bi]
     ++ [ "-optP-include", "-optP"++ (autogenModulesDir lbi </> cppHeaderName) ]
     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- PD.includes bi ]
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
         ++ (if verbosity >= deafening then ["-v"] else [])
         ++ ["-c",filename])


ghcCcOptions :: LocalBuildInfo -> BuildInfo -> FilePath -> [String]
ghcCcOptions lbi bi odir
     =  ["-I" ++ dir | dir <- PD.includeDirs bi]
     ++ (case withPackageDB lbi of
             SpecificPackageDB db -> ["-package-conf", db]
             _ -> [])
     ++ concat [ ["-package", display pkg] | pkg <- packageDeps lbi ]
     ++ ["-optc" ++ opt | opt <- PD.ccOptions bi]
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
      Just ghcVersion = programVersion ghcProg
  let decls = [
        ("modules", unwords (map display (PD.exposedModules lib ++ otherModules bi))),
        ("GHC", programPath ghcProg),
        ("GHC_VERSION", (display (compilerVersion (compiler lbi)))),
        ("VANILLA_WAY", if withVanillaLib lbi then "YES" else "NO"),
        ("WAYS", (if withProfLib lbi then "p " else "") ++ (if withSharedLib lbi then "dyn" else "")),
        ("odir", builddir),
        ("package", packageIdStr),
        ("GHC_OPTS", unwords $ programArgs ghcProg
                            ++ ["-package-name", packageIdStr ]
                            ++ ghcOptions lbi bi (buildDir lbi)),
        ("MAKEFILE", file),
        ("C_SRCS", unwords (cSources bi)),
        ("GHC_CC_OPTS", unwords (ghcCcOptions lbi bi (buildDir lbi))),
        ("GHCI_LIB", if withGHCiLib lbi
                     then builddir </> mkGHCiLibName (packageId pkg_descr)
                     else ""),
        ("soext", dllExtension),
        ("LIB_LD_OPTS", unwords (["-package-name", packageIdStr]
                                 ++ concat [ ["-package", display pkg] | pkg <- packageDeps lbi ]
                                 ++ ["-l"++libName | libName <- extraLibs bi]
                                 ++ ["-L"++libDir | libDir <- extraLibDirs bi])),
        ("AR", programPath arProg),
        ("LD", programPath ldProg ++ concat [" " ++ arg | arg <- programArgs ldProg ]),
        ("GENERATE_DOT_DEPEND", if ghcVersion >= Version [6,9] []
                                then "-dep-makefile $(odir)/.depend"
                                else "-optdep-f -optdep$(odir)/.depend")
        ]
      mkRules srcdir = [
        "$(odir_)%.$(osuf) : " ++ srcdir ++ "/%.hs",
        "\t$(GHC) $(GHC_OPTS) -c $< -o $@  -ohi $(basename $@).$(hisuf)",
        "",
        "$(odir_)%.$(osuf) : " ++ srcdir ++ "/%.lhs",
        "\t$(GHC) $(GHC_OPTS) -c $< -o $@  -ohi $(basename $@).$(hisuf)",
        "",
        "$(odir_)%.$(osuf) : " ++ srcdir ++ "/%.$(way_)s",
        "\t@$(RM) $@",
        "\t$(GHC) $(GHC_CC_OPTS) -c $< -o $@",
        "",
        "$(odir_)%.$(osuf) : " ++ srcdir ++ "/%.S",
        "\t@$(RM) $@",
        "\t$(GHC) $(GHC_CC_OPTS) -c $< -o $@",
        "",
        "$(odir_)%.$(osuf)-boot : " ++ srcdir ++ "/%.hs-boot",
        "\t$(GHC) $(GHC_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi-boot",
        "",
        "$(odir_)%.$(osuf)-boot : " ++ srcdir ++ "/%.lhs-boot",
        "\t$(GHC) $(GHC_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi-boot",
        ""]
      -- We used to do this with $(eval ...) and $(call ...) in the
      -- Makefile, but make 3.79.1 (which is what comes with msys)
      -- doesn't understand $(eval ...), so now we just stick the
      -- expanded loop directly into the Makefile we generate.
      vars = ["WAY_p_OPTS = -prof",
              "WAY_dyn_OPTS = -fPIC -dynamic",
              "WAY_dyn_CC_OPTS = -fPIC",
              "",
              "ifneq \"$(way)\" \"\"",
              "way_ := $(way)_",
              "_way := _$(way)",
              "GHC_OPTS += $(WAY_$(way)_OPTS)",
              "GHC_OPTS += -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(osuf)",
              "GHC_CC_OPTS += $(WAY_$(way)_CC_OPTS)",
              "endif",
              "",
              "osuf  = $(way_)o",
              "hisuf = $(way_)hi",
              "",
              "ifneq \"$(odir)\" \"\"",
              "odir_ = $(odir)/",
              "else",
              "odir_ =",
              "endif",
              ""]
      rules = concatMap mkRules (hsSourceDirs bi)

  hPutStrLn h "# DO NOT EDIT!  Automatically generated by Cabal\n"
  hPutStrLn h $ unlines (map (\(a,b)-> a ++ " = " ++ munge b) decls)
  hPutStrLn h $ unlines vars
  hPutStrLn h makefileTemplate
  hPutStrLn h $ unlines rules
   -- put the extra suffix rules *after* the suffix rules in the template.
   -- the suffix rules in the tempate handle source files that have been
   -- preprocessed and generated into distdir, whereas the suffix rules 
   -- here point to the source dir.  We want the distdir to override the
   -- source dir, just in case the user has left a preprocessed version
   -- of a source file lying around in the source dir.  Also this matches
   -- the behaviour of 'cabal build'.
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
installExe :: CopyFlags -- ^verbosity
           -> LocalBuildInfo
           -> InstallDirs FilePath -- ^Where to copy the files to
           -> InstallDirs FilePath -- ^Where to pretend the files are (i.e. ignores --destdir)
           -> FilePath  -- ^Build location
           -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
           -> PackageDescription
           -> IO ()
installExe flags lbi installDirs pretendInstallDirs buildPref (progprefix, progsuffix) pkg_descr
    = do let verbosity = fromFlag (copyVerbosity flags)
             useWrapper = fromFlag (copyUseWrapper flags)
             binDir = bindir installDirs
         createDirectoryIfMissingVerbose verbosity True binDir
         withExe pkg_descr $ \Executable { exeName = e } -> do
             let exeFileName = e <.> exeExtension
		 exeDynFileName = e <.> "dyn" <.> exeExtension
                 fixedExeBaseName = progprefix ++ e ++ progsuffix
                 installBinary dest = do
                     copyExe verbosity
                                     (buildPref </> e </> exeFileName) (dest <.> exeExtension)
		     exists <- doesFileExist (buildPref </> e </> exeDynFileName)
		     if exists then
			   copyFileVerbose verbosity
			    (buildPref </> e </> exeDynFileName) (dest <.> "dyn" <.> exeExtension)
		        else
			    return ()
                     stripExe verbosity lbi exeFileName (dest <.> exeExtension)
             if useWrapper
                 then do
                     let libExecDir = libexecdir installDirs
                         pretendLibExecDir = libexecdir pretendInstallDirs
                         absExeFileName =
                             libExecDir </> fixedExeBaseName <.> exeExtension
                         pretendAbsExeFileName =
                             pretendLibExecDir </> fixedExeBaseName <.> exeExtension
                         wrapperFileName = binDir </> fixedExeBaseName
                         myPkgId = packageId (PD.package (localPkgDescr lbi))
                         myCompilerId = compilerId (compiler lbi)
                         env = (ExecutableNameVar,
                                toPathTemplate pretendAbsExeFileName)
                             : fullPathTemplateEnv myPkgId myCompilerId
                                                   pretendInstallDirs
                     createDirectoryIfMissingVerbose verbosity True libExecDir
                     installBinary (libExecDir </> fixedExeBaseName)
                     -- XXX Should probably look somewhere more sensible
                     -- than just . for wrappers
                     wrapperTemplate <- readFile (e <.> "wrapper")
                     let wrapper = fromPathTemplate
                                 $ substPathTemplate env
                                 $ toPathTemplate wrapperTemplate
                     writeFileAtomic wrapperFileName wrapper
                     copyPermissions absExeFileName wrapperFileName
                 else do
                     installBinary (binDir </> fixedExeBaseName)

copyExe :: Verbosity -> FilePath -> FilePath -> IO ()
copyExe verbosity src dest = do
  info verbosity ("copy " ++ src ++ " to " ++ dest)
  copyExecutableFile src dest

stripExe :: Verbosity -> LocalBuildInfo -> FilePath -> FilePath -> IO ()
stripExe verbosity lbi name path = when (stripExes lbi) $
  case lookupProgram stripProgram (withPrograms lbi) of
    Just strip -> rawSystemProgram verbosity strip args
    Nothing    -> unless (buildOS == Windows) $
                  -- Don't bother warning on windows, we don't expect them to
                  -- have the strip program anyway.
                  warn verbosity $ "Unable to strip executable '" ++ name
                                ++ "' (missing the 'strip' program)"
  where
    args = path : case buildOS of
       OSX -> ["-x"] -- By default, stripping the ghc binary on at least
                     -- some OS X installations causes:
                     --     HSbase-3.0.o: unknown symbol `_environ'"
                     -- The -x flag fixes that.
       _   -> []

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: CopyFlags -- ^verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic librarys
              -> FilePath  -- ^Build location
              -> PackageDescription -> IO ()
installLib flags lbi targetDir dynlibTargetDir builtDir
              pkg@PackageDescription{library=Just lib} =
    unless (fromFlag $ copyInPlace flags) $ do
        -- copy .hi files over:
        let verbosity = fromFlag (copyVerbosity flags)
            copy src dst n = do
              createDirectoryIfMissingVerbose verbosity True dst
              copyFileVerbose verbosity (src </> n) (dst </> n)
            copyModuleFiles ext =
                smartCopySources verbosity [builtDir] targetDir
                                 (libModules pkg) [ext]
        ifVanilla $ copyModuleFiles "hi"
        ifProf    $ copyModuleFiles "p_hi"

        -- copy the built library files over:
        ifVanilla $ copy builtDir targetDir vanillaLibName
        ifProf    $ copy builtDir targetDir profileLibName
        ifGHCi    $ copy builtDir targetDir ghciLibName
        ifShared  $ copy builtDir dynlibTargetDir sharedLibName

        -- run ranlib if necessary:
        ifVanilla $ updateLibArchive verbosity lbi
                                     (targetDir </> vanillaLibName)
        ifProf    $ updateLibArchive verbosity lbi
                                     (targetDir </> profileLibName)

  where
    vanillaLibName = mkLibName pkgid
    profileLibName = mkProfLibName pkgid
    ghciLibName    = mkGHCiLibName pkgid
    sharedLibName  = mkSharedLibName pkgid (compilerId (compiler lbi))

    pkgid          = packageId pkg

    hasLib    = not $ null (libModules pkg)
                   && null (cSources (libBuildInfo lib))
    ifVanilla = when (hasLib && withVanillaLib lbi)
    ifProf    = when (hasLib && withProfLib    lbi)
    ifGHCi    = when (hasLib && withGHCiLib    lbi)
    ifShared  = when (hasLib && withSharedLib  lbi)

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
