-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.LHC
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

module Distribution.Simple.LHC (
        configure, getInstalledPackages,
        buildLib, buildExe,
        installLib, installExe,
        registerPackage,
        ghcOptions,
        ghcVerbosityOptions
 ) where

import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), Executable(..)
         , Library(..), libModules, hcOptions, usedExtensions, allExtensions )
import Distribution.InstalledPackageInfo
                                ( InstalledPackageInfo
                                , parseInstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
                                ( InstalledPackageInfo_(..) )
import Distribution.Simple.PackageIndex
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.ParseUtils  ( ParseResult(..) )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..) )
import Distribution.Simple.InstallDirs
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Package
         ( PackageIdentifier, Package(..) )
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration, ProgArg
         , ProgramLocation(..), rawSystemProgram, rawSystemProgramConf
         , rawSystemProgramStdout, rawSystemProgramStdoutConf
         , requireProgramVersion
         , userMaybeSpecifyPath, programPath, lookupProgram, addKnownProgram
         , arProgram, ranlibProgram, ldProgram
         , gccProgram, stripProgram
         , lhcProgram, lhcPkgProgram )
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..), compilerVersion
         , OptimisationLevel(..), PackageDB(..), PackageDBStack
         , Flag, languageToFlags, extensionsToFlags )
import Distribution.Version
         ( Version(..), orLaterVersion )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Verbosity
import Distribution.Text
         ( display, simpleParse )
import Language.Haskell.Extension
         ( Language(Haskell98), Extension(..), KnownExtension(..) )

import Control.Monad            ( unless, when )
import Data.List
import Data.Maybe               ( catMaybes )
import Data.Monoid              ( Monoid(..) )
import System.Directory         ( removeFile, renameFile,
                                  getDirectoryContents, doesFileExist,
                                  getTemporaryDirectory )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension )
import System.IO (hClose, hPutStrLn)
import Distribution.Compat.Exception (catchExit, catchIO)

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf = do

  (lhcProg, lhcVersion, conf') <-
    requireProgramVersion verbosity lhcProgram
      (orLaterVersion (Version [0,7] []))
      (userMaybeSpecifyPath "lhc" hcPath conf)

  (lhcPkgProg, lhcPkgVersion, conf'') <-
    requireProgramVersion verbosity lhcPkgProgram
      (orLaterVersion (Version [0,7] []))
      (userMaybeSpecifyPath "lhc-pkg" hcPkgPath conf')

  when (lhcVersion /= lhcPkgVersion) $ die $
       "Version mismatch between lhc and lhc-pkg: "
    ++ programPath lhcProg ++ " is version " ++ display lhcVersion ++ " "
    ++ programPath lhcPkgProg ++ " is version " ++ display lhcPkgVersion

  languages  <- getLanguages  verbosity lhcProg
  extensions <- getExtensions verbosity lhcProg

  let comp = Compiler {
        compilerId             = CompilerId LHC lhcVersion,
        compilerLanguages      = languages,
        compilerExtensions     = extensions
      }
      conf''' = configureToolchain lhcProg conf'' -- configure gcc and ld
  return (comp, conf''')

-- | Adjust the way we find and configure gcc and ld
--
configureToolchain :: ConfiguredProgram -> ProgramConfiguration
                                        -> ProgramConfiguration
configureToolchain lhcProg =
    addKnownProgram gccProgram {
      programFindLocation = findProg gccProgram (baseDir </> "gcc.exe"),
      programPostConf     = configureGcc
    }
  . addKnownProgram ldProgram {
      programFindLocation = findProg ldProgram (libDir </> "ld.exe"),
      programPostConf     = configureLd
    }
  where
    compilerDir = takeDirectory (programPath lhcProg)
    baseDir     = takeDirectory compilerDir
    libDir      = baseDir </> "gcc-lib"
    includeDir  = baseDir </> "include" </> "mingw"
    isWindows   = case buildOS of Windows -> True; _ -> False

    -- on Windows finding and configuring ghc's gcc and ld is a bit special
    findProg :: Program -> FilePath -> Verbosity -> IO (Maybe FilePath)
    findProg prog location | isWindows = \verbosity -> do
        exists <- doesFileExist location
        if exists then return (Just location)
                  else do warn verbosity ("Couldn't find " ++ programName prog ++ " where I expected it. Trying the search path.")
                          programFindLocation prog verbosity
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
               rawSystemProgram verbosity lhcProg ["-c", testcfile,
                                                   "-o", testofile]
               withTempFile tempDir ".o" $ \testofile' testohnd' ->
                 do
                   hClose testohnd'
                   _ <- rawSystemProgramStdout verbosity ldProg
                     ["-x", "-r", testofile, "-o", testofile']
                   return True
                 `catchIO`   (\_ -> return False)
                 `catchExit` (\_ -> return False)
      if ldx
        then return ["-x"]
        else return []

getLanguages :: Verbosity -> ConfiguredProgram -> IO [(Language, Flag)]
getLanguages _ _ = return [(Haskell98, "")]
--FIXME: does lhc support -XHaskell98 flag? from what version?

getExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Flag)]
getExtensions verbosity lhcProg = do
    exts <- rawSystemStdout verbosity (programPath lhcProg)
              ["--supported-languages"]
    -- GHC has the annoying habit of inverting some of the extensions
    -- so we have to try parsing ("No" ++ ghcExtensionName) first
    let readExtension str = do
          ext <- simpleParse ("No" ++ str)
          case ext of
            UnknownExtension _ -> simpleParse str
            _                  -> return ext
    return $ [ (ext, "-X" ++ display ext)
             | Just ext <- map readExtension (lines exts) ]

getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity packagedbs conf = do
  checkPackageDbStack packagedbs
  pkgss <- getInstalledPackages' verbosity packagedbs conf
  let indexes = [ PackageIndex.fromList (map (substTopDir topDir) pkgs)
                | (_, pkgs) <- pkgss ]
  return $! (mconcat indexes)

  where
    -- On Windows, various fields have $topdir/foo rather than full
    -- paths. We need to substitute the right value in so that when
    -- we, for example, call gcc, we have proper paths to give it
    Just ghcProg = lookupProgram lhcProgram conf
    compilerDir  = takeDirectory (programPath ghcProg)
    topDir       = takeDirectory compilerDir

checkPackageDbStack :: PackageDBStack -> IO ()
checkPackageDbStack (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStack _ =
  die $ "GHC.getInstalledPackages: the global package db must be "
     ++ "specified first and cannot be specified multiple times"

-- | Get the packages from specific PackageDBs, not cumulative.
--
getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramConfiguration
                     -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs conf
  =
  sequence
    [ do str <- rawSystemProgramStdoutConf verbosity lhcPkgProgram conf
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

-- | Build a library with LHC.
--
buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  let pref = buildDir lbi
      pkgid = packageId pkg_descr
      runGhcProg = rawSystemProgramConf verbosity lhcProgram (withPrograms lbi)
      ifVanillaLib forceVanilla = when (forceVanilla || withVanillaLib lbi)
      ifProfLib = when (withProfLib lbi)
      ifSharedLib = when (withSharedLib lbi)
      ifGHCiLib = when (withGHCiLib lbi && withVanillaLib lbi)

  libBi <- hackThreadedFlag verbosity
             (compiler lbi) (withProfLib lbi) (libBuildInfo lib)

  let libTargetDir = pref
      forceVanillaLib = EnableExtension TemplateHaskell `elem` allExtensions libBi
      -- TH always needs vanilla libs, even when building for profiling

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recurive modules?
  let ghcArgs =
             ["-package-name", display pkgid ]
          ++ constructGHCCmdLine lbi libBi clbi libTargetDir verbosity
          ++ map display (libModules lib)
      lhcWrap x = ["--build-library", "--ghc-opts=" ++ unwords x]
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
  unless (null (libModules lib)) $
    do ifVanillaLib forceVanillaLib (runGhcProg $ lhcWrap ghcArgs)
       ifProfLib (runGhcProg $ lhcWrap ghcArgsProf)
       ifSharedLib (runGhcProg $ lhcWrap ghcArgsShared)

  -- build any C sources
  unless (null (cSources libBi)) $ do
     info verbosity "Building C Sources..."
     sequence_ [do let (odir,args) = constructCcCmdLine lbi libBi clbi pref
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
    | x <- libModules lib ]
  stubProfObjs <- fmap catMaybes $ sequence
    [ findFileWithExtension ["p_" ++ objExtension] [libTargetDir]
        (ModuleName.toFilePath x ++"_stub")
    | x <- libModules lib ]
  stubSharedObjs <- fmap catMaybes $ sequence
    [ findFileWithExtension ["dyn_" ++ objExtension] [libTargetDir]
        (ModuleName.toFilePath x ++"_stub")
    | x <- libModules lib ]

  hObjs     <- getHaskellObjects lib lbi
                    pref objExtension True
  hProfObjs <-
    if (withProfLib lbi)
            then getHaskellObjects lib lbi
                    pref ("p_" ++ objExtension) True
            else return []
  hSharedObjs <-
    if (withSharedLib lbi)
            then getHaskellObjects lib lbi
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
            ++ ghcPackageFlags lbi clbi
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


-- | Build an executable with LHC.
--
buildExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity _pkg_descr lbi
  exe@Executable { exeName = exeName', modulePath = modPath } clbi = do
  let pref = buildDir lbi
      runGhcProg = rawSystemProgramConf verbosity lhcProgram (withPrograms lbi)

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
   sequence_ [do let (odir,args) = constructCcCmdLine lbi exeBi clbi
                                          exeDir filename verbosity
                 createDirectoryIfMissingVerbose verbosity True odir
                 runGhcProg args
             | filename <- cSources exeBi]

  srcMainFile <- findFile (exeDir : hsSourceDirs exeBi) modPath

  let cObjs = map (`replaceExtension` objExtension) (cSources exeBi)
  let lhcWrap x = ("--ghc-opts\"":x) ++ ["\""]
  let binArgs linkExe profExe =
             (if linkExe
                 then ["-o", targetDir </> exeNameReal]
                 else ["-c"])
          ++ constructGHCCmdLine lbi exeBi clbi exeDir verbosity
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
  when (withProfExe lbi && EnableExtension TemplateHaskell `elem` allExtensions exeBi)
     (runGhcProg $ lhcWrap (binArgs False False))

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
getHaskellObjects :: Library -> LocalBuildInfo
                  -> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects lib lbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
        let dirs = [ pref </> (ModuleName.toFilePath x ++ "_split")
                   | x <- libModules lib ]
        objss <- mapM getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
                     '.':wanted_obj_ext == obj_ext ]
        return objs
  | otherwise  =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
               | x <- libModules lib ]


constructGHCCmdLine
        :: LocalBuildInfo
        -> BuildInfo
        -> ComponentLocalBuildInfo
        -> FilePath
        -> Verbosity
        -> [String]
constructGHCCmdLine lbi bi clbi odir verbosity =
        ["--make"]
     ++ ghcVerbosityOptions verbosity
        -- Unsupported extensions have already been checked by configure
     ++ ghcOptions lbi bi clbi odir

ghcVerbosityOptions :: Verbosity -> [String]
ghcVerbosityOptions verbosity
     | verbosity >= deafening = ["-v"]
     | verbosity >= normal    = []
     | otherwise              = ["-w", "-v0"]

ghcOptions :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
           -> FilePath -> [String]
ghcOptions lbi bi clbi odir
     =  ["-hide-all-packages"]
     ++ ghcPackageDbOptions (withPackageDB lbi)
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
     ++ ghcPackageFlags lbi clbi
     ++ (case withOptimization lbi of
           NoOptimisation      -> []
           NormalOptimisation  -> ["-O"]
           MaximumOptimisation -> ["-O2"])
     ++ hcOptions GHC bi
     ++ languageToFlags c (defaultLanguage bi)
     ++ extensionsToFlags c (usedExtensions bi)
    where c = compiler lbi

ghcPackageFlags :: LocalBuildInfo -> ComponentLocalBuildInfo -> [String]
ghcPackageFlags lbi clbi
  | ghcVer >= Version [6,11] []
              = concat [ ["-package-id", display ipkgid]
                       | (ipkgid, _) <- componentPackageDeps clbi ]

  | otherwise = concat [ ["-package", display pkgid]
                       | (_, pkgid)  <- componentPackageDeps clbi ]
    where
      ghcVer = compilerVersion (compiler lbi)

ghcPackageDbOptions :: PackageDBStack -> [String]
ghcPackageDbOptions dbstack = case dbstack of
  (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
  (GlobalPackageDB:dbs)               -> "-no-user-package-conf"
                                       : concatMap specific dbs
  _                                   -> ierror
 where
    specific (SpecificPackageDB db) = [ "-package-conf", db ]
    specific _ = ierror
    ierror     = error ("internal error: unexpected package db stack: " ++ show dbstack)

constructCcCmdLine :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                   -> FilePath -> FilePath -> Verbosity -> (FilePath,[String])
constructCcCmdLine lbi bi clbi pref filename verbosity
  =  let odir | compilerVersion (compiler lbi) >= Version [6,4,1] []  = pref
              | otherwise = pref </> takeDirectory filename
                        -- ghc 6.4.1 fixed a bug in -odir handling
                        -- for C compilations.
     in
        (odir,
         ghcCcOptions lbi bi clbi odir
         ++ (if verbosity >= deafening then ["-v"] else [])
         ++ ["-c",filename])


ghcCcOptions :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
             -> FilePath -> [String]
ghcCcOptions lbi bi clbi odir
     =  ["-I" ++ dir | dir <- PD.includeDirs bi]
     ++ ghcPackageDbOptions (withPackageDB lbi)
     ++ ghcPackageFlags lbi clbi
     ++ ["-optc" ++ opt | opt <- PD.ccOptions bi]
     ++ (case withOptimization lbi of
           NoOptimisation -> []
           _              -> ["-optc-O2"])
     ++ ["-odir", odir]

mkGHCiLibName :: PackageIdentifier -> String
mkGHCiLibName lib = "HS" ++ display lib <.> "o"

-- -----------------------------------------------------------------------------
-- Installing

-- |Install executables for GHC.
installExe :: Verbosity
           -> LocalBuildInfo
           -> InstallDirs FilePath -- ^Where to copy the files to
           -> FilePath  -- ^Build location
           -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
           -> PackageDescription
           -> Executable
           -> IO ()
installExe verbosity lbi installDirs buildPref (progprefix, progsuffix) _pkg exe = do
  let binDir = bindir installDirs
  createDirectoryIfMissingVerbose verbosity True binDir
  let exeFileName = exeName exe <.> exeExtension
      fixedExeBaseName = progprefix ++ exeName exe ++ progsuffix
      installBinary dest = do
          installExecutableFile verbosity
            (buildPref </> exeName exe </> exeFileName)
            (dest <.> exeExtension)
          stripExe verbosity lbi exeFileName (dest <.> exeExtension)
  installBinary (binDir </> fixedExeBaseName)

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
installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic librarys
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir pkg lib = do
  -- copy .hi files over:
  let copy src dst n = do
        createDirectoryIfMissingVerbose verbosity True dst
        installOrdinaryFile verbosity (src </> n) (dst </> n)
      copyModuleFiles ext =
        findModuleFiles [builtDir] [ext] (libModules lib)
          >>= installOrdinaryFiles verbosity targetDir
  ifVanilla $ copyModuleFiles "hi"
  ifProf    $ copyModuleFiles "p_hi"
  hcrFiles <- findModuleFiles (builtDir : hsSourceDirs (libBuildInfo lib)) ["hcr"] (libModules lib)
  flip mapM_ hcrFiles $ \(srcBase, srcFile) -> runLhc ["--install-library", srcBase </> srcFile]

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

    hasLib    = not $ null (libModules lib)
                   && null (cSources (libBuildInfo lib))
    ifVanilla = when (hasLib && withVanillaLib lbi)
    ifProf    = when (hasLib && withProfLib    lbi)
    ifGHCi    = when (hasLib && withGHCiLib    lbi)
    ifShared  = when (hasLib && withSharedLib  lbi)

    runLhc    = rawSystemProgramConf verbosity lhcProgram (withPrograms lbi)

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

-- -----------------------------------------------------------------------------
-- Registering

registerPackage
  :: Verbosity
  -> InstalledPackageInfo
  -> PackageDescription
  -> LocalBuildInfo
  -> Bool
  -> PackageDBStack
  -> IO ()
registerPackage verbosity installedPkgInfo _pkg lbi _inplace packageDbs = do
  let Just lhcPkg = lookupProgram lhcPkgProgram (withPrograms lbi)
  HcPkg.reregister verbosity lhcPkg packageDbs (Right installedPkgInfo)
