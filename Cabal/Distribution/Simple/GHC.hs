{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC
-- Copyright   :  Isaac Jones 2003-2007
-- License     :  BSD3
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

module Distribution.Simple.GHC (
        getGhcInfo,
        configure,
        getInstalledPackages,
        getInstalledPackagesMonitorFiles,
        getPackageDBContents,
        buildLib, buildExe,
        replLib, replExe,
        startInterpreter,
        installLib, installExe,
        libAbiHash,
        hcPkgInfo,
        registerPackage,
        componentGhcOptions,
        componentCcGhcOptions,
        getLibDir,
        isDynamic,
        getGlobalPackageDB,
        pkgRoot
 ) where

import Control.Applicative -- 7.10 -Werror workaround
import Prelude             -- https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10#GHCsaysTheimportof...isredundant

import qualified Distribution.Simple.GHC.IPI642 as IPI642
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.GHC.ImplInfo
import Distribution.PackageDescription as PD
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Package
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Program
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.Program.Ar    as Ar
import qualified Distribution.Simple.Program.Ld    as Ld
import qualified Distribution.Simple.Program.Strip as Strip
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Version
import Distribution.System
import Distribution.Verbosity
import Distribution.Text
import Distribution.Utils.NubList
import Language.Haskell.Extension

import Control.Monad            ( unless, when )
import Data.Char                ( isDigit, isSpace )
import Data.List
import qualified Data.Map as M  ( fromList, lookup )
import Data.Maybe               ( catMaybes )
import Data.Monoid as Mon       ( Monoid(..) )
import Data.Version             ( showVersion )
import System.Directory
         ( doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing
         , canonicalizePath )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension,
                                  splitExtension, isRelative )
import qualified System.Info

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration
          -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf0 = do

  (ghcProg, ghcVersion, conf1) <-
    requireProgramVersion verbosity ghcProgram
      (orLaterVersion (Version [6,4] []))
      (userMaybeSpecifyPath "ghc" hcPath conf0)
  let implInfo = ghcVersionImplInfo ghcVersion

  -- This is slightly tricky, we have to configure ghc first, then we use the
  -- location of ghc to help find ghc-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcPkgProg, ghcPkgVersion, conf2) <-
    requireProgramVersion verbosity ghcPkgProgram {
      programFindLocation = guessGhcPkgFromGhcPath ghcProg
    }
    anyVersion (userMaybeSpecifyPath "ghc-pkg" hcPkgPath conf1)

  when (ghcVersion /= ghcPkgVersion) $ die $
       "Version mismatch between ghc and ghc-pkg: "
    ++ programPath ghcProg ++ " is version " ++ display ghcVersion ++ " "
    ++ programPath ghcPkgProg ++ " is version " ++ display ghcPkgVersion

  -- Likewise we try to find the matching hsc2hs and haddock programs.
  let hsc2hsProgram' = hsc2hsProgram {
                           programFindLocation = guessHsc2hsFromGhcPath ghcProg
                       }
      haddockProgram' = haddockProgram {
                           programFindLocation = guessHaddockFromGhcPath ghcProg
                       }
      conf3 = addKnownProgram haddockProgram' $
              addKnownProgram hsc2hsProgram' conf2

  languages  <- Internal.getLanguages verbosity implInfo ghcProg
  extensions0 <- Internal.getExtensions verbosity implInfo ghcProg

  ghcInfo <- Internal.getGhcInfo verbosity implInfo ghcProg
  let ghcInfoMap = M.fromList ghcInfo

      -- starting with GHC 8.0, `TemplateHaskell` will be omitted from
      -- `--supported-extensions` when it's not available.
      -- for older GHCs we can use the "Have interpreter" property to
      -- filter out `TemplateHaskell`
      extensions | ghcVersion < Version [8] []
                 , Just "NO" <- M.lookup "Have interpreter" ghcInfoMap
                   = filter ((/= EnableExtension TemplateHaskell) . fst) extensions0
                 | otherwise = extensions0

  let comp = Compiler {
        compilerId         = CompilerId GHC ghcVersion,
        compilerAbiTag     = NoAbiTag,
        compilerCompat     = [],
        compilerLanguages  = languages,
        compilerExtensions = extensions,
        compilerProperties = ghcInfoMap
      }
      compPlatform = Internal.targetPlatform ghcInfo
      -- configure gcc and ld
      conf4 = Internal.configureToolchain implInfo ghcProg ghcInfoMap conf3
  return (comp, compPlatform, conf4)

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find
-- the corresponding tool; e.g. if the tool is ghc-pkg, we try looking
-- for a versioned or unversioned ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-ghc-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
--
guessToolFromGhcPath :: Program -> ConfiguredProgram
                     -> Verbosity -> ProgramSearchPath
                     -> IO (Maybe (FilePath, [FilePath]))
guessToolFromGhcPath tool ghcProg verbosity searchpath
  = do let toolname          = programName tool
           given_path        = programPath ghcProg
           given_dir         = takeDirectory given_path
       real_path <- canonicalizePath given_path
       let real_dir           = takeDirectory real_path
           versionSuffix path = takeVersionSuffix (dropExeExtension path)
           guessNormal       dir = dir </> toolname <.> exeExtension
           guessGhcVersioned dir = dir </> (toolname ++ "-ghc" ++ versionSuffix dir)
                                       <.> exeExtension
           guessVersioned    dir = dir </> (toolname ++ versionSuffix dir)
                                       <.> exeExtension
           mkGuesses dir | null (versionSuffix dir) = [guessNormal dir]
                         | otherwise                = [guessGhcVersioned dir,
                                                       guessVersioned dir,
                                                       guessNormal dir]
           guesses = mkGuesses given_dir ++
                            if real_path == given_path
                                then []
                                else mkGuesses real_dir
       info verbosity $ "looking for tool " ++ toolname
         ++ " near compiler in " ++ given_dir
       exists <- mapM doesFileExist guesses
       case [ file | (file, True) <- zip guesses exists ] of
                   -- If we can't find it near ghc, fall back to the usual
                   -- method.
         []     -> programFindLocation tool verbosity searchpath
         (fp:_) -> do info verbosity $ "found " ++ toolname ++ " in " ++ fp
                      let lookedAt = map fst
                                   . takeWhile (\(_file, exist) -> not exist)
                                   $ zip guesses exists
                      return (Just (fp, lookedAt))

  where takeVersionSuffix :: FilePath -> String
        takeVersionSuffix = takeWhileEndLE isSuffixChar

        isSuffixChar :: Char -> Bool
        isSuffixChar c = isDigit c || c == '.' || c == '-'

        dropExeExtension :: FilePath -> FilePath
        dropExeExtension filepath =
          case splitExtension filepath of
            (filepath', extension) | extension == exeExtension -> filepath'
                                   | otherwise                 -> filepath

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding ghc-pkg, we try looking for both a versioned and unversioned
-- ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-ghc-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
--
guessGhcPkgFromGhcPath :: ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe (FilePath, [FilePath]))
guessGhcPkgFromGhcPath = guessToolFromGhcPath ghcPkgProgram

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding hsc2hs, we try looking for both a versioned and unversioned
-- hsc2hs in the same dir, that is:
--
-- > /usr/local/bin/hsc2hs-ghc-6.6.1(.exe)
-- > /usr/local/bin/hsc2hs-6.6.1(.exe)
-- > /usr/local/bin/hsc2hs(.exe)
--
guessHsc2hsFromGhcPath :: ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe (FilePath, [FilePath]))
guessHsc2hsFromGhcPath = guessToolFromGhcPath hsc2hsProgram

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding haddock, we try looking for both a versioned and unversioned
-- haddock in the same dir, that is:
--
-- > /usr/local/bin/haddock-ghc-6.6.1(.exe)
-- > /usr/local/bin/haddock-6.6.1(.exe)
-- > /usr/local/bin/haddock(.exe)
--
guessHaddockFromGhcPath :: ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe (FilePath, [FilePath]))
guessHaddockFromGhcPath = guessToolFromGhcPath haddockProgram

getGhcInfo :: Verbosity -> ConfiguredProgram -> IO [(String, String)]
getGhcInfo verbosity ghcProg = Internal.getGhcInfo verbosity implInfo ghcProg
  where
    Just version = programVersion ghcProg
    implInfo = ghcVersionImplInfo version

-- | Given a single package DB, return all installed packages.
getPackageDBContents :: Verbosity -> PackageDB -> ProgramConfiguration
                        -> IO InstalledPackageIndex
getPackageDBContents verbosity packagedb conf = do
  pkgss <- getInstalledPackages' verbosity [packagedb] conf
  toPackageIndex verbosity pkgss conf

-- | Given a package DB stack, return all installed packages.
getInstalledPackages :: Verbosity -> Compiler -> PackageDBStack -> ProgramConfiguration
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity comp packagedbs conf = do
  checkPackageDbEnvVar
  checkPackageDbStack comp packagedbs
  pkgss <- getInstalledPackages' verbosity packagedbs conf
  index <- toPackageIndex verbosity pkgss conf
  return $! hackRtsPackage index

  where
    hackRtsPackage index =
      case PackageIndex.lookupPackageName index (PackageName "rts") of
        [(_,[rts])]
           -> PackageIndex.insert (removeMingwIncludeDir rts) index
        _  -> index -- No (or multiple) ghc rts package is registered!!
                    -- Feh, whatever, the ghc test suite does some crazy stuff.

-- | Given a list of @(PackageDB, InstalledPackageInfo)@ pairs, produce a
-- @PackageIndex@. Helper function used by 'getPackageDBContents' and
-- 'getInstalledPackages'.
toPackageIndex :: Verbosity
               -> [(PackageDB, [InstalledPackageInfo])]
               -> ProgramConfiguration
               -> IO InstalledPackageIndex
toPackageIndex verbosity pkgss conf = do
  -- On Windows, various fields have $topdir/foo rather than full
  -- paths. We need to substitute the right value in so that when
  -- we, for example, call gcc, we have proper paths to give it.
  topDir <- getLibDir' verbosity ghcProg
  let indices = [ PackageIndex.fromList (map (Internal.substTopDir topDir) pkgs)
                | (_, pkgs) <- pkgss ]
  return $! mconcat indices

  where
    Just ghcProg = lookupProgram ghcProgram conf

getLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getLibDir verbosity lbi =
    dropWhileEndLE isSpace `fmap`
     rawSystemProgramStdoutConf verbosity ghcProgram
     (withPrograms lbi) ["--print-libdir"]

getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
getLibDir' verbosity ghcProg =
    dropWhileEndLE isSpace `fmap`
     rawSystemProgramStdout verbosity ghcProg ["--print-libdir"]


-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity ghcProg =
    dropWhileEndLE isSpace `fmap`
     rawSystemProgramStdout verbosity ghcProg ["--print-global-package-db"]

-- | Return the 'FilePath' to the per-user GHC package database.
getUserPackageDB :: Verbosity -> ConfiguredProgram -> Platform -> IO FilePath
getUserPackageDB _verbosity ghcProg (Platform arch os) = do
    -- It's rather annoying that we have to reconstruct this, because ghc
    -- hides this information from us otherwise. But for certain use cases
    -- like change monitoring it really can't remain hidden.
    appdir <- getAppUserDataDirectory "ghc"
    return (appdir </> platformAndVersion </> packageConfFileName)
  where
    platformAndVersion = intercalate "-" [ Internal.showArchString arch
                                         , Internal.showOsString os
                                         , display ghcVersion ]
    packageConfFileName
      | ghcVersion >= Version [6,12] []  = "package.conf.d"
      | otherwise                        = "package.conf"
    Just ghcVersion = programVersion ghcProg

checkPackageDbEnvVar :: IO ()
checkPackageDbEnvVar =
    Internal.checkPackageDbEnvVar "GHC" "GHC_PACKAGE_PATH"

checkPackageDbStack :: Compiler -> PackageDBStack -> IO ()
checkPackageDbStack comp = if flagPackageConf implInfo
                              then checkPackageDbStackPre76
                              else checkPackageDbStackPost76
  where implInfo = ghcVersionImplInfo (compilerVersion comp)

checkPackageDbStackPost76 :: PackageDBStack -> IO ()
checkPackageDbStackPost76 (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStackPost76 rest
  | GlobalPackageDB `elem` rest =
  die $ "If the global package db is specified, it must be "
     ++ "specified first and cannot be specified multiple times"
checkPackageDbStackPost76 _ = return ()

checkPackageDbStackPre76 :: PackageDBStack -> IO ()
checkPackageDbStackPre76 (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStackPre76 rest
  | GlobalPackageDB `notElem` rest =
  die $ "With current ghc versions the global package db is always used "
     ++ "and must be listed first. This ghc limitation is lifted in GHC 7.6,"
     ++ "see http://hackage.haskell.org/trac/ghc/ticket/5977"
checkPackageDbStackPre76 _ =
  die $ "If the global package db is specified, it must be "
     ++ "specified first and cannot be specified multiple times"

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
    [ do pkgs <- HcPkg.dump (hcPkgInfo conf) verbosity packagedb
         return (packagedb, pkgs)
    | packagedb <- packagedbs ]

  where
    Just ghcProg    = lookupProgram ghcProgram conf
    Just ghcVersion = programVersion ghcProg

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
      -- We dropped support for 6.4.2 and earlier.
      | otherwise
      = \file _ -> failToRead file
    Just ghcProg = lookupProgram ghcProgram conf
    Just ghcVersion = programVersion ghcProg
    failToRead file = die $ "cannot read ghc package database " ++ file

getInstalledPackagesMonitorFiles :: Verbosity -> Platform
                                 -> ProgramConfiguration
                                 -> [PackageDB]
                                 -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity platform progdb =
    mapM getPackageDBPath
  where
    getPackageDBPath :: PackageDB -> IO FilePath
    getPackageDBPath GlobalPackageDB =
      selectMonitorFile =<< getGlobalPackageDB verbosity ghcProg

    getPackageDBPath UserPackageDB =
      selectMonitorFile =<< getUserPackageDB verbosity ghcProg platform

    getPackageDBPath (SpecificPackageDB path) = selectMonitorFile path

    -- GHC has old style file dbs, and new style directory dbs.
    -- Note that for dir style dbs, we only need to monitor the cache file, not
    -- the whole directory. The ghc program itself only reads the cache file
    -- so it's safe to only monitor this one file.
    selectMonitorFile path = do
      isFileStyle <- doesFileExist path
      if isFileStyle then return path
                     else return (path </> "package.cache")

    Just ghcProg = lookupProgram ghcProgram progdb


-- -----------------------------------------------------------------------------
-- Building

-- | Build a library with GHC.
--
buildLib, replLib :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib = buildOrReplLib False
replLib  = buildOrReplLib True

buildOrReplLib :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Library            -> ComponentLocalBuildInfo -> IO ()
buildOrReplLib forRepl verbosity numJobs pkg_descr lbi lib clbi = do
  let libName = componentUnitId clbi
      libTargetDir
        | componentUnitId clbi == localUnitId lbi = buildDir lbi
        | otherwise = buildDir lbi </> display libName
      whenVanillaLib forceVanilla =
        when (forceVanilla || withVanillaLib lbi)
      whenProfLib = when (withProfLib lbi)
      whenSharedLib forceShared =
        when (forceShared || withSharedLib lbi)
      whenGHCiLib = when (withGHCiLib lbi && withVanillaLib lbi)
      ifReplLib = when forRepl
      comp = compiler lbi
      ghcVersion = compilerVersion comp
      implInfo  = getImplInfo comp
      (Platform _hostArch hostOS) = hostPlatform lbi

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let runGhcProg = runGHC verbosity ghcProg comp

  libBi <- hackThreadedFlag verbosity
             comp (withProfLib lbi) (libBuildInfo lib)

  let isGhcDynamic        = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions libBi
      forceVanillaLib = doingTH && not isGhcDynamic
      forceSharedLib  = doingTH &&     isGhcDynamic
      -- TH always needs default libs, even when building for profiling

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = fromFlag $ configCoverage $ configFlags lbi
      -- Component name. Not 'libName' because that has the "HS" prefix
      -- that GHC gives Haskell libraries.
      cname = display $ PD.package $ localPkgDescr lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | forRepl = Mon.mempty  -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way cname
        | otherwise = mempty

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let cObjs       = map (`replaceExtension` objExtension) (cSources libBi)
      baseOpts    = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      vanillaOpts = baseOpts `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptNumJobs      = numJobs,
                      ghcOptInputModules = toNubListR $ libModules lib,
                      ghcOptHPCDir       = hpcdir Hpc.Vanilla
                    }

      profOpts    = vanillaOpts `mappend` mempty {
                      ghcOptProfilingMode = toFlag True,
                      ghcOptProfilingAuto = Internal.profDetailLevelFlag True
                                              (withProfLibDetail lbi),
                      ghcOptHiSuffix      = toFlag "p_hi",
                      ghcOptObjSuffix     = toFlag "p_o",
                      ghcOptExtra         = toNubListR $ hcProfOptions GHC libBi,
                      ghcOptHPCDir        = hpcdir Hpc.Prof
                    }

      sharedOpts  = vanillaOpts `mappend` mempty {
                      ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                      ghcOptFPic        = toFlag True,
                      ghcOptHiSuffix    = toFlag "dyn_hi",
                      ghcOptObjSuffix   = toFlag "dyn_o",
                      ghcOptExtra       = toNubListR $ hcSharedOptions GHC libBi,
                      ghcOptHPCDir      = hpcdir Hpc.Dyn
                    }
      linkerOpts = mempty {
                      ghcOptLinkOptions    = toNubListR $ PD.ldOptions libBi,
                      ghcOptLinkLibs       = toNubListR $ extraLibs libBi,
                      ghcOptLinkLibPath    = toNubListR $ extraLibDirs libBi,
                      ghcOptLinkFrameworks = toNubListR $ PD.frameworks libBi,
                      ghcOptInputFiles     = toNubListR
                                             [libTargetDir </> x | x <- cObjs]
                   }
      replOpts    = vanillaOpts {
                      ghcOptExtra        = overNubListR
                                           Internal.filterGhciFlags $
                                           ghcOptExtra vanillaOpts,
                      ghcOptNumJobs      = mempty
                    }
                    `mappend` linkerOpts
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeInteractive,
                      ghcOptOptimisation = toFlag GhcNoOptimisation
                    }

      vanillaSharedOpts = vanillaOpts `mappend` mempty {
                      ghcOptDynLinkMode  = toFlag GhcStaticAndDynamic,
                      ghcOptDynHiSuffix  = toFlag "dyn_hi",
                      ghcOptDynObjSuffix = toFlag "dyn_o",
                      ghcOptHPCDir       = hpcdir Hpc.Dyn
                    }

  unless (forRepl || null (libModules lib)) $
    do let vanilla = whenVanillaLib forceVanillaLib (runGhcProg vanillaOpts)
           shared  = whenSharedLib  forceSharedLib  (runGhcProg sharedOpts)
           useDynToo = dynamicTooSupported &&
                       (forceVanillaLib || withVanillaLib lbi) &&
                       (forceSharedLib  || withSharedLib  lbi) &&
                       null (hcSharedOptions GHC libBi)
       if useDynToo
          then do
              runGhcProg vanillaSharedOpts
              case (hpcdir Hpc.Dyn, hpcdir Hpc.Vanilla) of
                (Cabal.Flag dynDir, Cabal.Flag vanillaDir) ->
                    -- When the vanilla and shared library builds are done
                    -- in one pass, only one set of HPC module interfaces
                    -- are generated. This set should suffice for both
                    -- static and dynamically linked executables. We copy
                    -- the modules interfaces so they are available under
                    -- both ways.
                    copyDirectoryRecursive verbosity dynDir vanillaDir
                _ -> return ()
          else if isGhcDynamic
            then do shared;  vanilla
            else do vanilla; shared
       whenProfLib (runGhcProg profOpts)

  -- build any C sources
  unless (null (cSources libBi)) $ do
    info verbosity "Building C Sources..."
    sequence_
      [ do let baseCcOpts    = Internal.componentCcGhcOptions verbosity implInfo
                               lbi libBi clbi libTargetDir filename
               vanillaCcOpts = if isGhcDynamic
                               -- Dynamic GHC requires C sources to be built
                               -- with -fPIC for REPL to work. See #2207.
                               then baseCcOpts { ghcOptFPic = toFlag True }
                               else baseCcOpts
               profCcOpts    = vanillaCcOpts `mappend` mempty {
                                 ghcOptProfilingMode = toFlag True,
                                 ghcOptObjSuffix     = toFlag "p_o"
                               }
               sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                                 ghcOptFPic        = toFlag True,
                                 ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                 ghcOptObjSuffix   = toFlag "dyn_o"
                               }
               odir          = fromFlag (ghcOptObjDir vanillaCcOpts)
           createDirectoryIfMissingVerbose verbosity True odir
           let runGhcProgIfNeeded ccOpts = do
                 needsRecomp <- checkNeedsRecompilation filename ccOpts
                 when needsRecomp $ runGhcProg ccOpts
           runGhcProgIfNeeded vanillaCcOpts
           unless forRepl $
             whenSharedLib forceSharedLib (runGhcProgIfNeeded sharedCcOpts)
           unless forRepl $ whenProfLib (runGhcProgIfNeeded profCcOpts)
      | filename <- cSources libBi]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.

  ifReplLib $ do
    when (null (libModules lib)) $ warn verbosity "No exposed modules"
    ifReplLib (runGhcProg replOpts)

  -- link:
  unless forRepl $ do
    info verbosity "Linking..."
    let cProfObjs   = map (`replaceExtension` ("p_" ++ objExtension))
                      (cSources libBi)
        cSharedObjs = map (`replaceExtension` ("dyn_" ++ objExtension))
                      (cSources libBi)
        cid = compilerId (compiler lbi)
        vanillaLibFilePath = libTargetDir </> mkLibName           libName
        profileLibFilePath = libTargetDir </> mkProfLibName       libName
        sharedLibFilePath  = libTargetDir </> mkSharedLibName cid libName
        ghciLibFilePath    = libTargetDir </> Internal.mkGHCiLibName libName
        libInstallPath = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
        sharedLibInstallPath = libInstallPath </> mkSharedLibName cid libName

    stubObjs <- catMaybes <$> sequence
      [ findFileWithExtension [objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < Version [7,2] [] -- ghc-7.2+ does not make _stub.o files
      , x <- libModules lib ]
    stubProfObjs <- catMaybes <$> sequence
      [ findFileWithExtension ["p_" ++ objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < Version [7,2] [] -- ghc-7.2+ does not make _stub.o files
      , x <- libModules lib ]
    stubSharedObjs <- catMaybes <$> sequence
      [ findFileWithExtension ["dyn_" ++ objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < Version [7,2] [] -- ghc-7.2+ does not make _stub.o files
      , x <- libModules lib ]

    hObjs     <- Internal.getHaskellObjects implInfo lib lbi
                      libTargetDir objExtension True
    hProfObjs <-
      if withProfLib lbi
              then Internal.getHaskellObjects implInfo lib lbi
                      libTargetDir ("p_" ++ objExtension) True
              else return []
    hSharedObjs <-
      if withSharedLib lbi
              then Internal.getHaskellObjects implInfo lib lbi
                      libTargetDir ("dyn_" ++ objExtension) False
              else return []

    unless (null hObjs && null cObjs && null stubObjs) $ do
      rpaths <- getRPaths lbi clbi

      let staticObjectFiles =
                 hObjs
              ++ map (libTargetDir </>) cObjs
              ++ stubObjs
          profObjectFiles =
                 hProfObjs
              ++ map (libTargetDir </>) cProfObjs
              ++ stubProfObjs
          ghciObjFiles =
                 hObjs
              ++ map (libTargetDir </>) cObjs
              ++ stubObjs
          dynamicObjectFiles =
                 hSharedObjs
              ++ map (libTargetDir </>) cSharedObjs
              ++ stubSharedObjs
          -- After the relocation lib is created we invoke ghc -shared
          -- with the dependencies spelled out as -package arguments
          -- and ghc invokes the linker with the proper library paths
          ghcSharedLinkArgs =
              mempty {
                ghcOptShared             = toFlag True,
                ghcOptDynLinkMode        = toFlag GhcDynamicOnly,
                ghcOptInputFiles         = toNubListR dynamicObjectFiles,
                ghcOptOutputFile         = toFlag sharedLibFilePath,
                -- For dynamic libs, Mac OS/X needs to know the install location
                -- at build time. This only applies to GHC < 7.8 - see the
                -- discussion in #1660.
                ghcOptDylibName          = if hostOS == OSX
                                              && ghcVersion < Version [7,8] []
                                            then toFlag sharedLibInstallPath
                                            else mempty,
                ghcOptNoAutoLinkPackages = toFlag True,
                ghcOptPackageDBs         = withPackageDB lbi,
                ghcOptPackages           = toNubListR $
                                           Internal.mkGhcOptPackages clbi ,
                ghcOptLinkLibs           = toNubListR $ extraLibs libBi,
                ghcOptLinkLibPath        = toNubListR $ extraLibDirs libBi,
                ghcOptLinkFrameworks     = toNubListR $ PD.frameworks libBi,
                ghcOptRPaths             = rpaths
              }

      info verbosity (show (ghcOptPackages ghcSharedLinkArgs))

      whenVanillaLib False $
        Ar.createArLibArchive verbosity lbi vanillaLibFilePath staticObjectFiles

      whenProfLib $
        Ar.createArLibArchive verbosity lbi profileLibFilePath profObjectFiles

      whenGHCiLib $ do
        (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
        Ld.combineObjectFiles verbosity ldProg
          ghciLibFilePath ghciObjFiles

      whenSharedLib False $
        runGhcProg ghcSharedLinkArgs

-- | Start a REPL without loading any source files.
startInterpreter :: Verbosity -> ProgramConfiguration -> Compiler
                 -> PackageDBStack -> IO ()
startInterpreter verbosity conf comp packageDBs = do
  let replOpts = mempty {
        ghcOptMode       = toFlag GhcModeInteractive,
        ghcOptPackageDBs = packageDBs
        }
  checkPackageDbStack comp packageDBs
  (ghcProg, _) <- requireProgram verbosity ghcProgram conf
  runGHC verbosity ghcProg comp replOpts

-- | Build an executable with GHC.
--
buildExe, replExe :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe = buildOrReplExe False
replExe  = buildOrReplExe True

buildOrReplExe :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildOrReplExe forRepl verbosity numJobs _pkg_descr lbi
  exe@Executable { exeName = exeName', modulePath = modPath } clbi = do

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let comp       = compiler lbi
      implInfo   = getImplInfo comp
      runGhcProg = runGHC verbosity ghcProg comp

  exeBi <- hackThreadedFlag verbosity
             comp (withProfExe lbi) (buildInfo exe)

  -- exeNameReal, the name that GHC really uses (with .exe on Windows)
  let exeNameReal = exeName' <.>
                    (if takeExtension exeName' /= ('.':exeExtension)
                       then exeExtension
                       else "")

  let targetDir = buildDir lbi </> exeName'
  let exeDir    = targetDir </> (exeName' ++ "-tmp")
  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True exeDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = fromFlag $ configCoverage $ configFlags lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | forRepl = mempty  -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way exeName'
        | otherwise = mempty

  -- build executables

  srcMainFile         <- findFile (exeDir : hsSourceDirs exeBi) modPath
  rpaths              <- getRPaths lbi clbi

  let isGhcDynamic        = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      isHaskellMain = elem (takeExtension srcMainFile) [".hs", ".lhs"]
      cSrcs         = cSources exeBi ++ [srcMainFile | not isHaskellMain]
      cObjs         = map (`replaceExtension` objExtension) cSrcs
      baseOpts   = (componentGhcOptions verbosity lbi exeBi clbi exeDir)
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptInputFiles   = toNubListR
                        [ srcMainFile | isHaskellMain],
                      ghcOptInputModules = toNubListR
                        [ m | not isHaskellMain, m <- exeModules exe]
                    }
      staticOpts = baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticOnly,
                      ghcOptHPCDir         = hpcdir Hpc.Vanilla
                   }
      profOpts   = baseOpts `mappend` mempty {
                      ghcOptProfilingMode  = toFlag True,
                      ghcOptProfilingAuto  = Internal.profDetailLevelFlag False
                                               (withProfExeDetail lbi),
                      ghcOptHiSuffix       = toFlag "p_hi",
                      ghcOptObjSuffix      = toFlag "p_o",
                      ghcOptExtra          = toNubListR (hcProfOptions GHC exeBi),
                      ghcOptHPCDir         = hpcdir Hpc.Prof
                    }
      dynOpts    = baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcDynamicOnly,
                      ghcOptHiSuffix       = toFlag "dyn_hi",
                      ghcOptObjSuffix      = toFlag "dyn_o",
                      ghcOptExtra          = toNubListR $
                                             hcSharedOptions GHC exeBi,
                      ghcOptHPCDir         = hpcdir Hpc.Dyn
                    }
      dynTooOpts = staticOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticAndDynamic,
                      ghcOptDynHiSuffix    = toFlag "dyn_hi",
                      ghcOptDynObjSuffix   = toFlag "dyn_o",
                      ghcOptHPCDir         = hpcdir Hpc.Dyn
                    }
      linkerOpts = mempty {
                      ghcOptLinkOptions    = toNubListR $ PD.ldOptions exeBi,
                      ghcOptLinkLibs       = toNubListR $ extraLibs exeBi,
                      ghcOptLinkLibPath    = toNubListR $ extraLibDirs exeBi,
                      ghcOptLinkFrameworks = toNubListR $ PD.frameworks exeBi,
                      ghcOptInputFiles     = toNubListR
                                             [exeDir </> x | x <- cObjs]
                    }
      dynLinkerOpts = mempty {
                      ghcOptRPaths         = rpaths
                   }
      replOpts   = baseOpts {
                      ghcOptExtra          = overNubListR
                                             Internal.filterGhciFlags
                                             (ghcOptExtra baseOpts)
                   }
                   -- For a normal compile we do separate invocations of ghc for
                   -- compiling as for linking. But for repl we have to do just
                   -- the one invocation, so that one has to include all the
                   -- linker stuff too, like -l flags and any .o files from C
                   -- files etc.
                   `mappend` linkerOpts
                   `mappend` mempty {
                      ghcOptMode           = toFlag GhcModeInteractive,
                      ghcOptOptimisation   = toFlag GhcNoOptimisation
                   }
      commonOpts  | withProfExe lbi = profOpts
                  | withDynExe  lbi = dynOpts
                  | otherwise       = staticOpts
      compileOpts | useDynToo = dynTooOpts
                  | otherwise = commonOpts
      withStaticExe = (not $ withProfExe lbi) && (not $ withDynExe lbi)

      -- For building exe's that use TH with -prof or -dynamic we actually have
      -- to build twice, once without -prof/-dynamic and then again with
      -- -prof/-dynamic. This is because the code that TH needs to run at
      -- compile time needs to be the vanilla ABI so it can be loaded up and run
      -- by the compiler.
      -- With dynamic-by-default GHC the TH object files loaded at compile-time
      -- need to be .dyn_o instead of .o.
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions exeBi
      -- Should we use -dynamic-too instead of compiling twice?
      useDynToo = dynamicTooSupported && isGhcDynamic
                  && doingTH && withStaticExe
                  && null (hcSharedOptions GHC exeBi)
      compileTHOpts | isGhcDynamic = dynOpts
                    | otherwise    = staticOpts
      compileForTH
        | forRepl      = False
        | useDynToo    = False
        | isGhcDynamic = doingTH && (withProfExe lbi || withStaticExe)
        | otherwise    = doingTH && (withProfExe lbi || withDynExe lbi)

      linkOpts = commonOpts `mappend`
                 linkerOpts `mappend`
                 mempty { ghcOptLinkNoHsMain   = toFlag (not isHaskellMain) } `mappend`
                 (if withDynExe lbi then dynLinkerOpts else mempty)

  -- Build static/dynamic object files for TH, if needed.
  when compileForTH $
    runGhcProg compileTHOpts { ghcOptNoLink  = toFlag True
                             , ghcOptNumJobs = numJobs }

  unless forRepl $
    runGhcProg compileOpts { ghcOptNoLink  = toFlag True
                           , ghcOptNumJobs = numJobs }

  -- build any C sources
  unless (null cSrcs) $ do
   info verbosity "Building C Sources..."
   sequence_
     [ do let opts = (Internal.componentCcGhcOptions verbosity implInfo lbi exeBi
                         clbi exeDir filename) `mappend` mempty {
                       ghcOptDynLinkMode   = toFlag (if withDynExe lbi
                                                       then GhcDynamicOnly
                                                       else GhcStaticOnly),
                       ghcOptProfilingMode = toFlag (withProfExe lbi)
                     }
              odir  = fromFlag (ghcOptObjDir opts)
          createDirectoryIfMissingVerbose verbosity True odir
          needsRecomp <- checkNeedsRecompilation filename opts
          when needsRecomp $
            runGhcProg opts
     | filename <- cSrcs ]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  when forRepl $ runGhcProg replOpts

  -- link:
  unless forRepl $ do
    info verbosity "Linking..."
    runGhcProg linkOpts { ghcOptOutputFile = toFlag (targetDir </> exeNameReal) }

-- | Returns True if the modification date of the given source file is newer than
-- the object file we last compiled for it, or if no object file exists yet.
checkNeedsRecompilation :: FilePath -> GhcOptions -> IO Bool
checkNeedsRecompilation filename opts = filename `moreRecentFile` oname
    where oname = getObjectFileName filename opts

-- | Finds the object file name of the given source file
getObjectFileName :: FilePath -> GhcOptions -> FilePath
getObjectFileName filename opts = oname
    where odir  = fromFlag (ghcOptObjDir opts)
          oext  = fromFlagOrDefault "o" (ghcOptObjSuffix opts)
          oname = odir </> replaceExtension filename oext

-- | Calculate the RPATHs for the component we are building.
--
-- Calculates relative RPATHs when 'relocatable' is set.
getRPaths :: LocalBuildInfo
          -> ComponentLocalBuildInfo -- ^ Component we are building
          -> IO (NubListR FilePath)
getRPaths lbi clbi | supportRPaths hostOS = do
    libraryPaths <- depLibraryPaths False (relocatable lbi) lbi clbi
    let hostPref = case hostOS of
                     OSX -> "@loader_path"
                     _   -> "$ORIGIN"
        relPath p = if isRelative p then hostPref </> p else p
        rpaths    = toNubListR (map relPath libraryPaths)
    return rpaths
  where
    (Platform _ hostOS) = hostPlatform lbi

    -- The list of RPath-supported operating systems below reflects the
    -- platforms on which Cabal's RPATH handling is tested. It does _NOT_
    -- reflect whether the OS supports RPATH.

    -- E.g. when this comment was written, the *BSD operating systems were
    -- untested with regards to Cabal RPATH handling, and were hence set to
    -- 'False', while those operating systems themselves do support RPATH.
    supportRPaths Linux       = True
    supportRPaths Windows     = False
    supportRPaths OSX         = True
    supportRPaths FreeBSD     = False
    supportRPaths OpenBSD     = False
    supportRPaths NetBSD      = False
    supportRPaths DragonFly   = False
    supportRPaths Solaris     = False
    supportRPaths AIX         = False
    supportRPaths HPUX        = False
    supportRPaths IRIX        = False
    supportRPaths HaLVM       = False
    supportRPaths IOS         = False
    supportRPaths Android     = False
    supportRPaths Ghcjs       = False
    supportRPaths Hurd        = False
    supportRPaths (OtherOS _) = False
    -- Do _not_ add a default case so that we get a warning here when a new OS
    -- is added.

getRPaths _ _ = return mempty

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


-- | Extracts a String representing a hash of the ABI of a built
-- library.  It can fail if the library has not yet been built.
--
libAbiHash :: Verbosity -> PackageDescription -> LocalBuildInfo
           -> Library -> ComponentLocalBuildInfo -> IO String
libAbiHash verbosity _pkg_descr lbi lib clbi = do
  libBi <- hackThreadedFlag verbosity
             (compiler lbi) (withProfLib lbi) (libBuildInfo lib)
  let
      comp        = compiler lbi
      vanillaArgs =
        (componentGhcOptions verbosity lbi libBi clbi (buildDir lbi))
        `mappend` mempty {
          ghcOptMode         = toFlag GhcModeAbiHash,
          ghcOptInputModules = toNubListR $ exposedModules lib
        }
      sharedArgs = vanillaArgs `mappend` mempty {
                       ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                       ghcOptFPic        = toFlag True,
                       ghcOptHiSuffix    = toFlag "dyn_hi",
                       ghcOptObjSuffix   = toFlag "dyn_o",
                       ghcOptExtra       = toNubListR $ hcSharedOptions GHC libBi
                   }
      profArgs   = vanillaArgs `mappend` mempty {
                     ghcOptProfilingMode = toFlag True,
                     ghcOptProfilingAuto = Internal.profDetailLevelFlag True
                                             (withProfLibDetail lbi),
                     ghcOptHiSuffix      = toFlag "p_hi",
                     ghcOptObjSuffix     = toFlag "p_o",
                     ghcOptExtra         = toNubListR $ hcProfOptions GHC libBi
                   }
      ghcArgs
        | withVanillaLib lbi = vanillaArgs
        | withSharedLib lbi = sharedArgs
        | withProfLib lbi = profArgs
        | otherwise = error "libAbiHash: Can't find an enabled library way"

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  hash <- getProgramInvocationOutput verbosity
          (ghcInvocation ghcProg comp ghcArgs)
  return (takeWhile (not . isSpace) hash)

componentGhcOptions :: Verbosity -> LocalBuildInfo
                    -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                    -> GhcOptions
componentGhcOptions = Internal.componentGhcOptions

componentCcGhcOptions :: Verbosity -> LocalBuildInfo
                      -> BuildInfo -> ComponentLocalBuildInfo
                      -> FilePath -> FilePath
                      -> GhcOptions
componentCcGhcOptions verbosity lbi =
    Internal.componentCcGhcOptions verbosity implInfo lbi
  where
    comp     = compiler lbi
    implInfo = getImplInfo comp

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
installExe verbosity lbi installDirs buildPref
  (progprefix, progsuffix) _pkg exe = do
  let binDir = bindir installDirs
  createDirectoryIfMissingVerbose verbosity True binDir
  let exeFileName = exeName exe <.> exeExtension
      fixedExeBaseName = progprefix ++ exeName exe ++ progsuffix
      installBinary dest = do
          installExecutableFile verbosity
            (buildPref </> exeName exe </> exeFileName)
            (dest <.> exeExtension)
          when (stripExes lbi) $
            Strip.stripExe verbosity (hostPlatform lbi) (withPrograms lbi)
                           (dest <.> exeExtension)
  installBinary (binDir </> fixedExeBaseName)

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic libraries
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> ComponentLocalBuildInfo
              -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir _pkg lib clbi = do
  -- copy .hi files over:
  whenVanilla $ copyModuleFiles "hi"
  whenProf    $ copyModuleFiles "p_hi"
  whenShared  $ copyModuleFiles "dyn_hi"

  -- copy the built library files over:
  whenVanilla $ installOrdinary builtDir targetDir       vanillaLibName
  whenProf    $ installOrdinary builtDir targetDir       profileLibName
  whenGHCi    $ installOrdinary builtDir targetDir       ghciLibName
  whenShared  $ installShared   builtDir dynlibTargetDir sharedLibName

  where
    install isShared srcDir dstDir name = do
      let src = srcDir </> name
          dst = dstDir </> name
      createDirectoryIfMissingVerbose verbosity True dstDir

      if isShared
        then installExecutableFile verbosity src dst
        else installOrdinaryFile   verbosity src dst

      when (stripLibs lbi) $ Strip.stripLib verbosity
                             (hostPlatform lbi) (withPrograms lbi) dst

    installOrdinary = install False
    installShared   = install True

    copyModuleFiles ext =
      findModuleFiles [builtDir] [ext] (libModules lib)
      >>= installOrdinaryFiles verbosity targetDir

    cid = compilerId (compiler lbi)
    libName = componentUnitId clbi
    vanillaLibName = mkLibName              libName
    profileLibName = mkProfLibName          libName
    ghciLibName    = Internal.mkGHCiLibName libName
    sharedLibName  = (mkSharedLibName cid)  libName

    hasLib    = not $ null (libModules lib)
                   && null (cSources (libBuildInfo lib))
    whenVanilla = when (hasLib && withVanillaLib lbi)
    whenProf    = when (hasLib && withProfLib    lbi)
    whenGHCi    = when (hasLib && withGHCiLib    lbi)
    whenShared  = when (hasLib && withSharedLib  lbi)

-- -----------------------------------------------------------------------------
-- Registering

hcPkgInfo :: ProgramConfiguration -> HcPkg.HcPkgInfo
hcPkgInfo conf = HcPkg.HcPkgInfo { HcPkg.hcPkgProgram    = ghcPkgProg
                                 , HcPkg.noPkgDbStack    = v < [6,9]
                                 , HcPkg.noVerboseFlag   = v < [6,11]
                                 , HcPkg.flagPackageConf = v < [7,5]
                                 , HcPkg.supportsDirDbs  = v >= [6,8]
                                 , HcPkg.requiresDirDbs  = v >= [7,10]
                                 , HcPkg.nativeMultiInstance  = v >= [7,10]
                                 , HcPkg.recacheMultiInstance = v >= [6,12]
                                 }
  where
    v               = versionBranch ver
    Just ghcPkgProg = lookupProgram ghcPkgProgram conf
    Just ver        = programVersion ghcPkgProg

registerPackage
  :: Verbosity
  -> ProgramConfiguration
  -> Bool
  -> PackageDBStack
  -> InstalledPackageInfo
  -> IO ()
registerPackage verbosity progdb multiInstance packageDbs installedPkgInfo
  | multiInstance
  = HcPkg.registerMultiInstance (hcPkgInfo progdb) verbosity
      packageDbs installedPkgInfo

  | otherwise
  = HcPkg.reregister (hcPkgInfo progdb) verbosity
      packageDbs (Right installedPkgInfo)

pkgRoot :: Verbosity -> LocalBuildInfo -> PackageDB -> IO FilePath
pkgRoot verbosity lbi = pkgRoot'
   where
    pkgRoot' GlobalPackageDB =
      let Just ghcProg = lookupProgram ghcProgram (withPrograms lbi)
      in  fmap takeDirectory (getGlobalPackageDB verbosity ghcProg)
    pkgRoot' UserPackageDB = do
      appDir <- getAppUserDataDirectory "ghc"
      let ver      = compilerVersion (compiler lbi)
          subdir   = System.Info.arch ++ '-':System.Info.os
                     ++ '-':showVersion ver
          rootDir  = appDir </> subdir
      -- We must create the root directory for the user package database if it
      -- does not yet exists. Otherwise '${pkgroot}' will resolve to a
      -- directory at the time of 'ghc-pkg register', and registration will
      -- fail.
      createDirectoryIfMissing True rootDir
      return rootDir
    pkgRoot' (SpecificPackageDB fp) = return (takeDirectory fp)

-- -----------------------------------------------------------------------------
-- Utils

isDynamic :: Compiler -> Bool
isDynamic = Internal.ghcLookupProperty "GHC Dynamic"

supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"
