{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

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
module Distribution.Simple.GHC
  ( getGhcInfo
  , configure
  , getInstalledPackages
  , getInstalledPackagesMonitorFiles
  , getPackageDBContents
  , buildLib
  , buildFLib
  , buildExe
  , replLib
  , replFLib
  , replExe
  , startInterpreter
  , installLib
  , installFLib
  , installExe
  , libAbiHash
  , hcPkgInfo
  , registerPackage
  , Internal.componentGhcOptions
  , Internal.componentCcGhcOptions
  , getGhcAppDir
  , getLibDir
  , isDynamic
  , getGlobalPackageDB
  , pkgRoot

    -- * Constructing and deconstructing GHC environment files
  , Internal.GhcEnvironmentFileEntry (..)
  , Internal.simpleGhcEnvironmentFile
  , Internal.renderGhcEnvironmentFile
  , Internal.writeGhcEnvironmentFile
  , Internal.ghcPlatformAndVersionString
  , readGhcEnvironmentFile
  , parseGhcEnvironmentFile
  , ParseErrorExc (..)

    -- * Version-specific implementation quirks
  , getImplInfo
  , GhcImplInfo (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Arrow ((***))
import Control.Monad (forM_)
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Distribution.CabalSpecVersion
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Package
import Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.Build.Inputs (PreBuildComponentInputs (..))
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Errors
import Distribution.Simple.Flag
import qualified Distribution.Simple.GHC.Build as GHC
import Distribution.Simple.GHC.Build.Utils
import Distribution.Simple.GHC.EnvironmentParser
import Distribution.Simple.GHC.ImplInfo
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PreProcess.Types
import Distribution.Simple.Program
import Distribution.Simple.Program.Builtin (runghcProgram)
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.Program.Strip as Strip
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Repl
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ParStrat
import Distribution.Types.TargetInfo
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getAppUserDataDirectory
  , getDirectoryContents
  )
import System.FilePath
  ( isRelative
  , takeDirectory
  )
import qualified System.Info
#ifndef mingw32_HOST_OS
import System.Directory (renameFile)
import System.Posix (createSymbolicLink)
#endif /* mingw32_HOST_OS */

import Distribution.Simple.Setup (BuildingWhat (..))
import Distribution.Simple.Setup.Build

-- -----------------------------------------------------------------------------
-- Configuring

configure
  :: Verbosity
  -> Maybe FilePath
  -> Maybe FilePath
  -> ProgramDb
  -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath hcPkgPath conf0 = do
  (ghcProg, ghcVersion, progdb1) <-
    requireProgramVersion
      verbosity
      ghcProgram
      (orLaterVersion (mkVersion [7, 0, 1]))
      (userMaybeSpecifyPath "ghc" hcPath conf0)
  let implInfo = ghcVersionImplInfo ghcVersion

  -- Cabal currently supports ghc >= 7.0.1 && < 9.12
  -- ... and the following odd development version
  unless (ghcVersion < mkVersion [9, 12]) $
    warn verbosity $
      "Unknown/unsupported 'ghc' version detected "
        ++ "(Cabal "
        ++ prettyShow cabalVersion
        ++ " supports 'ghc' version < 9.12): "
        ++ programPath ghcProg
        ++ " is version "
        ++ prettyShow ghcVersion

  -- This is slightly tricky, we have to configure ghc first, then we use the
  -- location of ghc to help find ghc-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcPkgProg, ghcPkgVersion, progdb2) <-
    requireProgramVersion
      verbosity
      ghcPkgProgram
        { programFindLocation = guessGhcPkgFromGhcPath ghcProg
        }
      anyVersion
      (userMaybeSpecifyPath "ghc-pkg" hcPkgPath progdb1)

  when (ghcVersion /= ghcPkgVersion) $
    dieWithException verbosity $
      VersionMismatchGHC (programPath ghcProg) ghcVersion (programPath ghcPkgProg) ghcPkgVersion
  -- Likewise we try to find the matching hsc2hs and haddock programs.
  let hsc2hsProgram' =
        hsc2hsProgram
          { programFindLocation = guessHsc2hsFromGhcPath ghcProg
          }
      haddockProgram' =
        haddockProgram
          { programFindLocation = guessHaddockFromGhcPath ghcProg
          }
      hpcProgram' =
        hpcProgram
          { programFindLocation = guessHpcFromGhcPath ghcProg
          }
      runghcProgram' =
        runghcProgram
          { programFindLocation = guessRunghcFromGhcPath ghcProg
          }
      progdb3 =
        addKnownProgram haddockProgram' $
          addKnownProgram hsc2hsProgram' $
            addKnownProgram hpcProgram' $
              addKnownProgram runghcProgram' progdb2

  languages <- Internal.getLanguages verbosity implInfo ghcProg
  extensions0 <- Internal.getExtensions verbosity implInfo ghcProg

  ghcInfo <- Internal.getGhcInfo verbosity implInfo ghcProg
  let ghcInfoMap = Map.fromList ghcInfo
      filterJS = if ghcVersion < mkVersion [9, 8] then filterExt JavaScriptFFI else id
      extensions =
        -- workaround https://gitlab.haskell.org/ghc/ghc/-/issues/11214
        filterJS $
          -- see 'filterExtTH' comment below
          filterExtTH $
            extensions0

      -- starting with GHC 8.0, `TemplateHaskell` will be omitted from
      -- `--supported-extensions` when it's not available.
      -- for older GHCs we can use the "Have interpreter" property to
      -- filter out `TemplateHaskell`
      filterExtTH
        | ghcVersion < mkVersion [8]
        , Just "NO" <- Map.lookup "Have interpreter" ghcInfoMap =
            filterExt TemplateHaskell
        | otherwise = id

      filterExt ext = filter ((/= EnableExtension ext) . fst)

      compilerId :: CompilerId
      compilerId = CompilerId GHC ghcVersion

      compilerAbiTag :: AbiTag
      compilerAbiTag = maybe NoAbiTag AbiTag (Map.lookup "Project Unit Id" ghcInfoMap >>= stripPrefix (prettyShow compilerId <> "-"))

  let comp =
        Compiler
          { compilerId
          , compilerAbiTag
          , compilerCompat = []
          , compilerLanguages = languages
          , compilerExtensions = extensions
          , compilerProperties = ghcInfoMap
          }
      compPlatform = Internal.targetPlatform ghcInfo
      -- configure gcc and ld
      progdb4 = Internal.configureToolchain implInfo ghcProg ghcInfoMap progdb3
  return (comp, compPlatform, progdb4)

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find
-- the corresponding tool; e.g. if the tool is ghc-pkg, we try looking
-- for a versioned or unversioned ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-ghc-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
guessToolFromGhcPath
  :: Program
  -> ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessToolFromGhcPath tool ghcProg verbosity searchpath =
  do
    let toolname = programName tool
        given_path = programPath ghcProg
        given_dir = takeDirectory given_path
    real_path <- canonicalizePath given_path
    let real_dir = takeDirectory real_path
        versionSuffix path = takeVersionSuffix (dropExeExtension path)
        given_suf = versionSuffix given_path
        real_suf = versionSuffix real_path
        guessNormal dir = dir </> toolname <.> exeExtension buildPlatform
        guessGhcVersioned dir suf =
          dir
            </> (toolname ++ "-ghc" ++ suf)
              <.> exeExtension buildPlatform
        guessVersioned dir suf =
          dir
            </> (toolname ++ suf)
              <.> exeExtension buildPlatform
        mkGuesses dir suf
          | null suf = [guessNormal dir]
          | otherwise =
              [ guessGhcVersioned dir suf
              , guessVersioned dir suf
              , guessNormal dir
              ]
        -- order matters here, see https://github.com/haskell/cabal/issues/7390
        guesses =
          ( if real_path == given_path
              then []
              else mkGuesses real_dir real_suf
          )
            ++ mkGuesses given_dir given_suf
    info verbosity $
      "looking for tool "
        ++ toolname
        ++ " near compiler in "
        ++ given_dir
    debug verbosity $ "candidate locations: " ++ show guesses
    exists <- traverse doesFileExist guesses
    case [file | (file, True) <- zip guesses exists] of
      -- If we can't find it near ghc, fall back to the usual
      -- method.
      [] -> programFindLocation tool verbosity searchpath
      (fp : _) -> do
        info verbosity $ "found " ++ toolname ++ " in " ++ fp
        let lookedAt =
              map fst
                . takeWhile (\(_file, exist) -> not exist)
                $ zip guesses exists
        return (Just (fp, lookedAt))
  where
    takeVersionSuffix :: FilePath -> String
    takeVersionSuffix = takeWhileEndLE isSuffixChar

    isSuffixChar :: Char -> Bool
    isSuffixChar c = isDigit c || c == '.' || c == '-'

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding ghc-pkg, we try looking for both a versioned and unversioned
-- ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-ghc-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
guessGhcPkgFromGhcPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessGhcPkgFromGhcPath = guessToolFromGhcPath ghcPkgProgram

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding hsc2hs, we try looking for both a versioned and unversioned
-- hsc2hs in the same dir, that is:
--
-- > /usr/local/bin/hsc2hs-ghc-6.6.1(.exe)
-- > /usr/local/bin/hsc2hs-6.6.1(.exe)
-- > /usr/local/bin/hsc2hs(.exe)
guessHsc2hsFromGhcPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessHsc2hsFromGhcPath = guessToolFromGhcPath hsc2hsProgram

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding haddock, we try looking for both a versioned and unversioned
-- haddock in the same dir, that is:
--
-- > /usr/local/bin/haddock-ghc-6.6.1(.exe)
-- > /usr/local/bin/haddock-6.6.1(.exe)
-- > /usr/local/bin/haddock(.exe)
guessHaddockFromGhcPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessHaddockFromGhcPath = guessToolFromGhcPath haddockProgram

guessHpcFromGhcPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessHpcFromGhcPath = guessToolFromGhcPath hpcProgram

guessRunghcFromGhcPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessRunghcFromGhcPath = guessToolFromGhcPath runghcProgram

getGhcInfo :: Verbosity -> ConfiguredProgram -> IO [(String, String)]
getGhcInfo verbosity ghcProg = Internal.getGhcInfo verbosity implInfo ghcProg
  where
    version = fromMaybe (error "GHC.getGhcInfo: no ghc version") $ programVersion ghcProg
    implInfo = ghcVersionImplInfo version

-- | Given a single package DB, return all installed packages.
getPackageDBContents
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> ProgramDb
  -> IO InstalledPackageIndex
getPackageDBContents verbosity mbWorkDir packagedb progdb = do
  pkgss <- getInstalledPackages' verbosity mbWorkDir [packagedb] progdb
  toPackageIndex verbosity pkgss progdb

-- | Given a package DB stack, return all installed packages.
getInstalledPackages
  :: Verbosity
  -> Compiler
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages verbosity comp mbWorkDir packagedbs progdb = do
  checkPackageDbEnvVar verbosity
  checkPackageDbStack verbosity comp packagedbs
  pkgss <- getInstalledPackages' verbosity mbWorkDir packagedbs progdb
  index <- toPackageIndex verbosity pkgss progdb
  return $! hackRtsPackage index
  where
    hackRtsPackage index =
      case PackageIndex.lookupPackageName index (mkPackageName "rts") of
        [(_, [rts])] ->
          PackageIndex.insert (removeMingwIncludeDir rts) index
        _ -> index -- No (or multiple) ghc rts package is registered!!
        -- Feh, whatever, the ghc test suite does some crazy stuff.

-- | Given a list of @(PackageDB, InstalledPackageInfo)@ pairs, produce a
-- @PackageIndex@. Helper function used by 'getPackageDBContents' and
-- 'getInstalledPackages'.
toPackageIndex
  :: Verbosity
  -> [(PackageDB, [InstalledPackageInfo])]
  -> ProgramDb
  -> IO InstalledPackageIndex
toPackageIndex verbosity pkgss progdb = do
  -- On Windows, various fields have $topdir/foo rather than full
  -- paths. We need to substitute the right value in so that when
  -- we, for example, call gcc, we have proper paths to give it.
  topDir <- getLibDir' verbosity ghcProg
  let indices =
        [ PackageIndex.fromList (map (Internal.substTopDir topDir) pkgs)
        | (_, pkgs) <- pkgss
        ]
  return $! mconcat indices
  where
    ghcProg = fromMaybe (error "GHC.toPackageIndex: no ghc program") $ lookupProgram ghcProgram progdb

-- | Return the 'FilePath' to the GHC application data directory.
--
-- @since 3.4.0.0
getGhcAppDir :: IO FilePath
getGhcAppDir = getAppUserDataDirectory "ghc"

getLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getLibDir verbosity lbi =
  dropWhileEndLE isSpace
    `fmap` getDbProgramOutput
      verbosity
      ghcProgram
      (withPrograms lbi)
      ["--print-libdir"]

getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
getLibDir' verbosity ghcProg =
  dropWhileEndLE isSpace
    `fmap` getProgramOutput verbosity ghcProg ["--print-libdir"]

-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity ghcProg =
  dropWhileEndLE isSpace
    `fmap` getProgramOutput verbosity ghcProg ["--print-global-package-db"]

-- | Return the 'FilePath' to the per-user GHC package database.
getUserPackageDB
  :: Verbosity -> ConfiguredProgram -> Platform -> IO FilePath
getUserPackageDB _verbosity ghcProg platform = do
  -- It's rather annoying that we have to reconstruct this, because ghc
  -- hides this information from us otherwise. But for certain use cases
  -- like change monitoring it really can't remain hidden.
  appdir <- getGhcAppDir
  return (appdir </> platformAndVersion </> packageConfFileName)
  where
    platformAndVersion =
      Internal.ghcPlatformAndVersionString
        platform
        ghcVersion
    packageConfFileName = "package.conf.d"
    ghcVersion = fromMaybe (error "GHC.getUserPackageDB: no ghc version") $ programVersion ghcProg

checkPackageDbEnvVar :: Verbosity -> IO ()
checkPackageDbEnvVar verbosity =
  Internal.checkPackageDbEnvVar verbosity "GHC" "GHC_PACKAGE_PATH"

checkPackageDbStack :: Verbosity -> Compiler -> PackageDBStack -> IO ()
checkPackageDbStack verbosity comp =
  if flagPackageConf implInfo
    then checkPackageDbStackPre76 verbosity
    else checkPackageDbStackPost76 verbosity
  where
    implInfo = ghcVersionImplInfo (compilerVersion comp)

checkPackageDbStackPost76 :: Verbosity -> PackageDBStack -> IO ()
checkPackageDbStackPost76 _ (GlobalPackageDB : rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStackPost76 verbosity rest
  | GlobalPackageDB `elem` rest =
      dieWithException verbosity CheckPackageDbStackPost76
checkPackageDbStackPost76 _ _ = return ()

checkPackageDbStackPre76 :: Verbosity -> PackageDBStack -> IO ()
checkPackageDbStackPre76 _ (GlobalPackageDB : rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStackPre76 verbosity rest
  | GlobalPackageDB `notElem` rest =
      dieWithException verbosity CheckPackageDbStackPre76
checkPackageDbStackPre76 verbosity _ =
  dieWithException verbosity GlobalPackageDbSpecifiedFirst

-- GHC < 6.10 put "$topdir/include/mingw" in rts's installDirs. This
-- breaks when you want to use a different gcc, so we need to filter
-- it out.
removeMingwIncludeDir :: InstalledPackageInfo -> InstalledPackageInfo
removeMingwIncludeDir pkg =
  let ids = InstalledPackageInfo.includeDirs pkg
      ids' = filter (not . ("mingw" `isSuffixOf`)) ids
   in pkg{InstalledPackageInfo.includeDirs = ids'}

-- | Get the packages from specific PackageDBs, not cumulative.
getInstalledPackages'
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> [PackageDB]
  -> ProgramDb
  -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity mbWorkDir packagedbs progdb =
  sequenceA
    [ do
      pkgs <- HcPkg.dump (hcPkgInfo progdb) verbosity mbWorkDir packagedb
      return (packagedb, pkgs)
    | packagedb <- packagedbs
    ]

getInstalledPackagesMonitorFiles
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> Platform
  -> ProgramDb
  -> [PackageDB]
  -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity mbWorkDir platform progdb =
  traverse getPackageDBPath
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
    selectMonitorFile path0 = do
      let path =
            if isRelative path0
              then interpretSymbolicPath mbWorkDir (makeRelativePathEx path0)
              else path0
      isFileStyle <- doesFileExist path
      if isFileStyle
        then return path
        else return (path </> "package.cache")

    ghcProg = fromMaybe (error "GHC.toPackageIndex: no ghc program") $ lookupProgram ghcProgram progdb

-- -----------------------------------------------------------------------------
-- Building a library

buildLib
  :: BuildFlags
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildLib flags numJobs pkg lbi lib clbi =
  GHC.build numJobs pkg $
    PreBuildComponentInputs
      { buildingWhat = BuildNormal flags
      , localBuildInfo = lbi
      , targetInfo = TargetInfo clbi (CLib lib)
      }

replLib
  :: ReplFlags
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
replLib flags numJobs pkg lbi lib clbi =
  GHC.build numJobs pkg $
    PreBuildComponentInputs
      { buildingWhat = BuildRepl flags
      , localBuildInfo = lbi
      , targetInfo = TargetInfo clbi (CLib lib)
      }

-- | Start a REPL without loading any source files.
startInterpreter
  :: Verbosity
  -> ProgramDb
  -> Compiler
  -> Platform
  -> PackageDBStack
  -> IO ()
startInterpreter verbosity progdb comp platform packageDBs = do
  let replOpts =
        mempty
          { ghcOptMode = toFlag GhcModeInteractive
          , ghcOptPackageDBs = packageDBs
          }
  checkPackageDbStack verbosity comp packageDBs
  (ghcProg, _) <- requireProgram verbosity ghcProgram progdb
  runGHC verbosity ghcProg comp platform Nothing replOpts

-- -----------------------------------------------------------------------------
-- Building an executable or foreign library

-- | Build a foreign library
buildFLib
  :: Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> ForeignLib
  -> ComponentLocalBuildInfo
  -> IO ()
buildFLib v numJobs pkg lbi flib clbi =
  GHC.build numJobs pkg $
    PreBuildComponentInputs
      { buildingWhat =
          BuildNormal $
            mempty
              { buildCommonFlags =
                  mempty{setupVerbosity = toFlag v}
              }
      , localBuildInfo = lbi
      , targetInfo = TargetInfo clbi (CFLib flib)
      }

replFLib
  :: ReplFlags
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> ForeignLib
  -> ComponentLocalBuildInfo
  -> IO ()
replFLib replFlags njobs pkg lbi flib clbi =
  GHC.build njobs pkg $
    PreBuildComponentInputs
      { buildingWhat = BuildRepl replFlags
      , localBuildInfo = lbi
      , targetInfo = TargetInfo clbi (CFLib flib)
      }

-- | Build an executable with GHC.
buildExe
  :: Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
buildExe v njobs pkg lbi exe clbi =
  GHC.build njobs pkg $
    PreBuildComponentInputs
      { buildingWhat =
          BuildNormal $
            mempty
              { buildCommonFlags =
                  mempty{setupVerbosity = toFlag v}
              }
      , localBuildInfo = lbi
      , targetInfo = TargetInfo clbi (CExe exe)
      }

replExe
  :: ReplFlags
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
replExe replFlags njobs pkg lbi exe clbi =
  GHC.build njobs pkg $
    PreBuildComponentInputs
      { buildingWhat = BuildRepl replFlags
      , localBuildInfo = lbi
      , targetInfo = TargetInfo clbi (CExe exe)
      }

-- | Extracts a String representing a hash of the ABI of a built
-- library.  It can fail if the library has not yet been built.
libAbiHash
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO String
libAbiHash verbosity _pkg_descr lbi lib clbi = do
  let
    libBi = libBuildInfo lib
    comp = compiler lbi
    platform = hostPlatform lbi
    mbWorkDir = mbWorkDirLBI lbi
    vanillaArgs =
      (Internal.componentGhcOptions verbosity lbi libBi clbi (componentBuildDir lbi clbi))
        `mappend` mempty
          { ghcOptMode = toFlag GhcModeAbiHash
          , ghcOptInputModules = toNubListR $ exposedModules lib
          }
    sharedArgs =
      vanillaArgs
        `mappend` mempty
          { ghcOptDynLinkMode = toFlag GhcDynamicOnly
          , ghcOptFPic = toFlag True
          , ghcOptHiSuffix = toFlag "dyn_hi"
          , ghcOptObjSuffix = toFlag "dyn_o"
          , ghcOptExtra = hcSharedOptions GHC libBi
          }
    profArgs =
      vanillaArgs
        `mappend` mempty
          { ghcOptProfilingMode = toFlag True
          , ghcOptProfilingAuto =
              Internal.profDetailLevelFlag
                True
                (withProfLibDetail lbi)
          , ghcOptHiSuffix = toFlag "p_hi"
          , ghcOptObjSuffix = toFlag "p_o"
          , ghcOptExtra = hcProfOptions GHC libBi
          }
    ghcArgs
      | withVanillaLib lbi = vanillaArgs
      | withSharedLib lbi = sharedArgs
      | withProfLib lbi = profArgs
      | otherwise = error "libAbiHash: Can't find an enabled library way"

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)

  hash <-
    getProgramInvocationOutput
      verbosity
      =<< ghcInvocation verbosity ghcProg comp platform mbWorkDir ghcArgs

  return (takeWhile (not . isSpace) hash)

-- -----------------------------------------------------------------------------
-- Installing

-- | Install executables for GHC.
installExe
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ Where to copy the files to
  -> FilePath
  -- ^ Build location
  -> (FilePath, FilePath)
  -- ^ Executable (prefix,suffix)
  -> PackageDescription
  -> Executable
  -> IO ()
installExe
  verbosity
  lbi
  binDir
  buildPref
  (progprefix, progsuffix)
  _pkg
  exe = do
    createDirectoryIfMissingVerbose verbosity True binDir
    let exeName' = unUnqualComponentName $ exeName exe
        exeFileName = exeTargetName (hostPlatform lbi) (exeName exe)
        fixedExeBaseName = progprefix ++ exeName' ++ progsuffix
        installBinary dest = do
          installExecutableFile
            verbosity
            (buildPref </> exeName' </> exeFileName)
            (dest <.> exeExtension (hostPlatform lbi))
          when (stripExes lbi) $
            Strip.stripExe
              verbosity
              (hostPlatform lbi)
              (withPrograms lbi)
              (dest <.> exeExtension (hostPlatform lbi))
    installBinary (binDir </> fixedExeBaseName)

-- | Install foreign library for GHC.
installFLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ install location
  -> FilePath
  -- ^ Build location
  -> PackageDescription
  -> ForeignLib
  -> IO ()
installFLib verbosity lbi targetDir builtDir _pkg flib =
  install
    (foreignLibIsShared flib)
    builtDir
    targetDir
    (flibTargetName lbi flib)
  where
    install isShared srcDir dstDir name = do
      let src = srcDir </> name
          dst = dstDir </> name
      createDirectoryIfMissingVerbose verbosity True targetDir
      -- TODO: Should we strip? (stripLibs lbi)
      if isShared
        then installExecutableFile verbosity src dst
        else installOrdinaryFile verbosity src dst
      -- Now install appropriate symlinks if library is versioned
      let (Platform _ os) = hostPlatform lbi
      when (not (null (foreignLibVersion flib os))) $ do
        when (os /= Linux) $
          dieWithException verbosity $
            CantInstallForeignLib
#ifndef mingw32_HOST_OS
        -- 'createSymbolicLink file1 file2' creates a symbolic link
        -- named 'file2' which points to the file 'file1'.
        -- Note that we do want a symlink to 'name' rather than
        -- 'dst', because the symlink will be relative to the
        -- directory it's created in.
        -- Finally, we first create the symlinks in a temporary
        -- directory and then rename to simulate 'ln --force'.
        withTempDirectory verbosity dstDir nm $ \tmpDir -> do
            let link1 = flibBuildName lbi flib
                link2 = "lib" ++ nm <.> "so"
            createSymbolicLink name (tmpDir </> link1)
            renameFile (tmpDir </> link1) (dstDir </> link1)
            createSymbolicLink name (tmpDir </> link2)
            renameFile (tmpDir </> link2) (dstDir </> link2)
      where
        nm :: String
        nm = unUnqualComponentName $ foreignLibName flib
#endif /* mingw32_HOST_OS */

-- | Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ install location
  -> FilePath
  -- ^ install location for dynamic libraries
  -> FilePath
  -- ^ Build location
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir _builtDir pkg lib clbi = do
  -- copy .hi files over:
  whenVanilla $ copyModuleFiles $ Suffix "hi"
  whenProf $ copyModuleFiles $ Suffix "p_hi"
  whenShared $ copyModuleFiles $ Suffix "dyn_hi"

  -- copy extra compilation artifacts that ghc plugins may produce
  copyDirectoryIfExists extraCompilationArtifacts

  -- copy the built library files over:
  whenHasCode $ do
    whenVanilla $ do
      sequence_
        [ installOrdinary
          builtDir
          targetDir
          (mkGenericStaticLibName (l ++ f))
        | l <-
            getHSLibraryName
              (componentUnitId clbi)
              : (extraBundledLibs (libBuildInfo lib))
        , f <- "" : extraLibFlavours (libBuildInfo lib)
        ]
      whenGHCi $ installOrdinary builtDir targetDir ghciLibName
    whenProf $ do
      installOrdinary builtDir targetDir profileLibName
      whenGHCi $ installOrdinary builtDir targetDir ghciProfLibName
    whenShared $
      if
          -- The behavior for "extra-bundled-libraries" changed in version 2.5.0.
          -- See ghc issue #15837 and Cabal PR #5855.
          | specVersion pkg < CabalSpecV3_0 -> do
              sequence_
                [ installShared
                  builtDir
                  dynlibTargetDir
                  (mkGenericSharedLibName platform compiler_id (l ++ f))
                | l <- getHSLibraryName uid : extraBundledLibs (libBuildInfo lib)
                , f <- "" : extraDynLibFlavours (libBuildInfo lib)
                ]
          | otherwise -> do
              sequence_
                [ installShared
                  builtDir
                  dynlibTargetDir
                  ( mkGenericSharedLibName
                      platform
                      compiler_id
                      (getHSLibraryName uid ++ f)
                  )
                | f <- "" : extraDynLibFlavours (libBuildInfo lib)
                ]
              sequence_
                [ do
                  files <- getDirectoryContents (i builtDir)
                  let l' =
                        mkGenericSharedBundledLibName
                          platform
                          compiler_id
                          l
                  forM_ files $ \file ->
                    when (l' `isPrefixOf` file) $ do
                      isFile <- doesFileExist (i $ builtDir </> makeRelativePathEx file)
                      when isFile $ do
                        installShared
                          builtDir
                          dynlibTargetDir
                          file
                | l <- extraBundledLibs (libBuildInfo lib)
                ]
  where
    -- See Note [Symbolic paths] in Distribution.Utils.Path
    i = interpretSymbolicPathLBI lbi

    builtDir = componentBuildDir lbi clbi
    mbWorkDir = mbWorkDirLBI lbi

    install isShared srcDir dstDir name = do
      let src = i $ srcDir </> makeRelativePathEx name
          dst = dstDir </> name

      createDirectoryIfMissingVerbose verbosity True dstDir

      if isShared
        then installExecutableFile verbosity src dst
        else installOrdinaryFile verbosity src dst

      when (stripLibs lbi) $
        Strip.stripLib
          verbosity
          platform
          (withPrograms lbi)
          dst

    installOrdinary = install False
    installShared = install True

    copyModuleFiles ext = do
      files <- findModuleFilesCwd verbosity mbWorkDir [builtDir] [ext] (allLibModules lib clbi)
      let files' = map (i *** getSymbolicPath) files
      installOrdinaryFiles verbosity targetDir files'

    copyDirectoryIfExists :: RelativePath Build (Dir Artifacts) -> IO ()
    copyDirectoryIfExists dirName = do
      let src = i $ builtDir </> dirName
          dst = targetDir </> getSymbolicPath dirName
      dirExists <- doesDirectoryExist src
      when dirExists $ copyDirectoryRecursive verbosity src dst

    compiler_id = compilerId (compiler lbi)
    platform = hostPlatform lbi
    uid = componentUnitId clbi
    profileLibName = mkProfLibName uid
    ghciLibName = Internal.mkGHCiLibName uid
    ghciProfLibName = Internal.mkGHCiProfLibName uid

    hasLib =
      not $
        null (allLibModules lib clbi)
          && null (cSources (libBuildInfo lib))
          && null (cxxSources (libBuildInfo lib))
          && null (cmmSources (libBuildInfo lib))
          && null (asmSources (libBuildInfo lib))
          && (null (jsSources (libBuildInfo lib)) || not hasJsSupport)
    hasJsSupport = case hostPlatform lbi of
      Platform JavaScript _ -> True
      _ -> False
    has_code = not (componentIsIndefinite clbi)
    whenHasCode = when has_code
    whenVanilla = when (hasLib && withVanillaLib lbi)
    whenProf = when (hasLib && withProfLib lbi && has_code)
    whenGHCi = when (hasLib && withGHCiLib lbi && has_code)
    whenShared = when (hasLib && withSharedLib lbi && has_code)

-- -----------------------------------------------------------------------------
-- Registering

hcPkgInfo :: ProgramDb -> HcPkg.HcPkgInfo
hcPkgInfo progdb =
  HcPkg.HcPkgInfo
    { HcPkg.hcPkgProgram = ghcPkgProg
    , HcPkg.noPkgDbStack = v < [6, 9]
    , HcPkg.noVerboseFlag = v < [6, 11]
    , HcPkg.flagPackageConf = v < [7, 5]
    , HcPkg.supportsDirDbs = v >= [6, 8]
    , HcPkg.requiresDirDbs = v >= [7, 10]
    , HcPkg.nativeMultiInstance = v >= [7, 10]
    , HcPkg.recacheMultiInstance = v >= [6, 12]
    , HcPkg.suppressFilesCheck = v >= [6, 6]
    }
  where
    v = versionNumbers ver
    ghcPkgProg = fromMaybe (error "GHC.hcPkgInfo: no ghc program") $ lookupProgram ghcPkgProgram progdb
    ver = fromMaybe (error "GHC.hcPkgInfo: no ghc version") $ programVersion ghcPkgProg

registerPackage
  :: Verbosity
  -> ProgramDb
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -> InstalledPackageInfo
  -> HcPkg.RegisterOptions
  -> IO ()
registerPackage verbosity progdb mbWorkDir packageDbs installedPkgInfo registerOptions =
  HcPkg.register
    (hcPkgInfo progdb)
    verbosity
    mbWorkDir
    packageDbs
    installedPkgInfo
    registerOptions

pkgRoot :: Verbosity -> LocalBuildInfo -> PackageDB -> IO (SymbolicPath CWD (Dir Pkg))
pkgRoot verbosity lbi = fmap makeSymbolicPath . pkgRoot'
  where
    pkgRoot' GlobalPackageDB =
      let ghcProg = fromMaybe (error "GHC.pkgRoot: no ghc program") $ lookupProgram ghcProgram (withPrograms lbi)
       in fmap takeDirectory (getGlobalPackageDB verbosity ghcProg)
    pkgRoot' UserPackageDB = do
      appDir <- getGhcAppDir
      let ver = compilerVersion (compiler lbi)
          subdir =
            System.Info.arch
              ++ '-'
              : System.Info.os
              ++ '-'
              : prettyShow ver
          rootDir = appDir </> subdir
      -- We must create the root directory for the user package database if it
      -- does not yet exist. Otherwise '${pkgroot}' will resolve to a
      -- directory at the time of 'ghc-pkg register', and registration will
      -- fail.
      createDirectoryIfMissing True rootDir
      return rootDir
    pkgRoot' (SpecificPackageDB fp) =
      return $
        takeDirectory $
          interpretSymbolicPathLBI lbi (unsafeMakeSymbolicPath fp)
