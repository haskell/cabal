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
  , compilerBuildWay
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
import qualified Distribution.Simple.GHC.Build as GHC
import Distribution.Simple.GHC.Build.Modules (BuildWay (..))
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

-- | Building an executable, starting the REPL, and building foreign
-- libraries are all very similar and implemented in 'gbuild'. The
-- 'GBuildMode' distinguishes between the various kinds of operation.
data GBuildMode
  = GBuildExe Executable
  | GReplExe ReplOptions Executable
  | GBuildFLib ForeignLib
  | GReplFLib ReplOptions ForeignLib

gbuildInfo :: GBuildMode -> BuildInfo
gbuildInfo (GBuildExe exe) = buildInfo exe
gbuildInfo (GReplExe _ exe) = buildInfo exe
gbuildInfo (GBuildFLib flib) = foreignLibBuildInfo flib
gbuildInfo (GReplFLib _ flib) = foreignLibBuildInfo flib

gbuildName :: GBuildMode -> String
gbuildName (GBuildExe exe) = unUnqualComponentName $ exeName exe
gbuildName (GReplExe _ exe) = unUnqualComponentName $ exeName exe
gbuildName (GBuildFLib flib) = unUnqualComponentName $ foreignLibName flib
gbuildName (GReplFLib _ flib) = unUnqualComponentName $ foreignLibName flib

gbuildTargetName :: LocalBuildInfo -> GBuildMode -> String
gbuildTargetName lbi (GBuildExe exe) = exeTargetName (hostPlatform lbi) exe
gbuildTargetName lbi (GReplExe _ exe) = exeTargetName (hostPlatform lbi) exe
gbuildTargetName lbi (GBuildFLib flib) = flibTargetName lbi flib
gbuildTargetName lbi (GReplFLib _ flib) = flibTargetName lbi flib

exeTargetName :: Platform -> Executable -> String
exeTargetName platform exe = unUnqualComponentName (exeName exe) `withExt` exeExtension platform

-- | Target name for a foreign library (the actual file name)
--
-- We do not use mkLibName and co here because the naming for foreign libraries
-- is slightly different (we don't use "_p" or compiler version suffices, and we
-- don't want the "lib" prefix on Windows).
--
-- TODO: We do use `dllExtension` and co here, but really that's wrong: they
-- use the OS used to build cabal to determine which extension to use, rather
-- than the target OS (but this is wrong elsewhere in Cabal as well).
flibTargetName :: LocalBuildInfo -> ForeignLib -> String
flibTargetName lbi flib =
  case (os, foreignLibType flib) of
    (Windows, ForeignLibNativeShared) -> nm <.> "dll"
    (Windows, ForeignLibNativeStatic) -> nm <.> "lib"
    (Linux, ForeignLibNativeShared) -> "lib" ++ nm <.> versionedExt
    (_other, ForeignLibNativeShared) ->
      "lib" ++ nm <.> dllExtension (hostPlatform lbi)
    (_other, ForeignLibNativeStatic) ->
      "lib" ++ nm <.> staticLibExtension (hostPlatform lbi)
    (_any, ForeignLibTypeUnknown) -> cabalBug "unknown foreign lib type"
  where
    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

    os :: OS
    os =
      let (Platform _ os') = hostPlatform lbi
       in os'

    -- If a foreign lib foo has lib-version-info 5:1:2 or
    -- lib-version-linux 3.2.1, it should be built as libfoo.so.3.2.1
    -- Libtool's version-info data is translated into library versions in a
    -- nontrivial way: so refer to libtool documentation.
    versionedExt :: String
    versionedExt =
      let nums = foreignLibVersion flib os
       in foldl (<.>) "so" (map show nums)

-- | Name for the library when building.
--
-- If the `lib-version-info` field or the `lib-version-linux` field of
-- a foreign library target is set, we need to incorporate that
-- version into the SONAME field.
--
-- If a foreign library foo has lib-version-info 5:1:2, it should be
-- built as libfoo.so.3.2.1.  We want it to get soname libfoo.so.3.
-- However, GHC does not allow overriding soname by setting linker
-- options, as it sets a soname of its own (namely the output
-- filename), after the user-supplied linker options.  Hence, we have
-- to compile the library with the soname as its filename.  We rename
-- the compiled binary afterwards.
--
-- This method allows to adjust the name of the library at build time
-- such that the correct soname can be set.
flibBuildName :: LocalBuildInfo -> ForeignLib -> String
flibBuildName lbi flib
  -- On linux, if a foreign-library has version data, the first digit is used
  -- to produce the SONAME.
  | (os, foreignLibType flib)
      == (Linux, ForeignLibNativeShared) =
      let nums = foreignLibVersion flib os
       in "lib" ++ nm <.> foldl (<.>) "so" (map show (take 1 nums))
  | otherwise = flibTargetName lbi flib
  where
    os :: OS
    os =
      let (Platform _ os') = hostPlatform lbi
       in os'

    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

gbuildIsRepl :: GBuildMode -> Bool
gbuildIsRepl (GBuildExe _) = False
gbuildIsRepl (GReplExe _ _) = True
gbuildIsRepl (GBuildFLib _) = False
gbuildIsRepl (GReplFLib _ _) = True

gbuildNeedDynamic :: LocalBuildInfo -> GBuildMode -> Bool
gbuildNeedDynamic lbi bm =
  case bm of
    GBuildExe _ -> withDynExe lbi
    GReplExe _ _ -> withDynExe lbi
    GBuildFLib flib -> withDynFLib flib
    GReplFLib _ flib -> withDynFLib flib
  where
    withDynFLib flib =
      case foreignLibType flib of
        ForeignLibNativeShared ->
          ForeignLibStandalone `notElem` foreignLibOptions flib
        ForeignLibNativeStatic ->
          False
        ForeignLibTypeUnknown ->
          cabalBug "unknown foreign lib type"

gbuildModDefFiles :: GBuildMode -> [FilePath]
gbuildModDefFiles (GBuildExe _) = []
gbuildModDefFiles (GReplExe _ _) = []
gbuildModDefFiles (GBuildFLib flib) = foreignLibModDefFile flib
gbuildModDefFiles (GReplFLib _ flib) = foreignLibModDefFile flib

-- | "Main" module name when overridden by @ghc-options: -main-is ...@
-- or 'Nothing' if no @-main-is@ flag could be found.
--
-- In case of 'Nothing', 'Distribution.ModuleName.main' can be assumed.
exeMainModuleName :: Executable -> Maybe ModuleName
exeMainModuleName Executable{buildInfo = bnfo} =
  -- GHC honors the last occurrence of a module name updated via -main-is
  --
  -- Moreover, -main-is when parsed left-to-right can update either
  -- the "Main" module name, or the "main" function name, or both,
  -- see also 'decodeMainIsArg'.
  msum $ reverse $ map decodeMainIsArg $ findIsMainArgs ghcopts
  where
    ghcopts = hcOptions GHC bnfo

    findIsMainArgs [] = []
    findIsMainArgs ("-main-is" : arg : rest) = arg : findIsMainArgs rest
    findIsMainArgs (_ : rest) = findIsMainArgs rest

-- | Decode argument to '-main-is'
--
-- Returns 'Nothing' if argument set only the function name.
--
-- This code has been stolen/refactored from GHC's DynFlags.setMainIs
-- function. The logic here is deliberately imperfect as it is
-- intended to be bug-compatible with GHC's parser. See discussion in
-- https://github.com/haskell/cabal/pull/4539#discussion_r118981753.
decodeMainIsArg :: String -> Maybe ModuleName
decodeMainIsArg arg
  | headOf main_fn isLower =
      -- The arg looked like "Foo.Bar.baz"
      Just (ModuleName.fromString main_mod)
  | headOf arg isUpper -- The arg looked like "Foo" or "Foo.Bar"
    =
      Just (ModuleName.fromString arg)
  | otherwise -- The arg looked like "baz"
    =
      Nothing
  where
    headOf :: String -> (Char -> Bool) -> Bool
    headOf str pred' = any pred' (safeHead str)

    (main_mod, main_fn) = splitLongestPrefix arg (== '.')

    splitLongestPrefix :: String -> (Char -> Bool) -> (String, String)
    splitLongestPrefix str pred'
      | null r_pre = (str, [])
      | otherwise = (reverse (safeTail r_pre), reverse r_suf)
      where
        -- 'safeTail' drops the char satisfying 'pred'
        (r_suf, r_pre) = break pred' (reverse str)

-- | A collection of:
--    * C input files
--    * C++ input files
--    * GHC input files
--    * GHC input modules
--
-- Used to correctly build and link sources.
data BuildSources = BuildSources
  { cSourcesFiles :: [FilePath]
  , cxxSourceFiles :: [FilePath]
  , inputSourceFiles :: [FilePath]
  , inputSourceModules :: [ModuleName]
  }

-- | Locate and return the 'BuildSources' required to build and link.
gbuildSources
  :: Verbosity
  -> PackageId
  -> CabalSpecVersion
  -> FilePath
  -> GBuildMode
  -> IO BuildSources
gbuildSources verbosity pkgId specVer tmpDir bm =
  case bm of
    GBuildExe exe -> exeSources exe
    GReplExe _ exe -> exeSources exe
    GBuildFLib flib -> return $ flibSources flib
    GReplFLib _ flib -> return $ flibSources flib
  where
    exeSources :: Executable -> IO BuildSources
    exeSources exe@Executable{buildInfo = bnfo, modulePath = modPath} = do
      main <- findFileEx verbosity (tmpDir : map getSymbolicPath (hsSourceDirs bnfo)) modPath
      let mainModName = fromMaybe ModuleName.main $ exeMainModuleName exe
          otherModNames = exeModules exe

      -- Scripts have fakePackageId and are always Haskell but can have any extension.
      if isHaskell main || pkgId == fakePackageId
        then
          if specVer < CabalSpecV2_0 && (mainModName `elem` otherModNames)
            then do
              -- The cabal manual clearly states that `other-modules` is
              -- intended for non-main modules.  However, there's at least one
              -- important package on Hackage (happy-1.19.5) which
              -- violates this. We workaround this here so that we don't
              -- invoke GHC with e.g.  'ghc --make Main src/Main.hs' which
              -- would result in GHC complaining about duplicate Main
              -- modules.
              --
              -- Finally, we only enable this workaround for
              -- specVersion < 2, as 'cabal-version:>=2.0' cabal files
              -- have no excuse anymore to keep doing it wrong... ;-)
              warn verbosity $
                "Enabling workaround for Main module '"
                  ++ prettyShow mainModName
                  ++ "' listed in 'other-modules' illegally!"

              return
                BuildSources
                  { cSourcesFiles = cSources bnfo
                  , cxxSourceFiles = cxxSources bnfo
                  , inputSourceFiles = [main]
                  , inputSourceModules =
                      filter (/= mainModName) $
                        exeModules exe
                  }
            else
              return
                BuildSources
                  { cSourcesFiles = cSources bnfo
                  , cxxSourceFiles = cxxSources bnfo
                  , inputSourceFiles = [main]
                  , inputSourceModules = exeModules exe
                  }
        else
          let (csf, cxxsf)
                | isCxx main = (cSources bnfo, main : cxxSources bnfo)
                -- if main is not a Haskell source
                -- and main is not a C++ source
                -- then we assume that it is a C source
                | otherwise = (main : cSources bnfo, cxxSources bnfo)
           in return
                BuildSources
                  { cSourcesFiles = csf
                  , cxxSourceFiles = cxxsf
                  , inputSourceFiles = []
                  , inputSourceModules = exeModules exe
                  }

    flibSources :: ForeignLib -> BuildSources
    flibSources flib@ForeignLib{foreignLibBuildInfo = bnfo} =
      BuildSources
        { cSourcesFiles = cSources bnfo
        , cxxSourceFiles = cxxSources bnfo
        , inputSourceFiles = []
        , inputSourceModules = foreignLibModules flib
        }

    isCxx :: FilePath -> Bool
    isCxx fp = elem (takeExtension fp) [".cpp", ".cxx", ".c++"]

-- | FilePath has a Haskell extension: .hs or .lhs
isHaskell :: FilePath -> Bool
isHaskell fp = elem (takeExtension fp) [".hs", ".lhs"]

replNoLoad :: Ord a => ReplOptions -> NubListR a -> NubListR a
replNoLoad replFlags l
  | replOptionsNoLoad replFlags == Flag True = mempty
  | otherwise = l

-- | Generic build function. See comment for 'GBuildMode'.
gbuild
  :: Verbosity
  -> Flag (Maybe Int)
  -> PackageDescription
  -> LocalBuildInfo
  -> GBuildMode
  -> ComponentLocalBuildInfo
  -> IO ()
gbuild verbosity numJobs pkg_descr lbi bm clbi = do
  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let replFlags = case bm of
        GReplExe flags _ -> flags
        GReplFLib flags _ -> flags
        GBuildExe{} -> mempty
        GBuildFLib{} -> mempty
      comp = compiler lbi
      platform = hostPlatform lbi
      implInfo = getImplInfo comp
      runGhcProg = runGHC verbosity ghcProg comp platform

  let bnfo = gbuildInfo bm

  -- the name that GHC really uses (e.g., with .exe on Windows for executables)
  let targetName = gbuildTargetName lbi bm
  let targetDir = buildDir lbi </> (gbuildName bm)
  let tmpDir = targetDir </> (gbuildName bm ++ "-tmp")
  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True tmpDir

  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = exeCoverage lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | gbuildIsRepl bm = mempty -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way (gbuildName bm)
        | otherwise = mempty

  rpaths <- getRPaths lbi clbi
  buildSources <- gbuildSources verbosity (package pkg_descr) (specVersion pkg_descr) tmpDir bm

  -- ensure extra lib dirs exist before passing to ghc
  cleanedExtraLibDirs <- filterM doesDirectoryExist (extraLibDirs bnfo)
  cleanedExtraLibDirsStatic <- filterM doesDirectoryExist (extraLibDirsStatic bnfo)

  let cSrcs = cSourcesFiles buildSources
      cxxSrcs = cxxSourceFiles buildSources
      inputFiles = inputSourceFiles buildSources
      inputModules = inputSourceModules buildSources
      isGhcDynamic = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      cLikeObjs = map (`replaceExtension` objExtension) cSrcs
      cxxObjs = map (`replaceExtension` objExtension) cxxSrcs
      needDynamic = gbuildNeedDynamic lbi bm
      needProfiling = withProfExe lbi

      -- build executables
      baseOpts =
        (componentGhcOptions verbosity lbi bnfo clbi tmpDir)
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeMake
            , ghcOptInputFiles =
                toNubListR $
                  if package pkg_descr == fakePackageId
                    then filter isHaskell inputFiles
                    else inputFiles
            , ghcOptInputScripts =
                toNubListR $
                  if package pkg_descr == fakePackageId
                    then filter (not . isHaskell) inputFiles
                    else []
            , ghcOptInputModules = toNubListR inputModules
            }
      staticOpts =
        baseOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcStaticOnly
            , ghcOptHPCDir = hpcdir Hpc.Vanilla
            }
      profOpts =
        baseOpts
          `mappend` mempty
            { ghcOptProfilingMode = toFlag True
            , ghcOptProfilingAuto =
                Internal.profDetailLevelFlag
                  False
                  (withProfExeDetail lbi)
            , ghcOptHiSuffix = toFlag "p_hi"
            , ghcOptObjSuffix = toFlag "p_o"
            , ghcOptExtra = hcProfOptions GHC bnfo
            , ghcOptHPCDir = hpcdir Hpc.Prof
            }
      dynOpts =
        baseOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcDynamicOnly
            , -- TODO: Does it hurt to set -fPIC for executables?
              ghcOptFPic = toFlag True
            , ghcOptHiSuffix = toFlag "dyn_hi"
            , ghcOptObjSuffix = toFlag "dyn_o"
            , ghcOptExtra = hcSharedOptions GHC bnfo
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }
      dynTooOpts =
        staticOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcStaticAndDynamic
            , ghcOptDynHiSuffix = toFlag "dyn_hi"
            , ghcOptDynObjSuffix = toFlag "dyn_o"
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }
      linkerOpts =
        mempty
          { ghcOptLinkOptions =
              PD.ldOptions bnfo
                ++ [ "-static"
                   | withFullyStaticExe lbi
                   ]
                -- Pass extra `ld-options` given
                -- through to GHC's linker.
                ++ maybe
                  []
                  programOverrideArgs
                  (lookupProgram ldProgram (withPrograms lbi))
          , ghcOptLinkLibs =
              if withFullyStaticExe lbi
                then extraLibsStatic bnfo
                else extraLibs bnfo
          , ghcOptLinkLibPath =
              toNubListR $
                if withFullyStaticExe lbi
                  then cleanedExtraLibDirsStatic
                  else cleanedExtraLibDirs
          , ghcOptLinkFrameworks =
              toNubListR $
                PD.frameworks bnfo
          , ghcOptLinkFrameworkDirs =
              toNubListR $
                PD.extraFrameworkDirs bnfo
          , ghcOptInputFiles =
              toNubListR
                [tmpDir </> x | x <- cLikeObjs ++ cxxObjs]
          }
      dynLinkerOpts =
        mempty
          { ghcOptRPaths = rpaths
          , ghcOptInputFiles =
              toNubListR
                [tmpDir </> x | x <- cLikeObjs ++ cxxObjs]
          }
      replOpts =
        baseOpts
          { ghcOptExtra =
              Internal.filterGhciFlags
                (ghcOptExtra baseOpts)
                <> replOptionsFlags replFlags
          , ghcOptInputModules = replNoLoad replFlags (ghcOptInputModules baseOpts)
          , ghcOptInputFiles = replNoLoad replFlags (ghcOptInputFiles baseOpts)
          }
          -- For a normal compile we do separate invocations of ghc for
          -- compiling as for linking. But for repl we have to do just
          -- the one invocation, so that one has to include all the
          -- linker stuff too, like -l flags and any .o files from C
          -- files etc.
          `mappend` linkerOpts
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeInteractive
            , ghcOptOptimisation = toFlag GhcNoOptimisation
            }
      commonOpts
        | needProfiling = profOpts
        | needDynamic = dynOpts
        | otherwise = staticOpts
      compileOpts
        | useDynToo = dynTooOpts
        | otherwise = commonOpts
      withStaticExe = not needProfiling && not needDynamic

      -- For building exe's that use TH with -prof or -dynamic we actually have
      -- to build twice, once without -prof/-dynamic and then again with
      -- -prof/-dynamic. This is because the code that TH needs to run at
      -- compile time needs to be the vanilla ABI so it can be loaded up and run
      -- by the compiler.
      -- With dynamic-by-default GHC the TH object files loaded at compile-time
      -- need to be .dyn_o instead of .o.
      doingTH = usesTemplateHaskellOrQQ bnfo
      -- Should we use -dynamic-too instead of compiling twice?
      useDynToo =
        dynamicTooSupported
          && isGhcDynamic
          && doingTH
          && withStaticExe
          && null (hcSharedOptions GHC bnfo)
      compileTHOpts
        | isGhcDynamic = dynOpts
        | otherwise = staticOpts
      compileForTH
        | gbuildIsRepl bm = False
        | useDynToo = False
        | isGhcDynamic = doingTH && (needProfiling || withStaticExe)
        | otherwise = doingTH && (needProfiling || needDynamic)

  -- Build static/dynamic object files for TH, if needed.
  when compileForTH $
    runGhcProg
      compileTHOpts
        { ghcOptNoLink = toFlag True
        , ghcOptNumJobs = numJobs
        }

  -- Do not try to build anything if there are no input files.
  -- This can happen if the cabal file ends up with only cSrcs
  -- but no Haskell modules.
  unless
    ( (null inputFiles && null inputModules)
        || gbuildIsRepl bm
    )
    $ runGhcProg
      compileOpts
        { ghcOptNoLink = toFlag True
        , ghcOptNumJobs = numJobs
        }

  -- build any C++ sources
  unless (null cxxSrcs) $ do
    info verbosity "Building C++ Sources..."
    sequence_
      [ do
        let baseCxxOpts =
              Internal.componentCxxGhcOptions
                verbosity
                implInfo
                lbi
                bnfo
                clbi
                tmpDir
                filename
            vanillaCxxOpts =
              if isGhcDynamic
                then -- Dynamic GHC requires C++ sources to be built
                -- with -fPIC for REPL to work. See #2207.
                  baseCxxOpts{ghcOptFPic = toFlag True}
                else baseCxxOpts
            profCxxOpts =
              vanillaCxxOpts
                `mappend` mempty
                  { ghcOptProfilingMode = toFlag True
                  }
            sharedCxxOpts =
              vanillaCxxOpts
                `mappend` mempty
                  { ghcOptFPic = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  }
            opts
              | needProfiling = profCxxOpts
              | needDynamic = sharedCxxOpts
              | otherwise = vanillaCxxOpts
            -- TODO: Placing all Haskell, C, & C++ objects in a single directory
            --       Has the potential for file collisions. In general we would
            --       consider this a user error. However, we should strive to
            --       add a warning if this occurs.
            odir = fromFlag (ghcOptObjDir opts)
        createDirectoryIfMissingVerbose verbosity True odir
        needsRecomp <- checkNeedsRecompilation filename opts
        when needsRecomp $
          runGhcProg opts
      | filename <- cxxSrcs
      ]

  -- build any C sources
  unless (null cSrcs) $ do
    info verbosity "Building C Sources..."
    sequence_
      [ do
        let baseCcOpts =
              Internal.componentCcGhcOptions
                verbosity
                implInfo
                lbi
                bnfo
                clbi
                tmpDir
                filename
            vanillaCcOpts =
              if isGhcDynamic
                then -- Dynamic GHC requires C sources to be built
                -- with -fPIC for REPL to work. See #2207.
                  baseCcOpts{ghcOptFPic = toFlag True}
                else baseCcOpts
            profCcOpts =
              vanillaCcOpts
                `mappend` mempty
                  { ghcOptProfilingMode = toFlag True
                  }
            sharedCcOpts =
              vanillaCcOpts
                `mappend` mempty
                  { ghcOptFPic = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  }
            opts
              | needProfiling = profCcOpts
              | needDynamic = sharedCcOpts
              | otherwise = vanillaCcOpts
            odir = fromFlag (ghcOptObjDir opts)
        createDirectoryIfMissingVerbose verbosity True odir
        needsRecomp <- checkNeedsRecompilation filename opts
        when needsRecomp $
          runGhcProg opts
      | filename <- cSrcs
      ]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  case bm of
    GReplExe _ _ -> runReplOrWriteFlags verbosity ghcProg comp platform replFlags replOpts bnfo clbi (pkgName (PD.package pkg_descr))
    GReplFLib _ _ -> runReplOrWriteFlags verbosity ghcProg comp platform replFlags replOpts bnfo clbi (pkgName (PD.package pkg_descr))
    GBuildExe _ -> do
      let linkOpts =
            commonOpts
              `mappend` linkerOpts
              `mappend` mempty
                { ghcOptLinkNoHsMain = toFlag (null inputFiles)
                }
              `mappend` (if withDynExe lbi then dynLinkerOpts else mempty)

      info verbosity "Linking..."
      -- Work around old GHCs not relinking in this
      -- situation, see #3294
      let target = targetDir </> targetName
      when (compilerVersion comp < mkVersion [7, 7]) $ do
        e <- doesFileExist target
        when e (removeFile target)
      runGhcProg linkOpts{ghcOptOutputFile = toFlag target}
    GBuildFLib flib -> do
      let
        -- Instruct GHC to link against libHSrts.
        rtsLinkOpts :: GhcOptions
        rtsLinkOpts
          | supportsFLinkRts =
              mempty
                { ghcOptLinkRts = toFlag True
                }
          | otherwise =
              mempty
                { ghcOptLinkLibs = rtsOptLinkLibs
                , ghcOptLinkLibPath = toNubListR $ rtsLibPaths rtsInfo
                }
          where
            threaded = hasThreaded (gbuildInfo bm)
            supportsFLinkRts = compilerVersion comp >= mkVersion [9, 0]
            rtsInfo = extractRtsInfo lbi
            rtsOptLinkLibs =
              [ if needDynamic
                  then
                    if threaded
                      then dynRtsThreadedLib (rtsDynamicInfo rtsInfo)
                      else dynRtsVanillaLib (rtsDynamicInfo rtsInfo)
                  else
                    if threaded
                      then statRtsThreadedLib (rtsStaticInfo rtsInfo)
                      else statRtsVanillaLib (rtsStaticInfo rtsInfo)
              ]

        linkOpts :: GhcOptions
        linkOpts = case foreignLibType flib of
          ForeignLibNativeShared ->
            commonOpts
              `mappend` linkerOpts
              `mappend` dynLinkerOpts
              `mappend` rtsLinkOpts
              `mappend` mempty
                { ghcOptLinkNoHsMain = toFlag True
                , ghcOptShared = toFlag True
                , ghcOptFPic = toFlag True
                , ghcOptLinkModDefFiles = toNubListR $ gbuildModDefFiles bm
                }
              -- See Note [RPATH]
              `mappend` ifNeedsRPathWorkaround
                lbi
                mempty
                  { ghcOptLinkOptions = ["-Wl,--no-as-needed"]
                  , ghcOptLinkLibs = ["ffi"]
                  }
          ForeignLibNativeStatic ->
            -- this should be caught by buildFLib
            -- (and if we do implement this, we probably don't even want to call
            -- ghc here, but rather Ar.createArLibArchive or something)
            cabalBug "static libraries not yet implemented"
          ForeignLibTypeUnknown ->
            cabalBug "unknown foreign lib type"
      -- We build under a (potentially) different filename to set a
      -- soname on supported platforms.  See also the note for
      -- @flibBuildName@.
      info verbosity "Linking..."
      let buildName = flibBuildName lbi flib
      runGhcProg linkOpts{ghcOptOutputFile = toFlag (targetDir </> buildName)}
      renameFile (targetDir </> buildName) (targetDir </> targetName)

{-
Note [RPATH]
~~~~~~~~~~~~

Suppose that the dynamic library depends on `base`, but not (directly) on
`integer-gmp` (which, however, is a dependency of `base`). We will link the
library as

    gcc ... -lHSbase-4.7.0.2-ghc7.8.4 -lHSinteger-gmp-0.5.1.0-ghc7.8.4 ...

However, on systems (like Ubuntu) where the linker gets called with `-as-needed`
by default, the linker will notice that `integer-gmp` isn't actually a direct
dependency and hence omit the link.

Then when we attempt to link a C program against this dynamic library, the
_static_ linker will attempt to verify that all symbols can be resolved.  The
dynamic library itself does not require any symbols from `integer-gmp`, but
`base` does. In order to verify that the symbols used by `base` can be
resolved, the static linker needs to be able to _find_ integer-gmp.

Finding the `base` dependency is simple, because the dynamic elf header
(`readelf -d`) for the library that we have created looks something like

    (NEEDED) Shared library: [libHSbase-4.7.0.2-ghc7.8.4.so]
    (RPATH)  Library rpath: [/path/to/base-4.7.0.2:...]

However, when it comes to resolving the dependency on `integer-gmp`, it needs
to look at the dynamic header for `base`. On modern ghc (7.8 and higher) this
looks something like

    (NEEDED) Shared library: [libHSinteger-gmp-0.5.1.0-ghc7.8.4.so]
    (RPATH)  Library rpath: [$ORIGIN/../integer-gmp-0.5.1.0:...]

This specifies the location of `integer-gmp` _in terms of_ the location of base
(using the `$ORIGIN`) variable. But here's the crux: when the static linker
attempts to verify that all symbols can be resolved, [**IT DOES NOT RESOLVE
`$ORIGIN`**](http://stackoverflow.com/questions/6323603/ld-using-rpath-origin-inside-a-shared-library-recursive).
As a consequence, it will not be able to resolve the symbols and report the
missing symbols as errors, _even though the dynamic linker **would** be able to
resolve these symbols_. We can tell the static linker not to report these
errors by using `--unresolved-symbols=ignore-all` and all will be fine when we
run the program ([(indeed, this is what the gold linker
does)](https://sourceware.org/ml/binutils/2013-05/msg00038.html), but it makes
the resulting library more difficult to use.

Instead what we can do is make sure that the generated dynamic library has
explicit top-level dependencies on these libraries. This means that the static
linker knows where to find them, and when we have transitive dependencies on
the same libraries the linker will only load them once, so we avoid needing to
look at the `RPATH` of our dependencies. We can do this by passing
`--no-as-needed` to the linker, so that it doesn't omit any libraries.

Note that on older ghc (7.6 and before) the Haskell libraries don't have an
RPATH set at all, which makes it even more important that we make these
top-level dependencies.

Finally, we have to explicitly link against `libffi` for the same reason. For
newer ghc this _happens_ to be unnecessary on many systems because `libffi` is
a library which is not specific to GHC, and when the static linker verifies
that all symbols can be resolved it will find the `libffi` that is globally
installed (completely independent from ghc). Of course, this may well be the
_wrong_ version of `libffi`, but it's quite possible that symbol resolution
happens to work. This is of course the wrong approach, which is why we link
explicitly against `libffi` so that we will find the _right_ version of
`libffi`.
-}

-- | Do we need the RPATH workaround?
--
-- See Note [RPATH].
ifNeedsRPathWorkaround :: Monoid a => LocalBuildInfo -> a -> a
ifNeedsRPathWorkaround lbi a =
  case hostPlatform lbi of
    Platform _ Linux -> a
    _otherwise -> mempty

data DynamicRtsInfo = DynamicRtsInfo
  { dynRtsVanillaLib :: FilePath
  , dynRtsThreadedLib :: FilePath
  , dynRtsDebugLib :: FilePath
  , dynRtsEventlogLib :: FilePath
  , dynRtsThreadedDebugLib :: FilePath
  , dynRtsThreadedEventlogLib :: FilePath
  }

data StaticRtsInfo = StaticRtsInfo
  { statRtsVanillaLib :: FilePath
  , statRtsThreadedLib :: FilePath
  , statRtsDebugLib :: FilePath
  , statRtsEventlogLib :: FilePath
  , statRtsThreadedDebugLib :: FilePath
  , statRtsThreadedEventlogLib :: FilePath
  , statRtsProfilingLib :: FilePath
  , statRtsThreadedProfilingLib :: FilePath
  }

data RtsInfo = RtsInfo
  { rtsDynamicInfo :: DynamicRtsInfo
  , rtsStaticInfo :: StaticRtsInfo
  , rtsLibPaths :: [FilePath]
  }

-- | Extract (and compute) information about the RTS library
--
-- TODO: This hardcodes the name as @HSrts-ghc<version>@. I don't know if we can
-- find this information somewhere. We can lookup the 'hsLibraries' field of
-- 'InstalledPackageInfo' but it will tell us @["HSrts", "Cffi"]@, which
-- doesn't really help.
extractRtsInfo :: LocalBuildInfo -> RtsInfo
extractRtsInfo lbi =
  case PackageIndex.lookupPackageName
    (installedPkgs lbi)
    (mkPackageName "rts") of
    [(_, [rts])] -> aux rts
    _otherwise -> error "No (or multiple) ghc rts package is registered"
  where
    aux :: InstalledPackageInfo -> RtsInfo
    aux rts =
      RtsInfo
        { rtsDynamicInfo =
            DynamicRtsInfo
              { dynRtsVanillaLib = withGhcVersion "HSrts"
              , dynRtsThreadedLib = withGhcVersion "HSrts_thr"
              , dynRtsDebugLib = withGhcVersion "HSrts_debug"
              , dynRtsEventlogLib = withGhcVersion "HSrts_l"
              , dynRtsThreadedDebugLib = withGhcVersion "HSrts_thr_debug"
              , dynRtsThreadedEventlogLib = withGhcVersion "HSrts_thr_l"
              }
        , rtsStaticInfo =
            StaticRtsInfo
              { statRtsVanillaLib = "HSrts"
              , statRtsThreadedLib = "HSrts_thr"
              , statRtsDebugLib = "HSrts_debug"
              , statRtsEventlogLib = "HSrts_l"
              , statRtsThreadedDebugLib = "HSrts_thr_debug"
              , statRtsThreadedEventlogLib = "HSrts_thr_l"
              , statRtsProfilingLib = "HSrts_p"
              , statRtsThreadedProfilingLib = "HSrts_thr_p"
              }
        , rtsLibPaths = InstalledPackageInfo.libraryDirs rts
        }
    withGhcVersion = (++ ("-ghc" ++ prettyShow (compilerVersion (compiler lbi))))

-- | Returns True if the modification date of the given source file is newer than
-- the object file we last compiled for it, or if no object file exists yet.
checkNeedsRecompilation :: FilePath -> GhcOptions -> IO Bool
checkNeedsRecompilation filename opts = filename `moreRecentFile` oname
  where
    oname = getObjectFileName filename opts

-- | Finds the object file name of the given source file
getObjectFileName :: FilePath -> GhcOptions -> FilePath
getObjectFileName filename opts = oname
  where
    odir = fromFlag (ghcOptObjDir opts)
    oext = fromFlagOrDefault "o" (ghcOptObjSuffix opts)
    oname = odir </> replaceExtension filename oext

-- | Calculate the RPATHs for the component we are building.
--
-- Calculates relative RPATHs when 'relocatable' is set.
getRPaths
  :: LocalBuildInfo
  -> ComponentLocalBuildInfo
  -- ^ Component we are building
  -> IO (NubListR FilePath)
getRPaths lbi clbi | supportRPaths hostOS = do
  libraryPaths <- depLibraryPaths False (relocatable lbi) lbi clbi
  let hostPref = case hostOS of
        OSX -> "@loader_path"
        _ -> "$ORIGIN"
      relPath p = if isRelative p then hostPref </> p else p
      rpaths = toNubListR (map relPath libraryPaths)
  return rpaths
  where
    (Platform _ hostOS) = hostPlatform lbi
    compid = compilerId . compiler $ lbi

    -- The list of RPath-supported operating systems below reflects the
    -- platforms on which Cabal's RPATH handling is tested. It does _NOT_
    -- reflect whether the OS supports RPATH.

    -- E.g. when this comment was written, the *BSD operating systems were
    -- untested with regards to Cabal RPATH handling, and were hence set to
    -- 'False', while those operating systems themselves do support RPATH.
    supportRPaths Linux = True
    supportRPaths Windows = False
    supportRPaths OSX = True
    supportRPaths FreeBSD =
      case compid of
        CompilerId GHC ver | ver >= mkVersion [7, 10, 2] -> True
        _ -> False
    supportRPaths OpenBSD = False
    supportRPaths NetBSD = False
    supportRPaths DragonFly = False
    supportRPaths Solaris = False
    supportRPaths AIX = False
    supportRPaths HPUX = False
    supportRPaths IRIX = False
    supportRPaths HaLVM = False
    supportRPaths IOS = False
    supportRPaths Android = False
    supportRPaths Ghcjs = False
    supportRPaths Wasi = False
    supportRPaths Hurd = False
    supportRPaths (OtherOS _) = False
-- Do _not_ add a default case so that we get a warning here when a new OS
-- is added.

getRPaths _ _ = return mempty

-- | Determine whether the given 'BuildInfo' is intended to link against the
-- threaded RTS. This is used to determine which RTS to link against when
-- building a foreign library with a GHC without support for @-flink-rts@.
hasThreaded :: BuildInfo -> Bool
hasThreaded bi = elem "-threaded" ghc
  where
    PerCompilerFlavor ghc _ = options bi
=======
replExe replFlags v njobs pkg lbi =
  gbuild v njobs pkg lbi . GReplExe replFlags

-- | Building an executable, starting the REPL, and building foreign
-- libraries are all very similar and implemented in 'gbuild'. The
-- 'GBuildMode' distinguishes between the various kinds of operation.
data GBuildMode
  = GBuildExe Executable
  | GReplExe ReplOptions Executable
  | GBuildFLib ForeignLib
  | GReplFLib ReplOptions ForeignLib

gbuildInfo :: GBuildMode -> BuildInfo
gbuildInfo (GBuildExe exe) = buildInfo exe
gbuildInfo (GReplExe _ exe) = buildInfo exe
gbuildInfo (GBuildFLib flib) = foreignLibBuildInfo flib
gbuildInfo (GReplFLib _ flib) = foreignLibBuildInfo flib

gbuildName :: GBuildMode -> String
gbuildName (GBuildExe exe) = unUnqualComponentName $ exeName exe
gbuildName (GReplExe _ exe) = unUnqualComponentName $ exeName exe
gbuildName (GBuildFLib flib) = unUnqualComponentName $ foreignLibName flib
gbuildName (GReplFLib _ flib) = unUnqualComponentName $ foreignLibName flib

gbuildTargetName :: LocalBuildInfo -> GBuildMode -> String
gbuildTargetName lbi (GBuildExe exe) = exeTargetName (hostPlatform lbi) exe
gbuildTargetName lbi (GReplExe _ exe) = exeTargetName (hostPlatform lbi) exe
gbuildTargetName lbi (GBuildFLib flib) = flibTargetName lbi flib
gbuildTargetName lbi (GReplFLib _ flib) = flibTargetName lbi flib

exeTargetName :: Platform -> Executable -> String
exeTargetName platform exe = unUnqualComponentName (exeName exe) `withExt` exeExtension platform

-- | Target name for a foreign library (the actual file name)
--
-- We do not use mkLibName and co here because the naming for foreign libraries
-- is slightly different (we don't use "_p" or compiler version suffices, and we
-- don't want the "lib" prefix on Windows).
--
-- TODO: We do use `dllExtension` and co here, but really that's wrong: they
-- use the OS used to build cabal to determine which extension to use, rather
-- than the target OS (but this is wrong elsewhere in Cabal as well).
flibTargetName :: LocalBuildInfo -> ForeignLib -> String
flibTargetName lbi flib =
  case (os, foreignLibType flib) of
    (Windows, ForeignLibNativeShared) -> nm <.> "dll"
    (Windows, ForeignLibNativeStatic) -> nm <.> "lib"
    (Linux, ForeignLibNativeShared) -> "lib" ++ nm <.> versionedExt
    (_other, ForeignLibNativeShared) ->
      "lib" ++ nm <.> dllExtension (hostPlatform lbi)
    (_other, ForeignLibNativeStatic) ->
      "lib" ++ nm <.> staticLibExtension (hostPlatform lbi)
    (_any, ForeignLibTypeUnknown) -> cabalBug "unknown foreign lib type"
  where
    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

    os :: OS
    os =
      let (Platform _ os') = hostPlatform lbi
       in os'

    -- If a foreign lib foo has lib-version-info 5:1:2 or
    -- lib-version-linux 3.2.1, it should be built as libfoo.so.3.2.1
    -- Libtool's version-info data is translated into library versions in a
    -- nontrivial way: so refer to libtool documentation.
    versionedExt :: String
    versionedExt =
      let nums = foreignLibVersion flib os
       in foldl (<.>) "so" (map show nums)

-- | Name for the library when building.
--
-- If the `lib-version-info` field or the `lib-version-linux` field of
-- a foreign library target is set, we need to incorporate that
-- version into the SONAME field.
--
-- If a foreign library foo has lib-version-info 5:1:2, it should be
-- built as libfoo.so.3.2.1.  We want it to get soname libfoo.so.3.
-- However, GHC does not allow overriding soname by setting linker
-- options, as it sets a soname of its own (namely the output
-- filename), after the user-supplied linker options.  Hence, we have
-- to compile the library with the soname as its filename.  We rename
-- the compiled binary afterwards.
--
-- This method allows to adjust the name of the library at build time
-- such that the correct soname can be set.
flibBuildName :: LocalBuildInfo -> ForeignLib -> String
flibBuildName lbi flib
  -- On linux, if a foreign-library has version data, the first digit is used
  -- to produce the SONAME.
  | (os, foreignLibType flib)
      == (Linux, ForeignLibNativeShared) =
      let nums = foreignLibVersion flib os
       in "lib" ++ nm <.> foldl (<.>) "so" (map show (take 1 nums))
  | otherwise = flibTargetName lbi flib
  where
    os :: OS
    os =
      let (Platform _ os') = hostPlatform lbi
       in os'

    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

gbuildIsRepl :: GBuildMode -> Bool
gbuildIsRepl (GBuildExe _) = False
gbuildIsRepl (GReplExe _ _) = True
gbuildIsRepl (GBuildFLib _) = False
gbuildIsRepl (GReplFLib _ _) = True

gbuildNeedDynamic :: LocalBuildInfo -> GBuildMode -> Bool
gbuildNeedDynamic lbi bm =
  case bm of
    GBuildExe _ -> withDynExe lbi
    GReplExe _ _ -> withDynExe lbi
    GBuildFLib flib -> withDynFLib flib
    GReplFLib _ flib -> withDynFLib flib
  where
    withDynFLib flib =
      case foreignLibType flib of
        ForeignLibNativeShared ->
          ForeignLibStandalone `notElem` foreignLibOptions flib
        ForeignLibNativeStatic ->
          False
        ForeignLibTypeUnknown ->
          cabalBug "unknown foreign lib type"

gbuildModDefFiles :: GBuildMode -> [FilePath]
gbuildModDefFiles (GBuildExe _) = []
gbuildModDefFiles (GReplExe _ _) = []
gbuildModDefFiles (GBuildFLib flib) = foreignLibModDefFile flib
gbuildModDefFiles (GReplFLib _ flib) = foreignLibModDefFile flib

-- | "Main" module name when overridden by @ghc-options: -main-is ...@
-- or 'Nothing' if no @-main-is@ flag could be found.
--
-- In case of 'Nothing', 'Distribution.ModuleName.main' can be assumed.
exeMainModuleName :: Executable -> Maybe ModuleName
exeMainModuleName Executable{buildInfo = bnfo} =
  -- GHC honors the last occurrence of a module name updated via -main-is
  --
  -- Moreover, -main-is when parsed left-to-right can update either
  -- the "Main" module name, or the "main" function name, or both,
  -- see also 'decodeMainIsArg'.
  msum $ reverse $ map decodeMainIsArg $ findIsMainArgs ghcopts
  where
    ghcopts = hcOptions GHC bnfo

    findIsMainArgs [] = []
    findIsMainArgs ("-main-is" : arg : rest) = arg : findIsMainArgs rest
    findIsMainArgs (_ : rest) = findIsMainArgs rest

-- | Decode argument to '-main-is'
--
-- Returns 'Nothing' if argument set only the function name.
--
-- This code has been stolen/refactored from GHC's DynFlags.setMainIs
-- function. The logic here is deliberately imperfect as it is
-- intended to be bug-compatible with GHC's parser. See discussion in
-- https://github.com/haskell/cabal/pull/4539#discussion_r118981753.
decodeMainIsArg :: String -> Maybe ModuleName
decodeMainIsArg arg
  | headOf main_fn isLower =
      -- The arg looked like "Foo.Bar.baz"
      Just (ModuleName.fromString main_mod)
  | headOf arg isUpper -- The arg looked like "Foo" or "Foo.Bar"
    =
      Just (ModuleName.fromString arg)
  | otherwise -- The arg looked like "baz"
    =
      Nothing
  where
    headOf :: String -> (Char -> Bool) -> Bool
    headOf str pred' = any pred' (safeHead str)

    (main_mod, main_fn) = splitLongestPrefix arg (== '.')

    splitLongestPrefix :: String -> (Char -> Bool) -> (String, String)
    splitLongestPrefix str pred'
      | null r_pre = (str, [])
      | otherwise = (reverse (safeTail r_pre), reverse r_suf)
      where
        -- 'safeTail' drops the char satisfying 'pred'
        (r_suf, r_pre) = break pred' (reverse str)

-- | A collection of:
--    * C input files
--    * C++ input files
--    * GHC input files
--    * GHC input modules
--
-- Used to correctly build and link sources.
data BuildSources = BuildSources
  { cSourcesFiles :: [FilePath]
  , cxxSourceFiles :: [FilePath]
  , inputSourceFiles :: [FilePath]
  , inputSourceModules :: [ModuleName]
  }

-- | Locate and return the 'BuildSources' required to build and link.
gbuildSources
  :: Verbosity
  -> PackageId
  -> CabalSpecVersion
  -> FilePath
  -> GBuildMode
  -> IO BuildSources
gbuildSources verbosity pkgId specVer tmpDir bm =
  case bm of
    GBuildExe exe -> exeSources exe
    GReplExe _ exe -> exeSources exe
    GBuildFLib flib -> return $ flibSources flib
    GReplFLib _ flib -> return $ flibSources flib
  where
    exeSources :: Executable -> IO BuildSources
    exeSources exe@Executable{buildInfo = bnfo, modulePath = modPath} = do
      main <- findFileEx verbosity (tmpDir : map getSymbolicPath (hsSourceDirs bnfo)) modPath
      let mainModName = fromMaybe ModuleName.main $ exeMainModuleName exe
          otherModNames = exeModules exe

      -- Scripts have fakePackageId and are always Haskell but can have any extension.
      if isHaskell main || pkgId == fakePackageId
        then
          if specVer < CabalSpecV2_0 && (mainModName `elem` otherModNames)
            then do
              -- The cabal manual clearly states that `other-modules` is
              -- intended for non-main modules.  However, there's at least one
              -- important package on Hackage (happy-1.19.5) which
              -- violates this. We workaround this here so that we don't
              -- invoke GHC with e.g.  'ghc --make Main src/Main.hs' which
              -- would result in GHC complaining about duplicate Main
              -- modules.
              --
              -- Finally, we only enable this workaround for
              -- specVersion < 2, as 'cabal-version:>=2.0' cabal files
              -- have no excuse anymore to keep doing it wrong... ;-)
              warn verbosity $
                "Enabling workaround for Main module '"
                  ++ prettyShow mainModName
                  ++ "' listed in 'other-modules' illegally!"

              return
                BuildSources
                  { cSourcesFiles = cSources bnfo
                  , cxxSourceFiles = cxxSources bnfo
                  , inputSourceFiles = [main]
                  , inputSourceModules =
                      filter (/= mainModName) $
                        exeModules exe
                  }
            else
              return
                BuildSources
                  { cSourcesFiles = cSources bnfo
                  , cxxSourceFiles = cxxSources bnfo
                  , inputSourceFiles = [main]
                  , inputSourceModules = exeModules exe
                  }
        else
          let (csf, cxxsf)
                | isCxx main = (cSources bnfo, main : cxxSources bnfo)
                -- if main is not a Haskell source
                -- and main is not a C++ source
                -- then we assume that it is a C source
                | otherwise = (main : cSources bnfo, cxxSources bnfo)
           in return
                BuildSources
                  { cSourcesFiles = csf
                  , cxxSourceFiles = cxxsf
                  , inputSourceFiles = []
                  , inputSourceModules = exeModules exe
                  }

    flibSources :: ForeignLib -> BuildSources
    flibSources flib@ForeignLib{foreignLibBuildInfo = bnfo} =
      BuildSources
        { cSourcesFiles = cSources bnfo
        , cxxSourceFiles = cxxSources bnfo
        , inputSourceFiles = []
        , inputSourceModules = foreignLibModules flib
        }

    isCxx :: FilePath -> Bool
    isCxx fp = elem (takeExtension fp) [".cpp", ".cxx", ".c++"]

-- | FilePath has a Haskell extension: .hs or .lhs
isHaskell :: FilePath -> Bool
isHaskell fp = elem (takeExtension fp) [".hs", ".lhs"]

replNoLoad :: Ord a => ReplOptions -> NubListR a -> NubListR a
replNoLoad replFlags l
  | replOptionsNoLoad replFlags == Flag True = mempty
  | otherwise = l

-- | Generic build function. See comment for 'GBuildMode'.
gbuild
  :: Verbosity
  -> Flag (Maybe Int)
  -> PackageDescription
  -> LocalBuildInfo
  -> GBuildMode
  -> ComponentLocalBuildInfo
  -> IO ()
gbuild verbosity numJobs pkg_descr lbi bm clbi = do
  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let replFlags = case bm of
        GReplExe flags _ -> flags
        GReplFLib flags _ -> flags
        GBuildExe{} -> mempty
        GBuildFLib{} -> mempty
      comp = compiler lbi
      platform = hostPlatform lbi
      implInfo = getImplInfo comp
      runGhcProg = runGHC verbosity ghcProg comp platform

  let bnfo = gbuildInfo bm

  -- the name that GHC really uses (e.g., with .exe on Windows for executables)
  let targetName = gbuildTargetName lbi bm
  let targetDir = buildDir lbi </> (gbuildName bm)
  let tmpDir = targetDir </> (gbuildName bm ++ "-tmp")
  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True tmpDir

  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = exeCoverage lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | gbuildIsRepl bm = mempty -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way (gbuildName bm)
        | otherwise = mempty

  rpaths <- getRPaths lbi clbi
  buildSources <- gbuildSources verbosity (package pkg_descr) (specVersion pkg_descr) tmpDir bm

  -- ensure extra lib dirs exist before passing to ghc
  cleanedExtraLibDirs <- filterM doesDirectoryExist (extraLibDirs bnfo)
  cleanedExtraLibDirsStatic <- filterM doesDirectoryExist (extraLibDirsStatic bnfo)

  let cSrcs = cSourcesFiles buildSources
      cxxSrcs = cxxSourceFiles buildSources
      inputFiles = inputSourceFiles buildSources
      inputModules = inputSourceModules buildSources
      isGhcDynamic = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      cLikeObjs = map (`replaceExtension` objExtension) cSrcs
      cxxObjs = map (`replaceExtension` objExtension) cxxSrcs
      needDynamic = gbuildNeedDynamic lbi bm
      needProfiling = withProfExe lbi

      -- build executables
      baseOpts =
        (componentGhcOptions verbosity lbi bnfo clbi tmpDir)
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeMake
            , ghcOptInputFiles =
                toNubListR $
                  if package pkg_descr == fakePackageId
                    then filter isHaskell inputFiles
                    else inputFiles
            , ghcOptInputScripts =
                toNubListR $
                  if package pkg_descr == fakePackageId
                    then filter (not . isHaskell) inputFiles
                    else []
            , ghcOptInputModules = toNubListR inputModules
            }
      staticOpts =
        baseOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcStaticOnly
            , ghcOptHPCDir = hpcdir Hpc.Vanilla
            }
      profOpts =
        baseOpts
          `mappend` mempty
            { ghcOptProfilingMode = toFlag True
            , ghcOptProfilingAuto =
                Internal.profDetailLevelFlag
                  False
                  (withProfExeDetail lbi)
            , ghcOptHiSuffix = toFlag "p_hi"
            , ghcOptObjSuffix = toFlag "p_o"
            , ghcOptExtra = hcProfOptions GHC bnfo
            , ghcOptHPCDir = hpcdir Hpc.Prof
            }
      dynOpts =
        baseOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcDynamicOnly
            , -- TODO: Does it hurt to set -fPIC for executables?
              ghcOptFPic = toFlag True
            , ghcOptHiSuffix = toFlag "dyn_hi"
            , ghcOptObjSuffix = toFlag "dyn_o"
            , ghcOptExtra = hcOptions GHC bnfo ++ hcSharedOptions GHC bnfo
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }
      dynTooOpts =
        staticOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcStaticAndDynamic
            , ghcOptDynHiSuffix = toFlag "dyn_hi"
            , ghcOptDynObjSuffix = toFlag "dyn_o"
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }
      linkerOpts =
        mempty
          { ghcOptLinkOptions =
              PD.ldOptions bnfo
                ++ [ "-static"
                   | withFullyStaticExe lbi
                   ]
                -- Pass extra `ld-options` given
                -- through to GHC's linker.
                ++ maybe
                  []
                  programOverrideArgs
                  (lookupProgram ldProgram (withPrograms lbi))
          , ghcOptLinkLibs =
              if withFullyStaticExe lbi
                then extraLibsStatic bnfo
                else extraLibs bnfo
          , ghcOptLinkLibPath =
              toNubListR $
                if withFullyStaticExe lbi
                  then cleanedExtraLibDirsStatic
                  else cleanedExtraLibDirs
          , ghcOptLinkFrameworks =
              toNubListR $
                PD.frameworks bnfo
          , ghcOptLinkFrameworkDirs =
              toNubListR $
                PD.extraFrameworkDirs bnfo
          , ghcOptInputFiles =
              toNubListR
                [tmpDir </> x | x <- cLikeObjs ++ cxxObjs]
          }
      dynLinkerOpts =
        mempty
          { ghcOptRPaths = rpaths
          , ghcOptInputFiles =
              toNubListR
                [tmpDir </> x | x <- cLikeObjs ++ cxxObjs]
          }
      replOpts =
        baseOpts
          { ghcOptExtra =
              Internal.filterGhciFlags
                (ghcOptExtra baseOpts)
                <> replOptionsFlags replFlags
          , ghcOptInputModules = replNoLoad replFlags (ghcOptInputModules baseOpts)
          , ghcOptInputFiles = replNoLoad replFlags (ghcOptInputFiles baseOpts)
          }
          -- For a normal compile we do separate invocations of ghc for
          -- compiling as for linking. But for repl we have to do just
          -- the one invocation, so that one has to include all the
          -- linker stuff too, like -l flags and any .o files from C
          -- files etc.
          `mappend` linkerOpts
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeInteractive
            , ghcOptOptimisation = toFlag GhcNoOptimisation
            }
      commonOpts
        | needProfiling = profOpts
        | needDynamic = dynOpts
        | otherwise = staticOpts
      compileOpts
        | useDynToo = dynTooOpts
        | otherwise = commonOpts
      withStaticExe = not needProfiling && not needDynamic

      -- For building exe's that use TH with -prof or -dynamic we actually have
      -- to build twice, once without -prof/-dynamic and then again with
      -- -prof/-dynamic. This is because the code that TH needs to run at
      -- compile time needs to be the vanilla ABI so it can be loaded up and run
      -- by the compiler.
      -- With dynamic-by-default GHC the TH object files loaded at compile-time
      -- need to be .dyn_o instead of .o.
      doingTH = usesTemplateHaskellOrQQ bnfo
      -- Should we use -dynamic-too instead of compiling twice?
      useDynToo =
        dynamicTooSupported
          && isGhcDynamic
          && doingTH
          && withStaticExe
          && null (hcSharedOptions GHC bnfo)
      compileTHOpts
        | isGhcDynamic = dynOpts
        | otherwise = staticOpts
      compileForTH
        | gbuildIsRepl bm = False
        | useDynToo = False
        | isGhcDynamic = doingTH && (needProfiling || withStaticExe)
        | otherwise = doingTH && (needProfiling || needDynamic)

  -- Build static/dynamic object files for TH, if needed.
  when compileForTH $
    runGhcProg
      compileTHOpts
        { ghcOptNoLink = toFlag True
        , ghcOptNumJobs = numJobs
        }

  -- Do not try to build anything if there are no input files.
  -- This can happen if the cabal file ends up with only cSrcs
  -- but no Haskell modules.
  unless
    ( (null inputFiles && null inputModules)
        || gbuildIsRepl bm
    )
    $ runGhcProg
      compileOpts
        { ghcOptNoLink = toFlag True
        , ghcOptNumJobs = numJobs
        }

  -- build any C++ sources
  unless (null cxxSrcs) $ do
    info verbosity "Building C++ Sources..."
    sequence_
      [ do
        let baseCxxOpts =
              Internal.componentCxxGhcOptions
                verbosity
                implInfo
                lbi
                bnfo
                clbi
                tmpDir
                filename
            vanillaCxxOpts =
              if isGhcDynamic
                then -- Dynamic GHC requires C++ sources to be built
                -- with -fPIC for REPL to work. See #2207.
                  baseCxxOpts{ghcOptFPic = toFlag True}
                else baseCxxOpts
            profCxxOpts =
              vanillaCxxOpts
                `mappend` mempty
                  { ghcOptProfilingMode = toFlag True
                  }
            sharedCxxOpts =
              vanillaCxxOpts
                `mappend` mempty
                  { ghcOptFPic = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  }
            opts
              | needProfiling = profCxxOpts
              | needDynamic = sharedCxxOpts
              | otherwise = vanillaCxxOpts
            -- TODO: Placing all Haskell, C, & C++ objects in a single directory
            --       Has the potential for file collisions. In general we would
            --       consider this a user error. However, we should strive to
            --       add a warning if this occurs.
            odir = fromFlag (ghcOptObjDir opts)
        createDirectoryIfMissingVerbose verbosity True odir
        needsRecomp <- checkNeedsRecompilation filename opts
        when needsRecomp $
          runGhcProg opts
      | filename <- cxxSrcs
      ]

  -- build any C sources
  unless (null cSrcs) $ do
    info verbosity "Building C Sources..."
    sequence_
      [ do
        let baseCcOpts =
              Internal.componentCcGhcOptions
                verbosity
                implInfo
                lbi
                bnfo
                clbi
                tmpDir
                filename
            vanillaCcOpts =
              if isGhcDynamic
                then -- Dynamic GHC requires C sources to be built
                -- with -fPIC for REPL to work. See #2207.
                  baseCcOpts{ghcOptFPic = toFlag True}
                else baseCcOpts
            profCcOpts =
              vanillaCcOpts
                `mappend` mempty
                  { ghcOptProfilingMode = toFlag True
                  }
            sharedCcOpts =
              vanillaCcOpts
                `mappend` mempty
                  { ghcOptFPic = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  }
            opts
              | needProfiling = profCcOpts
              | needDynamic = sharedCcOpts
              | otherwise = vanillaCcOpts
            odir = fromFlag (ghcOptObjDir opts)
        createDirectoryIfMissingVerbose verbosity True odir
        needsRecomp <- checkNeedsRecompilation filename opts
        when needsRecomp $
          runGhcProg opts
      | filename <- cSrcs
      ]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  case bm of
    GReplExe _ _ -> runReplOrWriteFlags verbosity ghcProg comp platform replFlags replOpts bnfo clbi (pkgName (PD.package pkg_descr))
    GReplFLib _ _ -> runReplOrWriteFlags verbosity ghcProg comp platform replFlags replOpts bnfo clbi (pkgName (PD.package pkg_descr))
    GBuildExe _ -> do
      let linkOpts =
            commonOpts
              `mappend` linkerOpts
              `mappend` mempty
                { ghcOptLinkNoHsMain = toFlag (null inputFiles)
                }
              `mappend` (if withDynExe lbi then dynLinkerOpts else mempty)

      info verbosity "Linking..."
      -- Work around old GHCs not relinking in this
      -- situation, see #3294
      let target = targetDir </> targetName
      when (compilerVersion comp < mkVersion [7, 7]) $ do
        e <- doesFileExist target
        when e (removeFile target)
      runGhcProg linkOpts{ghcOptOutputFile = toFlag target}
    GBuildFLib flib -> do
      let
        -- Instruct GHC to link against libHSrts.
        rtsLinkOpts :: GhcOptions
        rtsLinkOpts
          | supportsFLinkRts =
              mempty
                { ghcOptLinkRts = toFlag True
                }
          | otherwise =
              mempty
                { ghcOptLinkLibs = rtsOptLinkLibs
                , ghcOptLinkLibPath = toNubListR $ rtsLibPaths rtsInfo
                }
          where
            threaded = hasThreaded (gbuildInfo bm)
            supportsFLinkRts = compilerVersion comp >= mkVersion [9, 0]
            rtsInfo = extractRtsInfo lbi
            rtsOptLinkLibs =
              [ if needDynamic
                  then
                    if threaded
                      then dynRtsThreadedLib (rtsDynamicInfo rtsInfo)
                      else dynRtsVanillaLib (rtsDynamicInfo rtsInfo)
                  else
                    if threaded
                      then statRtsThreadedLib (rtsStaticInfo rtsInfo)
                      else statRtsVanillaLib (rtsStaticInfo rtsInfo)
              ]

        linkOpts :: GhcOptions
        linkOpts = case foreignLibType flib of
          ForeignLibNativeShared ->
            commonOpts
              `mappend` linkerOpts
              `mappend` dynLinkerOpts
              `mappend` rtsLinkOpts
              `mappend` mempty
                { ghcOptLinkNoHsMain = toFlag True
                , ghcOptShared = toFlag True
                , ghcOptFPic = toFlag True
                , ghcOptLinkModDefFiles = toNubListR $ gbuildModDefFiles bm
                }
              -- See Note [RPATH]
              `mappend` ifNeedsRPathWorkaround
                lbi
                mempty
                  { ghcOptLinkOptions = ["-Wl,--no-as-needed"]
                  , ghcOptLinkLibs = ["ffi"]
                  }
          ForeignLibNativeStatic ->
            -- this should be caught by buildFLib
            -- (and if we do implement this, we probably don't even want to call
            -- ghc here, but rather Ar.createArLibArchive or something)
            cabalBug "static libraries not yet implemented"
          ForeignLibTypeUnknown ->
            cabalBug "unknown foreign lib type"
      -- We build under a (potentially) different filename to set a
      -- soname on supported platforms.  See also the note for
      -- @flibBuildName@.
      info verbosity "Linking..."
      let buildName = flibBuildName lbi flib
      runGhcProg linkOpts{ghcOptOutputFile = toFlag (targetDir </> buildName)}
      renameFile (targetDir </> buildName) (targetDir </> targetName)

{-
Note [RPATH]
~~~~~~~~~~~~

Suppose that the dynamic library depends on `base`, but not (directly) on
`integer-gmp` (which, however, is a dependency of `base`). We will link the
library as

    gcc ... -lHSbase-4.7.0.2-ghc7.8.4 -lHSinteger-gmp-0.5.1.0-ghc7.8.4 ...

However, on systems (like Ubuntu) where the linker gets called with `-as-needed`
by default, the linker will notice that `integer-gmp` isn't actually a direct
dependency and hence omit the link.

Then when we attempt to link a C program against this dynamic library, the
_static_ linker will attempt to verify that all symbols can be resolved.  The
dynamic library itself does not require any symbols from `integer-gmp`, but
`base` does. In order to verify that the symbols used by `base` can be
resolved, the static linker needs to be able to _find_ integer-gmp.

Finding the `base` dependency is simple, because the dynamic elf header
(`readelf -d`) for the library that we have created looks something like

    (NEEDED) Shared library: [libHSbase-4.7.0.2-ghc7.8.4.so]
    (RPATH)  Library rpath: [/path/to/base-4.7.0.2:...]

However, when it comes to resolving the dependency on `integer-gmp`, it needs
to look at the dynamic header for `base`. On modern ghc (7.8 and higher) this
looks something like

    (NEEDED) Shared library: [libHSinteger-gmp-0.5.1.0-ghc7.8.4.so]
    (RPATH)  Library rpath: [$ORIGIN/../integer-gmp-0.5.1.0:...]

This specifies the location of `integer-gmp` _in terms of_ the location of base
(using the `$ORIGIN`) variable. But here's the crux: when the static linker
attempts to verify that all symbols can be resolved, [**IT DOES NOT RESOLVE
`$ORIGIN`**](http://stackoverflow.com/questions/6323603/ld-using-rpath-origin-inside-a-shared-library-recursive).
As a consequence, it will not be able to resolve the symbols and report the
missing symbols as errors, _even though the dynamic linker **would** be able to
resolve these symbols_. We can tell the static linker not to report these
errors by using `--unresolved-symbols=ignore-all` and all will be fine when we
run the program ([(indeed, this is what the gold linker
does)](https://sourceware.org/ml/binutils/2013-05/msg00038.html), but it makes
the resulting library more difficult to use.

Instead what we can do is make sure that the generated dynamic library has
explicit top-level dependencies on these libraries. This means that the static
linker knows where to find them, and when we have transitive dependencies on
the same libraries the linker will only load them once, so we avoid needing to
look at the `RPATH` of our dependencies. We can do this by passing
`--no-as-needed` to the linker, so that it doesn't omit any libraries.

Note that on older ghc (7.6 and before) the Haskell libraries don't have an
RPATH set at all, which makes it even more important that we make these
top-level dependencies.

Finally, we have to explicitly link against `libffi` for the same reason. For
newer ghc this _happens_ to be unnecessary on many systems because `libffi` is
a library which is not specific to GHC, and when the static linker verifies
that all symbols can be resolved it will find the `libffi` that is globally
installed (completely independent from ghc). Of course, this may well be the
_wrong_ version of `libffi`, but it's quite possible that symbol resolution
happens to work. This is of course the wrong approach, which is why we link
explicitly against `libffi` so that we will find the _right_ version of
`libffi`.
-}

-- | Do we need the RPATH workaround?
--
-- See Note [RPATH].
ifNeedsRPathWorkaround :: Monoid a => LocalBuildInfo -> a -> a
ifNeedsRPathWorkaround lbi a =
  case hostPlatform lbi of
    Platform _ Linux -> a
    _otherwise -> mempty

data DynamicRtsInfo = DynamicRtsInfo
  { dynRtsVanillaLib :: FilePath
  , dynRtsThreadedLib :: FilePath
  , dynRtsDebugLib :: FilePath
  , dynRtsEventlogLib :: FilePath
  , dynRtsThreadedDebugLib :: FilePath
  , dynRtsThreadedEventlogLib :: FilePath
  }

data StaticRtsInfo = StaticRtsInfo
  { statRtsVanillaLib :: FilePath
  , statRtsThreadedLib :: FilePath
  , statRtsDebugLib :: FilePath
  , statRtsEventlogLib :: FilePath
  , statRtsThreadedDebugLib :: FilePath
  , statRtsThreadedEventlogLib :: FilePath
  , statRtsProfilingLib :: FilePath
  , statRtsThreadedProfilingLib :: FilePath
  }

data RtsInfo = RtsInfo
  { rtsDynamicInfo :: DynamicRtsInfo
  , rtsStaticInfo :: StaticRtsInfo
  , rtsLibPaths :: [FilePath]
  }

-- | Extract (and compute) information about the RTS library
--
-- TODO: This hardcodes the name as @HSrts-ghc<version>@. I don't know if we can
-- find this information somewhere. We can lookup the 'hsLibraries' field of
-- 'InstalledPackageInfo' but it will tell us @["HSrts", "Cffi"]@, which
-- doesn't really help.
extractRtsInfo :: LocalBuildInfo -> RtsInfo
extractRtsInfo lbi =
  case PackageIndex.lookupPackageName
    (installedPkgs lbi)
    (mkPackageName "rts") of
    [(_, [rts])] -> aux rts
    _otherwise -> error "No (or multiple) ghc rts package is registered"
  where
    aux :: InstalledPackageInfo -> RtsInfo
    aux rts =
      RtsInfo
        { rtsDynamicInfo =
            DynamicRtsInfo
              { dynRtsVanillaLib = withGhcVersion "HSrts"
              , dynRtsThreadedLib = withGhcVersion "HSrts_thr"
              , dynRtsDebugLib = withGhcVersion "HSrts_debug"
              , dynRtsEventlogLib = withGhcVersion "HSrts_l"
              , dynRtsThreadedDebugLib = withGhcVersion "HSrts_thr_debug"
              , dynRtsThreadedEventlogLib = withGhcVersion "HSrts_thr_l"
              }
        , rtsStaticInfo =
            StaticRtsInfo
              { statRtsVanillaLib = "HSrts"
              , statRtsThreadedLib = "HSrts_thr"
              , statRtsDebugLib = "HSrts_debug"
              , statRtsEventlogLib = "HSrts_l"
              , statRtsThreadedDebugLib = "HSrts_thr_debug"
              , statRtsThreadedEventlogLib = "HSrts_thr_l"
              , statRtsProfilingLib = "HSrts_p"
              , statRtsThreadedProfilingLib = "HSrts_thr_p"
              }
        , rtsLibPaths = InstalledPackageInfo.libraryDirs rts
        }
    withGhcVersion = (++ ("-ghc" ++ prettyShow (compilerVersion (compiler lbi))))

-- | Returns True if the modification date of the given source file is newer than
-- the object file we last compiled for it, or if no object file exists yet.
checkNeedsRecompilation :: FilePath -> GhcOptions -> IO Bool
checkNeedsRecompilation filename opts = filename `moreRecentFile` oname
  where
    oname = getObjectFileName filename opts

-- | Finds the object file name of the given source file
getObjectFileName :: FilePath -> GhcOptions -> FilePath
getObjectFileName filename opts = oname
  where
    odir = fromFlag (ghcOptObjDir opts)
    oext = fromFlagOrDefault "o" (ghcOptObjSuffix opts)
    oname = odir </> replaceExtension filename oext

-- | Calculate the RPATHs for the component we are building.
--
-- Calculates relative RPATHs when 'relocatable' is set.
getRPaths
  :: LocalBuildInfo
  -> ComponentLocalBuildInfo
  -- ^ Component we are building
  -> IO (NubListR FilePath)
getRPaths lbi clbi | supportRPaths hostOS = do
  libraryPaths <- depLibraryPaths False (relocatable lbi) lbi clbi
  let hostPref = case hostOS of
        OSX -> "@loader_path"
        _ -> "$ORIGIN"
      relPath p = if isRelative p then hostPref </> p else p
      rpaths = toNubListR (map relPath libraryPaths)
  return rpaths
  where
    (Platform _ hostOS) = hostPlatform lbi
    compid = compilerId . compiler $ lbi

    -- The list of RPath-supported operating systems below reflects the
    -- platforms on which Cabal's RPATH handling is tested. It does _NOT_
    -- reflect whether the OS supports RPATH.

    -- E.g. when this comment was written, the *BSD operating systems were
    -- untested with regards to Cabal RPATH handling, and were hence set to
    -- 'False', while those operating systems themselves do support RPATH.
    supportRPaths Linux = True
    supportRPaths Windows = False
    supportRPaths OSX = True
    supportRPaths FreeBSD =
      case compid of
        CompilerId GHC ver | ver >= mkVersion [7, 10, 2] -> True
        _ -> False
    supportRPaths OpenBSD = False
    supportRPaths NetBSD = False
    supportRPaths DragonFly = False
    supportRPaths Solaris = False
    supportRPaths AIX = False
    supportRPaths HPUX = False
    supportRPaths IRIX = False
    supportRPaths HaLVM = False
    supportRPaths IOS = False
    supportRPaths Android = False
    supportRPaths Ghcjs = False
    supportRPaths Wasi = False
    supportRPaths Hurd = False
    supportRPaths (OtherOS _) = False
-- Do _not_ add a default case so that we get a warning here when a new OS
-- is added.

getRPaths _ _ = return mempty

-- | Determine whether the given 'BuildInfo' is intended to link against the
-- threaded RTS. This is used to determine which RTS to link against when
-- building a foreign library with a GHC without support for @-flink-rts@.
hasThreaded :: BuildInfo -> Bool
hasThreaded bi = elem "-threaded" ghc
  where
    PerCompilerFlavor ghc _ = options bi
>>>>>>> 8f2f96212 (Always pass `ghc-options`)

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
          , ghcOptExtra = hcOptions GHC libBi ++ hcSharedOptions GHC libBi
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
    profDynArgs =
      vanillaArgs
        `mappend` mempty
          { ghcOptProfilingMode = toFlag True
          , ghcOptProfilingAuto =
              Internal.profDetailLevelFlag
                True
                (withProfLibDetail lbi)
          , ghcOptDynLinkMode = toFlag GhcDynamicOnly
          , ghcOptFPic = toFlag True
          , ghcOptHiSuffix = toFlag "p_dyn_hi"
          , ghcOptObjSuffix = toFlag "p_dyn_o"
          , ghcOptExtra = hcProfSharedOptions GHC libBi
          }
    ghcArgs =
      let (libWays, _, _) = buildWays lbi
       in case libWays (componentIsIndefinite clbi) of
            (ProfDynWay : _) -> profDynArgs
            (ProfWay : _) -> profArgs
            (StaticWay : _) -> vanillaArgs
            (DynWay : _) -> sharedArgs
            _ -> error "libAbiHash: Can't find an enabled library way"

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
  let
    (wantedLibWays, _, _) = buildWays lbi
    isIndef = componentIsIndefinite clbi
    libWays = wantedLibWays isIndef

  info verbosity ("Wanted install ways: " ++ show libWays)

  -- copy .hi files over:
  forM_ (wantedLibWays isIndef) $ \w -> case w of
    StaticWay -> copyModuleFiles (Suffix "hi")
    DynWay -> copyModuleFiles (Suffix "dyn_hi")
    ProfWay -> copyModuleFiles (Suffix "p_hi")
    ProfDynWay -> copyModuleFiles (Suffix "p_dyn_hi")

  -- copy extra compilation artifacts that ghc plugins may produce
  copyDirectoryIfExists extraCompilationArtifacts

  -- copy the built library files over:
  when (has_code && hasLib) $ do
    forM_ libWays $ \w -> case w of
      StaticWay -> do
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
      ProfWay -> do
        installOrdinary builtDir targetDir profileLibName
        whenGHCi $ installOrdinary builtDir targetDir ghciProfLibName
      ProfDynWay -> do
        installShared
          builtDir
          dynlibTargetDir
          (mkProfSharedLibName platform compiler_id uid)
      DynWay -> do
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
                            (l ++ f)
                    forM_ files $ \file ->
                      when (l' `isPrefixOf` file) $ do
                        isFile <- doesFileExist (i $ builtDir </> makeRelativePathEx file)
                        when isFile $ do
                          installShared
                            builtDir
                            dynlibTargetDir
                            file
                  | l <- extraBundledLibs (libBuildInfo lib)
                  , f <- "" : extraDynLibFlavours (libBuildInfo lib)
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
    whenGHCi = when (hasLib && withGHCiLib lbi && has_code)

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
