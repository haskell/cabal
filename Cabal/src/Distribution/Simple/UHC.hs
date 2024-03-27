{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.UHC
-- Copyright   :  Andres Loeh 2009
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the UHC-specific code for configuring, building
-- and installing packages.
--
-- Thanks to the authors of the other implementation-specific files, in
-- particular to Isaac Jones, Duncan Coutts and Henning Thielemann, for
-- inspiration on how to design this module.
module Distribution.Simple.UHC
  ( configure
  , getInstalledPackages
  , buildLib
  , buildExe
  , installLib
  , registerPackage
  , inplacePackageDbPath
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.InstalledPackageInfo
import Distribution.Package hiding (installedUnitId)
import Distribution.PackageDescription
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.MungedPackageId
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension

import qualified Data.Map as Map (empty)
import System.Directory
import System.FilePath (pathSeparator)

-- -----------------------------------------------------------------------------
-- Configuring

configure
  :: Verbosity
  -> Maybe FilePath
  -> Maybe FilePath
  -> ProgramDb
  -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath _hcPkgPath progdb = do
  (_uhcProg, uhcVersion, progdb') <-
    requireProgramVersion
      verbosity
      uhcProgram
      (orLaterVersion (mkVersion [1, 0, 2]))
      (userMaybeSpecifyPath "uhc" hcPath progdb)

  let comp =
        Compiler
          { compilerId = CompilerId UHC uhcVersion
          , compilerAbiTag = NoAbiTag
          , compilerCompat = []
          , compilerLanguages = uhcLanguages
          , compilerExtensions = uhcLanguageExtensions
          , compilerProperties = Map.empty
          }
      compPlatform = Nothing
  return (comp, compPlatform, progdb')

uhcLanguages :: [(Language, CompilerFlag)]
uhcLanguages = [(Haskell98, "")]

-- | The flags for the supported extensions.
uhcLanguageExtensions :: [(Extension, Maybe CompilerFlag)]
uhcLanguageExtensions =
  let doFlag (f, (enable, disable)) =
        [ (EnableExtension f, enable)
        , (DisableExtension f, disable)
        ]
      alwaysOn = (Nothing, Nothing {- wrong -})
   in concatMap
        doFlag
        [ (CPP, (Just "--cpp", Nothing {- wrong -}))
        , (PolymorphicComponents, alwaysOn)
        , (ExistentialQuantification, alwaysOn)
        , (ForeignFunctionInterface, alwaysOn)
        , (UndecidableInstances, alwaysOn)
        , (MultiParamTypeClasses, alwaysOn)
        , (Rank2Types, alwaysOn)
        , (PatternSignatures, alwaysOn)
        , (EmptyDataDecls, alwaysOn)
        , (ImplicitPrelude, (Nothing, Just "--no-prelude" {- wrong -}))
        , (TypeOperators, alwaysOn)
        , (OverlappingInstances, alwaysOn)
        , (FlexibleInstances, alwaysOn)
        ]

getInstalledPackages
  :: Verbosity
  -> Compiler
  -> PackageDBStack
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages verbosity comp packagedbs progdb = do
  let compilerid = compilerId comp
  systemPkgDir <- getGlobalPackageDir verbosity progdb
  userPkgDir <- getUserPackageDir
  let pkgDirs = nub (concatMap (packageDbPaths userPkgDir systemPkgDir) packagedbs)
  -- putStrLn $ "pkgdirs: " ++ show pkgDirs
  pkgs <-
    liftM (map addBuiltinVersions . concat) $
      traverse
        (\d -> getDirectoryContents d >>= filterM (isPkgDir (prettyShow compilerid) d))
        pkgDirs
  -- putStrLn $ "pkgs: " ++ show pkgs
  let iPkgs =
        map mkInstalledPackageInfo $
          concatMap parsePackage $
            pkgs
  -- putStrLn $ "installed pkgs: " ++ show iPkgs
  return (fromList iPkgs)

getGlobalPackageDir :: Verbosity -> ProgramDb -> IO FilePath
getGlobalPackageDir verbosity progdb = do
  output <-
    getDbProgramOutput
      verbosity
      uhcProgram
      progdb
      ["--meta-pkgdir-system"]
  -- we need to trim because pkgdir contains an extra newline at the end
  let pkgdir = trimEnd output
  return pkgdir
  where
    trimEnd = dropWhileEnd isSpace

getUserPackageDir :: IO FilePath
getUserPackageDir = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".cabal" </> "lib" -- TODO: determine in some other way

packageDbPaths :: FilePath -> FilePath -> PackageDB -> [FilePath]
packageDbPaths user system db =
  case db of
    GlobalPackageDB -> [system]
    UserPackageDB -> [user]
    SpecificPackageDB path -> [path]

-- | Hack to add version numbers to UHC-built-in packages. This should sooner or
-- later be fixed on the UHC side.
addBuiltinVersions :: String -> String
{-
addBuiltinVersions "uhcbase"  = "uhcbase-1.0"
addBuiltinVersions "base"  = "base-3.0"
addBuiltinVersions "array" = "array-0.2"
-}
addBuiltinVersions xs = xs

-- | Name of the installed package config file.
installedPkgConfig :: String
installedPkgConfig = "installed-pkg-config"

-- | Check if a certain dir contains a valid package. Currently, we are
-- looking only for the presence of an installed package configuration.
-- TODO: Actually make use of the information provided in the file.
isPkgDir :: String -> String -> String -> IO Bool
isPkgDir _ _ ('.' : _) = return False -- ignore files starting with a .
isPkgDir c dir xs = do
  let candidate = dir </> uhcPackageDir xs c
  -- putStrLn $ "trying: " ++ candidate
  doesFileExist (candidate </> installedPkgConfig)

parsePackage :: String -> [PackageId]
parsePackage = toList . simpleParsec

-- | Create a trivial package info from a directory name.
mkInstalledPackageInfo :: PackageId -> InstalledPackageInfo
mkInstalledPackageInfo p =
  emptyInstalledPackageInfo
    { installedUnitId = mkLegacyUnitId p
    , sourcePackageId = p
    }

-- -----------------------------------------------------------------------------
-- Building

buildLib
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  systemPkgDir <- getGlobalPackageDir verbosity (withPrograms lbi)
  userPkgDir <- getUserPackageDir
  let runUhcProg = runDbProgram verbosity uhcProgram (withPrograms lbi)
  let uhcArgs =
        -- set package name
        ["--pkg-build=" ++ prettyShow (packageId pkg_descr)]
          -- common flags lib/exe
          ++ constructUHCCmdLine
            userPkgDir
            systemPkgDir
            lbi
            (libBuildInfo lib)
            clbi
            (buildDir lbi)
            verbosity
          -- source files
          -- suboptimal: UHC does not understand module names, so
          -- we replace periods by path separators
          ++ map
            (map (\c -> if c == '.' then pathSeparator else c))
            (map prettyShow (allLibModules lib clbi))

  runUhcProg uhcArgs

  return ()

buildExe
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
buildExe verbosity _pkg_descr lbi exe clbi = do
  systemPkgDir <- getGlobalPackageDir verbosity (withPrograms lbi)
  userPkgDir <- getUserPackageDir
  let mbWorkDir = mbWorkDirLBI lbi
  srcMainPath <- findFileCwd verbosity mbWorkDir (hsSourceDirs $ buildInfo exe) (modulePath exe)
  let runUhcProg = runDbProgram verbosity uhcProgram (withPrograms lbi)
      i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path
      uhcArgs =
        -- common flags lib/exe
        constructUHCCmdLine
          userPkgDir
          systemPkgDir
          lbi
          (buildInfo exe)
          clbi
          (buildDir lbi)
          verbosity
          -- output file
          ++ ["--output", i $ buildDir lbi </> makeRelativePathEx (prettyShow (exeName exe))]
          -- main source module
          ++ [i $ srcMainPath]
  runUhcProg uhcArgs

constructUHCCmdLine
  :: FilePath
  -> FilePath
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Dir Build)
  -> Verbosity
  -> [String]
constructUHCCmdLine user system lbi bi clbi odir verbosity =
  -- verbosity
  ( if verbosity >= deafening
      then ["-v4"]
      else
        if verbosity >= normal
          then []
          else ["-v0"]
  )
    ++ hcOptions UHC bi
    -- flags for language extensions
    ++ languageToFlags (compiler lbi) (defaultLanguage bi)
    ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
    -- packages
    ++ ["--hide-all-packages"]
    ++ uhcPackageDbOptions user system (withPackageDB lbi)
    ++ ["--package=uhcbase"]
    ++ ["--package=" ++ prettyShow (mungedName pkgid) | (_, pkgid) <- componentPackageDeps clbi]
    -- search paths
    ++ ["-i" ++ i odir]
    ++ ["-i" ++ i l | l <- nub (hsSourceDirs bi)]
    ++ ["-i" ++ i (autogenComponentModulesDir lbi clbi)]
    ++ ["-i" ++ i (autogenPackageModulesDir lbi)]
    -- cpp options
    ++ ["--optP=" ++ opt | opt <- cppOptions bi]
    -- output path
    ++ ["--odir=" ++ i odir]
    -- optimization
    ++ ( case withOptimization lbi of
          NoOptimisation -> ["-O0"]
          NormalOptimisation -> ["-O1"]
          MaximumOptimisation -> ["-O2"]
       )
  where
    i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path

uhcPackageDbOptions :: FilePath -> FilePath -> PackageDBStack -> [String]
uhcPackageDbOptions user system db =
  map
    (\x -> "--pkg-searchpath=" ++ x)
    (concatMap (packageDbPaths user system) db)

-- -----------------------------------------------------------------------------
-- Installation

installLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -> FilePath
  -> FilePath
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
installLib verbosity _lbi targetDir _dynlibTargetDir builtDir pkg _library _clbi = do
  -- putStrLn $ "dest:  " ++ targetDir
  -- putStrLn $ "built: " ++ builtDir
  installDirectoryContents verbosity (builtDir </> prettyShow (packageId pkg)) targetDir

-- currently hard-coded UHC code generator and variant to use
uhcTarget, uhcTargetVariant :: String
uhcTarget = "bc"
uhcTargetVariant = "plain"

-- root directory for a package in UHC
uhcPackageDir :: String -> String -> FilePath
uhcPackageSubDir :: String -> FilePath
uhcPackageDir pkgid compilerid = pkgid </> uhcPackageSubDir compilerid
uhcPackageSubDir compilerid = compilerid </> uhcTarget </> uhcTargetVariant

-- -----------------------------------------------------------------------------
-- Registering

registerPackage
  :: Verbosity
  -> Compiler
  -> ProgramDb
  -> PackageDBStack
  -> InstalledPackageInfo
  -> IO ()
registerPackage verbosity comp progdb packageDbs installedPkgInfo = do
  dbdir <- case registrationPackageDB packageDbs of
    GlobalPackageDB -> getGlobalPackageDir verbosity progdb
    UserPackageDB -> getUserPackageDir
    SpecificPackageDB dir -> return dir
  let pkgdir = dbdir </> uhcPackageDir (prettyShow pkgid) (prettyShow compilerid)
  createDirectoryIfMissingVerbose verbosity True pkgdir
  writeUTF8File
    (pkgdir </> installedPkgConfig)
    (showInstalledPackageInfo installedPkgInfo)
  where
    pkgid = sourcePackageId installedPkgInfo
    compilerid = compilerId comp

inplacePackageDbPath :: LocalBuildInfo -> SymbolicPath Pkg (Dir PkgDB)
inplacePackageDbPath lbi = coerceSymbolicPath $ buildDir lbi
