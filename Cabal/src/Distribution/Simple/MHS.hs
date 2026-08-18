{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.MHS
-- Copyright   :  Cabal Devs 2026
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the MHS-specific code for configuring, building
-- and installing packages.

module Distribution.Simple.MHS
  ( configure
  , getInstalledPackages
  , buildLib
  , buildExe
  , installLib
  , registerPackage
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.InstalledPackageInfo
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Verbosity
import qualified Data.Map as Map (empty)
import Distribution.System (Platform)
import Distribution.Version (mkVersion, orLaterVersion)
import Language.Haskell.Extension
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex

-- -----------------------------------------------------------------------------
-- Configuring

configure
  :: Verbosity
  -> Maybe FilePath
  -> ProgramDb
  -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath progdb = do
  (_mhsProg, mhsVersion, progdb') <-
    requireProgramVersion
      verbosity
      mhsProgram
      (orLaterVersion (mkVersion [0, 10, 0]))
      (userMaybeSpecifyPath "mhs" hcPath progdb)

  let comp =
        Compiler
          { compilerId = CompilerId MHS mhsVersion
          , compilerAbiTag = NoAbiTag
          , compilerCompat = []
          , compilerLanguages = mhsLanguages
          , compilerExtensions = mhsLanguageExtensions
          , compilerProperties = Map.empty
          , compilerWiredInUnitIds = Nothing
          }
      compPlatform = Nothing
  info verbosity $ "MHS configured with languages: " ++ show (map fst mhsLanguages)
  return (comp, compPlatform, progdb')

mhsLanguages :: [(Language, CompilerFlag)]
mhsLanguages = [(Haskell2010, ""), (Haskell98, "")] -- MHS docs say it supports 2010 but 98 should be fine too

mhsLanguageExtensions :: [(Extension, Maybe CompilerFlag)]
mhsLanguageExtensions =
  [
  -- TODO: use the list from the MHS documentation
  ]

getInstalledPackages
  :: Verbosity
  -> Compiler
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackX (SymbolicPath from (Dir PkgDB))
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages _verbosity _comp _mbWorkDir _packagedbs _progdb =
  return (PackageIndex.fromList [])

-- -----------------------------------------------------------------------------
-- Building

buildLib
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildLib _verbosity _pkg_descr _lbi _lib _clbi = return ()

buildExe
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
buildExe verbosity _pkg_descr lbi exe _clbi = do
  let runMhsProg = runDbProgramCwd verbosity (mbWorkDirLBI lbi) mhsProgram (withPrograms lbi)
  
  let mbWorkDir = mbWorkDirLBI lbi
  srcMainPath <- findFileCwd verbosity mbWorkDir (hsSourceDirs $ buildInfo exe) (modulePath exe)
  
  let odir = buildDir lbi
      u = interpretSymbolicPathCWD
      
      mhsArgs = 
           ["-i" ++ u d | d <- hsSourceDirs (buildInfo exe)]
        ++ ["-o", u $ odir </> makeRelativePathEx (prettyShow (exeName exe))]
        ++ [u srcMainPath]

  runMhsProg mhsArgs

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
installLib _verbosity _lbi _targetDir _dynlibTargetDir _builtDir _pkg _library _clbi = return ()

-- -----------------------------------------------------------------------------
-- Registering

registerPackage
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> Compiler
  -> ProgramDb
  -> PackageDBStackS from
  -> InstalledPackageInfo
  -> IO ()
registerPackage _verbosity _mbWorkDir _comp _progdb _packageDbs _installedPkgInfo = return ()
