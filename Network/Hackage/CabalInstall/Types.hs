-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- All data types for the entire cabal-install system gathered here to avoid some .hs-boot files.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Types where

import Distribution.Setup (CompilerFlavor(..),Compiler)
import Distribution.Package (PackageIdentifier)
import Distribution.Version (Dependency)

import System.IO (Handle)

data PkgInfo = PkgInfo
    { infoId        :: PackageIdentifier
    , infoDeps      :: [Dependency]
    , infoSynopsis  :: String
    , infoURL       :: String
    }
    deriving (Show, Read, Eq)

data Action
    = FetchCmd
    | InstallCmd
    | BuildDepCmd
    | CleanCmd
    | UpdateCmd
    | InfoCmd
    | HelpCmd
    | ListCmd

data TempFlags = TempFlags {
        tempHcFlavor    :: Maybe CompilerFlavor,
        tempHcPath      :: Maybe FilePath, -- ^given compiler location
        tempConfPath    :: Maybe FilePath,
        tempHcPkg       :: Maybe FilePath, -- ^given hc-pkg location
        tempPrefix      :: Maybe FilePath,
        tempServers     :: [String],       -- ^Available Hackage servers.
        tempTarPath     :: Maybe FilePath,
        tempRunHc       :: Maybe FilePath,
        tempVerbose     :: Int,            -- ^verbosity level
--        tempUpgradeDeps :: Bool,
        tempUser        :: Bool,           -- ^--user flag
        tempUserIns     :: Bool            -- ^--user-install flag
   }

data ConfigFlags = ConfigFlags {
        configCompiler    :: Compiler,
        configConfPath    :: FilePath,
        configPrefix      :: Maybe FilePath,
        configServers     :: [String],       -- ^Available Hackage servers.
        configTarPath     :: FilePath,
        configRunHc       :: FilePath,
        configOutputGen   :: OutputGen,
        configVerbose     :: Int,
--        configUpgradeDeps :: Bool,
        configUser        :: Bool,           -- ^--user flag
        configUserIns     :: Bool            -- ^--user-install flag
   }

data Flag
    = GhcFlag | NhcFlag | HugsFlag
    | WithCompiler FilePath | WithHcPkg FilePath
    | WithConfPath FilePath | WithTarPath FilePath
    | WithServer String
    | UserFlag | GlobalFlag
    | UserInstallFlag | GlobalInstallFlag
--    | UpgradeDeps
    | HelpFlag
    | Verbose Int


data OutputGen
    = OutputGen
      { prepareInstall :: [(PackageIdentifier,[String],String)] -> IO ()
      , pkgIsPresent   :: PackageIdentifier -> IO ()
      , downloadingPkg :: PackageIdentifier -> IO ()
      , executingCmd   :: String -> [String] -> IO ()
      , cmdFailed      :: String -> [String] -> Int -> IO () -- cmd, flags and errno.
      , buildingPkg    :: PackageIdentifier -> IO () -- Package is fetched and unpacked. Starting installation.
      , stepConfigPkg  :: PackageIdentifier -> IO ()
      , stepBuildPkg   :: PackageIdentifier -> IO ()
      , stepInstallPkg :: PackageIdentifier -> IO ()
      , stepFinishedPkg:: PackageIdentifier -> IO ()
      , noSetupScript  :: PackageIdentifier -> IO ()
      , noCabalFile    :: PackageIdentifier -> IO ()
      , gettingPkgList :: String -> IO () -- Server.
      , showPackageInfo :: Maybe FilePath -- pkg file if fetched.
                        -> Bool -- is installed
                        -> [String] -- Options
                        -> Dependency -- Which dependency is this package supposed to fill
                        -> (PackageIdentifier,String,[ResolvedPackage])
                        -> IO ()
      , showOtherPackageInfo :: Maybe PackageIdentifier -- package if installed.
                             -> Dependency
                             -> IO () -- Show package which isn't available from any server.
      , cmdStdout      :: Maybe Handle
      , cmdStderr      :: Maybe Handle
      }



data ResolvedPackage
    = ResolvedPackage
    { fulfilling :: Dependency
    , resolvedData :: Maybe ( PackageIdentifier -- pkg id
                            , String            -- pkg location
                            , [ResolvedPackage] -- pkg dependencies
                            )
    , pkgOptions :: [String]
    } deriving Eq

data UnresolvedDependency
    = UnresolvedDependency
    { dependency :: Dependency
    , depOptions :: [String]
    }


