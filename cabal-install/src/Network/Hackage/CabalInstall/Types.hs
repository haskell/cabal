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

import Distribution.Simple.Compiler (CompilerFlavor(..),Compiler)
import Distribution.Simple.Program  (ProgramConfiguration)
import Distribution.Package (PackageIdentifier)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Version (Dependency)
import Distribution.Verbosity

import System.IO (Handle)

-- | We re-use @GenericPackageDescription@ and use the @package-url@
-- field to store the tarball URL.
data PkgInfo = PkgInfo {
                        pkgRepo :: Repo,
                        pkgDesc :: GenericPackageDescription
                       }

data Action
    = FetchCmd
    | InstallCmd
    | BuildDepCmd
    | CleanCmd
    | UpdateCmd
    | InfoCmd
    | HelpCmd
    | ListCmd
 deriving (Eq)

data TempFlags = TempFlags {
        tempHcFlavor    :: Maybe CompilerFlavor,
        tempHcPath      :: Maybe FilePath, -- ^given compiler location
        tempConfDir     :: Maybe FilePath,
        tempCacheDir    :: Maybe FilePath,
        tempHcPkg       :: Maybe FilePath, -- ^given hc-pkg location
        tempPrefix      :: Maybe FilePath,
        tempTarPath     :: Maybe FilePath,
        tempVerbose     :: Verbosity,            -- ^verbosity level
--        tempUpgradeDeps :: Bool,
        tempUserIns     :: Bool,           -- ^--user-install flag
        tempHelp        :: Bool
   }

data ConfigFlags = ConfigFlags {
        configCompiler    :: Compiler,
	configPrograms    :: ProgramConfiguration,
        configConfDir     :: FilePath,
        configCacheDir    :: FilePath,
        configPrefix      :: Maybe FilePath,
        configServers     :: [Repo],       -- ^Available Hackage servers.
        configTarPath     :: FilePath,
        configOutputGen   :: OutputGen,
        configVerbose     :: Verbosity,
--        configUpgradeDeps :: Bool,
        configUserIns     :: Bool            -- ^--user-install flag
   }

data Repo = Repo {
                  repoName :: String,
                  repoURL :: String
                 }
          deriving (Show,Eq)

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
                        -> (PackageIdentifier,Repo,[ResolvedPackage])
                        -> IO ()
      , showOtherPackageInfo :: Maybe PackageIdentifier -- package if installed.
                             -> Dependency
                             -> IO () -- Show package which isn't available from any server.
      , cmdStdout      :: Maybe Handle
      , cmdStderr      :: Maybe Handle
      , -- | Output a message.
        message :: Verbosity -- ^ minimum verbosity needed to output this message
                -> String -> IO ()
      }



data ResolvedPackage
    = ResolvedPackage
    { fulfilling :: Dependency
    , resolvedData :: Maybe ( PackageIdentifier -- pkg id
                            , Repo            -- pkg location
                            , [ResolvedPackage] -- pkg dependencies
                            )
    , pkgOptions :: [String]
    } deriving Eq

data UnresolvedDependency
    = UnresolvedDependency
    { dependency :: Dependency
    , depOptions :: [String]
    }

data ResolvedDependency
    = ResolvedDependency PackageIdentifier Repo [(Dependency,Maybe ResolvedDependency)]
      deriving (Eq,Show)
