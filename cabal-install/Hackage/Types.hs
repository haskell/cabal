-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- All data types for the entire cabal-install system gathered here to avoid some .hs-boot files.
-----------------------------------------------------------------------------
module Hackage.Types where

import Distribution.Simple.Compiler (CompilerFlavor)
import Distribution.Simple.InstallDirs (InstallDirTemplates)
import Distribution.Package (PackageIdentifier)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Version (Dependency)
import Distribution.Verbosity

-- | We re-use @GenericPackageDescription@ and use the @package-url@
-- field to store the tarball URL.
data PkgInfo = PkgInfo {
                        pkgInfoId :: PackageIdentifier,
                        pkgRepo :: Repo,
                        pkgDesc :: GenericPackageDescription
                       }
               deriving (Show)

data Action
    = FetchCmd
    | InstallCmd
    | CleanCmd
    | UpdateCmd
    | InfoCmd
    | HelpCmd
    | ListCmd
 deriving (Show,Eq)

data Option = OptCompilerFlavor CompilerFlavor
            | OptCompiler FilePath
            | OptHcPkg FilePath
            | OptConfigFile FilePath
            | OptCacheDir FilePath
            | OptPrefix FilePath
            | OptBinDir FilePath
            | OptLibDir FilePath
            | OptLibSubDir FilePath
            | OptLibExecDir FilePath
            | OptDataDir FilePath
            | OptDataSubDir FilePath
            | OptDocDir FilePath
            | OptHtmlDir FilePath
            | OptUserInstall Bool
            | OptHelp
            | OptVerbose Verbosity
  deriving (Eq,Show)

data ConfigFlags = ConfigFlags {
        configCompiler    :: CompilerFlavor,
        configCompilerPath :: Maybe FilePath,
        configHcPkgPath   :: Maybe FilePath,
        configUserInstallDirs   :: InstallDirTemplates,
        configGlobalInstallDirs :: InstallDirTemplates,
        configCacheDir    :: FilePath,
        configRepos       :: [Repo],       -- ^Available Hackage servers.
        configVerbose     :: Verbosity,
        configUserInstall :: Bool            -- ^--user-install flag
   }
  deriving (Show)

data Repo = Repo {
                  repoName :: String,
                  repoURL :: String
                 }
          deriving (Show,Eq)

data ResolvedPackage = Installed Dependency PackageIdentifier
                     | Available Dependency PkgInfo [String] [ResolvedPackage]
                     | Unavailable Dependency
                       deriving (Show)

fulfills :: ResolvedPackage -> Dependency
fulfills (Installed d _) = d
fulfills (Available d _ _ _) = d
fulfills (Unavailable d) = d

data UnresolvedDependency
    = UnresolvedDependency
    { dependency :: Dependency
    , depOptions :: [String]
    }
  deriving (Show)