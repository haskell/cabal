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

import Distribution.Package
         ( PackageIdentifier(..), Package(..), Dependency )
import Distribution.PackageDescription
         ( GenericPackageDescription, FlagAssignment )

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }

-- | We re-use @GenericPackageDescription@ and use the @package-url@
-- field to store the tarball URL.
data AvailablePackage = AvailablePackage {
    packageInfoId      :: PackageIdentifier,
    packageDescription :: GenericPackageDescription,
    packageSource      :: AvailablePackageSource
  }
  deriving Show

instance Package AvailablePackage where packageId = packageInfoId

data AvailablePackageSource =

    -- | The unpacked package in the current dir
    LocalUnpackedPackage
    
    -- | A package available as a tarball from a remote repository, with a
    -- locally cached copy. ie a package available from hackage
  | RepoTarballPackage Repo

--  | ScmPackage
  deriving Show

--TODO:
--  * generalise local package to any local unpacked package, not just in the
--      current dir, ie add a FilePath param
--  * add support for darcs and other SCM style remote repos with a local cache

data RemoteRepo = RemoteRepo {
    remoteRepoName :: String,
    remoteRepoURL  :: String
  }
  deriving (Show,Eq)

data Repo = Repo {
    repoRemote   :: RemoteRepo,
    repoCacheDir :: FilePath
  }
  deriving (Show,Eq)

repoName :: Repo -> String
repoName = remoteRepoName . repoRemote

repoURL :: Repo -> String
repoURL = remoteRepoURL . repoRemote

data UnresolvedDependency
    = UnresolvedDependency
    { dependency :: Dependency
    , depFlags   :: FlagAssignment
    }
  deriving (Show)
