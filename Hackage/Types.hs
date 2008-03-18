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

import Distribution.Package (PackageIdentifier(..), showPackageId, Package(..), Dependency)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Version (showVersion)
import Distribution.Simple.Utils (intercalate)

import System.FilePath ((</>), (<.>))

type Username = String
type Password = String

-- | We re-use @GenericPackageDescription@ and use the @package-url@
-- field to store the tarball URL.
data PkgInfo = PkgInfo {
    pkgInfoId :: PackageIdentifier,
    pkgRepo   :: Repo,
    pkgDesc   :: GenericPackageDescription
  }
  deriving (Show)

instance Package PkgInfo where packageId = pkgInfoId

-- |Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
packageFile :: PkgInfo -> FilePath
packageFile pkg = packageDir pkg
              </> showPackageId (packageId pkg)
              <.> "tar.gz"

-- |Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifer@ is stored.
packageDir :: PkgInfo -> FilePath
packageDir PkgInfo { pkgInfoId = p, pkgRepo = repo } = 
                         repoCacheDir repo
                     </> pkgName p
                     </> showVersion (pkgVersion p)

-- | Generate the URL of the tarball for a given package.
packageURL :: PkgInfo -> String
packageURL pkg = intercalate "/"
    [repoURL (pkgRepo pkg),
     pkgName p, showVersion (pkgVersion p),
     showPackageId p ++ ".tar.gz"]
    where p = packageId pkg

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

data ResolvedDependency
       = InstalledDependency Dependency PackageIdentifier
       | AvailableDependency Dependency PkgInfo FlagAssignment [ResolvedDependency]
       | UnavailableDependency Dependency
       deriving (Show)

-- | Explicit user's assignment of configurations flags,
-- eg --flags=foo --flags=-bar
-- becomes [("foo", True), ("bar", False)]
type FlagAssignment = [(String, Bool)]

fulfills :: ResolvedDependency -> Dependency
fulfills (InstalledDependency d _) = d
fulfills (AvailableDependency d _ _ _) = d
fulfills (UnavailableDependency d) = d

data UnresolvedDependency
    = UnresolvedDependency
    { dependency :: Dependency
    , depFlags   :: FlagAssignment
    }
  deriving (Show)
