{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Types
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Various common data types for the entire cabal-install system
-----------------------------------------------------------------------------
module Distribution.Client.Types where

import Distribution.Package
         ( PackageName, PackageId, Package(..), ComponentId(..)
         , ComponentId(..)
         , HasComponentId(..), PackageInstalled(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Solver.PackageIndex
         ( PackageIndex )
import Distribution.Solver.ComponentDeps
         ( ComponentDeps )
import qualified Distribution.Solver.ComponentDeps as CD
import Distribution.Solver.Types
         ( SourcePackage, ConfiguredPackage(..)
         , confInstId )
import Distribution.Version
         ( VersionRange )

import Data.Map (Map)
import Network.URI (URI, nullURI)
import Control.Exception
         ( SomeException )

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }

-- | This is the information we get from a @00-index.tar.gz@ hackage index.
--
data SourcePackageDb = SourcePackageDb {
  packageIndex       :: PackageIndex (SourcePackage PackageLocation'),
  packagePreferences :: Map PackageName VersionRange
}

-- ------------------------------------------------------------
-- * Various kinds of information about packages
-- ------------------------------------------------------------

-- | Subclass of packages that have specific versioned dependencies.
--
-- So for example a not-yet-configured package has dependencies on version
-- ranges, not specific versions. A configured or an already installed package
-- depends on exact versions. Some operations or data structures (like
--  dependency graphs) only make sense on this subclass of package types.
--
class Package pkg => PackageFixedDeps pkg where
  depends :: pkg -> ComponentDeps [ComponentId]

instance PackageFixedDeps InstalledPackageInfo where
  depends = CD.fromInstalled . installedDepends

instance PackageFixedDeps (ConfiguredPackage loc) where
  depends (ConfiguredPackage _ _ _ deps) = fmap (map confInstId) deps

-- | Like 'ConfiguredPackage', but with all dependencies guaranteed to be
-- installed already, hence itself ready to be installed.
data GenericReadyPackage srcpkg ipkg
   = ReadyPackage
       srcpkg                  -- see 'ConfiguredPackage'.
       (ComponentDeps [ipkg])  -- Installed dependencies.
  deriving (Eq, Show)

type ReadyPackage = GenericReadyPackage (ConfiguredPackage PackageLocation') InstalledPackageInfo

instance Package srcpkg => Package (GenericReadyPackage srcpkg ipkg) where
  packageId (ReadyPackage srcpkg _deps) = packageId srcpkg

instance (Package srcpkg, HasComponentId ipkg) =>
         PackageFixedDeps (GenericReadyPackage srcpkg ipkg) where
  depends (ReadyPackage _ deps) = fmap (map installedComponentId) deps

instance HasComponentId srcpkg =>
         HasComponentId (GenericReadyPackage srcpkg ipkg) where
  installedComponentId (ReadyPackage pkg _) = installedComponentId pkg

-- ------------------------------------------------------------
-- * Package locations and repositories
-- ------------------------------------------------------------

type PackageLocation' = PackageLocation (Maybe FilePath)

data PackageLocation local =

    -- | An unpacked package in the given dir, or current dir
    LocalUnpackedPackage FilePath

    -- | A package as a tarball that's available as a local tarball
  | LocalTarballPackage FilePath

    -- | A package as a tarball from a remote URI
  | RemoteTarballPackage URI local

    -- | A package available as a tarball from a repository.
    --
    -- It may be from a local repository or from a remote repository, with a
    -- locally cached copy. ie a package available from hackage
  | RepoTarballPackage Repo PackageId local

--TODO:
--  * add support for darcs and other SCM style remote repos with a local cache
--  | ScmPackage
  deriving (Show, Functor)

data LocalRepo = LocalRepo
  deriving (Show,Eq)

data RemoteRepo =
    RemoteRepo {
      remoteRepoName     :: String,
      remoteRepoURI      :: URI,
      remoteRepoRootKeys :: (),

      -- | Normally a repo just specifies an HTTP or HTTPS URI, but as a
      -- special case we may know a repo supports both and want to try HTTPS
      -- if we can, but still allow falling back to HTTP.
      --
      -- This field is not currently stored in the config file, but is filled
      -- in automagically for known repos.
      remoteRepoShouldTryHttps :: Bool
    }

  -- FIXME: discuss this type some more.

  deriving (Show,Eq,Ord)

-- | Construct a partial 'RemoteRepo' value to fold the field parser list over.
emptyRemoteRepo :: String -> RemoteRepo
emptyRemoteRepo name = RemoteRepo name nullURI () False

data Repo = Repo {
    repoKind     :: Either RemoteRepo LocalRepo,
    repoLocalDir :: FilePath
  }
  deriving (Show,Eq)

-- ------------------------------------------------------------
-- * Build results
-- ------------------------------------------------------------

type BuildResult  = Either BuildFailure BuildSuccess
data BuildFailure = PlanningFailed
                  | DependentFailed PackageId
                  | DownloadFailed  SomeException
                  | UnpackFailed    SomeException
                  | ConfigureFailed SomeException
                  | BuildFailed     SomeException
                  | TestsFailed     SomeException
                  | InstallFailed   SomeException
data BuildSuccess = BuildOk         DocsResult TestsResult
                                    (Maybe InstalledPackageInfo)

data DocsResult  = DocsNotTried  | DocsFailed  | DocsOk
data TestsResult = TestsNotTried | TestsOk
