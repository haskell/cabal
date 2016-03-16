{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
module Distribution.Client.Types
    ( PackageFixedDeps(..)
    , ReadyPackage
    , ConfiguredPackage(..)
    , ConfiguredId(..)
    , SourcePackage(..)
    , SourcePackageDb(..)
    , GenericReadyPackage(..)
    , UnresolvedSourcePackage
    , UnresolvedPkgLoc
    , ResolvedPkgLoc
    , PackageLocation(..)
    , PackageDescriptionOverride
    , enableStanzas
    , maybeRepoRemote
    , emptyRemoteRepo
    , fakeUnitId
    , OptionalStanza(..)
    , Repo(..)
    , Username(..)
    , Password(..)
    , RemoteRepo(..)
    , BuildResult
    , BuildFailure(..)
    , BuildSuccess(..)
    , DocsResult(..)
    , TestsResult(..)
    ) where

import Distribution.Package
         ( PackageName, PackageId, Package(..)
         , UnitId(..), HasUnitId(..), PackageInstalled(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Solver.PackageIndex
         ( PackageIndex )
import Distribution.Solver.ComponentDeps
         ( ComponentDeps )
import qualified Distribution.Solver.ComponentDeps as CD
import Distribution.Solver.Types
import Distribution.Version
         ( VersionRange )

import Data.Map (Map)
import Network.URI (URI(..), URIAuth(..), nullURI)
import Control.Exception
         ( SomeException )
import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary(..))


newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }

-- | This is the information we get from a @00-index.tar.gz@ hackage index.
--
data SourcePackageDb = SourcePackageDb {
  packageIndex       :: PackageIndex UnresolvedSourcePackage,
  packagePreferences :: Map PackageName VersionRange
}
  deriving (Eq, Generic)

instance Binary SourcePackageDb

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
  depends :: pkg -> ComponentDeps [UnitId]

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
  deriving (Eq, Show, Generic)

type ReadyPackage = GenericReadyPackage (ConfiguredPackage UnresolvedPkgLoc) InstalledPackageInfo

instance Package srcpkg => Package (GenericReadyPackage srcpkg ipkg) where
  packageId (ReadyPackage srcpkg _deps) = packageId srcpkg

instance (Package srcpkg, HasUnitId ipkg) =>
         PackageFixedDeps (GenericReadyPackage srcpkg ipkg) where
  depends (ReadyPackage _ deps) = fmap (map installedUnitId) deps

instance HasUnitId srcpkg =>
         HasUnitId (GenericReadyPackage srcpkg ipkg) where
  installedUnitId (ReadyPackage pkg _) = installedUnitId pkg

instance (Binary srcpkg, Binary ipkg) => Binary (GenericReadyPackage srcpkg ipkg)


-- | Convenience alias for 'SourcePackage UnresolvedPkgLoc'.
type UnresolvedSourcePackage = SourcePackage UnresolvedPkgLoc

-- ------------------------------------------------------------
-- * Package locations and repositories
-- ------------------------------------------------------------

type UnresolvedPkgLoc = PackageLocation (Maybe FilePath)

type ResolvedPkgLoc = PackageLocation FilePath

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
  deriving (Show, Functor, Eq, Ord, Generic)

instance Binary local => Binary (PackageLocation local)

-- note, network-uri-2.6.0.3+ provide a Generic instance but earlier
-- versions do not, so we use manual Binary instances here
instance Binary URI where
  put (URI a b c d e) = do put a; put b; put c; put d; put e
  get = do !a <- get; !b <- get; !c <- get; !d <- get; !e <- get
           return (URI a b c d e)

instance Binary URIAuth where
  put (URIAuth a b c) = do put a; put b; put c
  get = do !a <- get; !b <- get; !c <- get; return (URIAuth a b c)

data RemoteRepo =
    RemoteRepo {
      remoteRepoName     :: String,
      remoteRepoURI      :: URI,

      -- | Enable secure access?
      --
      -- 'Nothing' here represents "whatever the default is"; this is important
      -- to allow for a smooth transition from opt-in to opt-out security
      -- (once we switch to opt-out, all access to the central Hackage
      -- repository should be secure by default)
      remoteRepoSecure :: Maybe Bool,

      -- | Root key IDs (for bootstrapping)
      remoteRepoRootKeys :: [String],

      -- | Threshold for verification during bootstrapping
      remoteRepoKeyThreshold :: Int,

      -- | Normally a repo just specifies an HTTP or HTTPS URI, but as a
      -- special case we may know a repo supports both and want to try HTTPS
      -- if we can, but still allow falling back to HTTP.
      --
      -- This field is not currently stored in the config file, but is filled
      -- in automagically for known repos.
      remoteRepoShouldTryHttps :: Bool
    }

  deriving (Show, Eq, Ord, Generic)

instance Binary RemoteRepo

-- | Construct a partial 'RemoteRepo' value to fold the field parser list over.
emptyRemoteRepo :: String -> RemoteRepo
emptyRemoteRepo name = RemoteRepo name nullURI Nothing [] 0 False

-- | Different kinds of repositories
--
-- NOTE: It is important that this type remains serializable.
data Repo =
    -- | Local repositories
    RepoLocal {
        repoLocalDir :: FilePath
      }

    -- | Standard (unsecured) remote repositores
  | RepoRemote {
        repoRemote   :: RemoteRepo
      , repoLocalDir :: FilePath
      }

    -- | Secure repositories
    --
    -- Although this contains the same fields as 'RepoRemote', we use a separate
    -- constructor to avoid confusing the two.
    --
    -- Not all access to a secure repo goes through the hackage-security
    -- library currently; code paths that do not still make use of the
    -- 'repoRemote' and 'repoLocalDir' fields directly.
  | RepoSecure {
        repoRemote   :: RemoteRepo
      , repoLocalDir :: FilePath
      }
  deriving (Show, Eq, Ord, Generic)

instance Binary Repo

-- | Check if this is a remote repo
maybeRepoRemote :: Repo -> Maybe RemoteRepo
maybeRepoRemote (RepoLocal    _localDir) = Nothing
maybeRepoRemote (RepoRemote r _localDir) = Just r
maybeRepoRemote (RepoSecure r _localDir) = Just r

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
  deriving (Show, Generic)
data BuildSuccess = BuildOk         DocsResult TestsResult
                                    (Maybe InstalledPackageInfo)
  deriving (Show, Generic)

data DocsResult  = DocsNotTried  | DocsFailed  | DocsOk
  deriving (Show, Generic)
data TestsResult = TestsNotTried | TestsOk
  deriving (Show, Generic)

instance Binary BuildFailure
instance Binary BuildSuccess
instance Binary DocsResult
instance Binary TestsResult

--FIXME: this is a total cheat
instance Binary SomeException where
  put _ = return ()
  get = fail "cannot serialise exceptions"
