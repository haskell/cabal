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
         ( PackageName, PackageId, Package(..)
         , mkPackageKey, PackageKey, InstalledPackageId(..)
         , HasInstalledPackageId(..), PackageInstalled(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.PackageDescription
         ( Benchmark(..), GenericPackageDescription(..), FlagAssignment
         , TestSuite(..) )
import Distribution.PackageDescription.Configuration
         ( mapTreeData )
import Distribution.Client.PackageIndex
         ( PackageIndex, PackageFixedDeps(..) )
import Distribution.Client.ComponentDeps
         ( ComponentDeps )
import qualified Distribution.Client.ComponentDeps as CD
import Distribution.Version
         ( VersionRange )
import Distribution.Simple.Compiler
         ( Compiler, packageKeySupported )
import Distribution.Text (display)
import qualified Distribution.InstalledPackageInfo as Info

import Data.Map (Map)
import Network.URI (URI)
import Data.ByteString.Lazy (ByteString)
import Control.Exception
         ( SomeException )

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }

-- | This is the information we get from a @00-index.tar.gz@ hackage index.
--
data SourcePackageDb = SourcePackageDb {
  packageIndex       :: PackageIndex SourcePackage,
  packagePreferences :: Map PackageName VersionRange
}

-- ------------------------------------------------------------
-- * Various kinds of information about packages
-- ------------------------------------------------------------

-- | InstalledPackage caches its dependencies as source package IDs.
-- This is for the benefit of the top-down solver only.
data InstalledPackage = InstalledPackage
       InstalledPackageInfo
       [PackageId]

instance Package InstalledPackage where
  packageId (InstalledPackage pkg _) = packageId pkg
instance PackageFixedDeps InstalledPackage where
  depends (InstalledPackage pkg _) = depends pkg
instance HasInstalledPackageId InstalledPackage where
  installedPackageId (InstalledPackage pkg _) = installedPackageId pkg
instance PackageInstalled InstalledPackage where
  installedDepends (InstalledPackage pkg _) = installedDepends pkg


-- | In order to reuse the implementation of PackageIndex which relies on
-- 'InstalledPackageId', we need to be able to synthesize these IDs prior
-- to installation.  Eventually, we'll move to a representation of
-- 'InstalledPackageId' which can be properly computed before compilation
-- (of course, it's a bit of a misnomer since the packages are not actually
-- installed yet.)  In any case, we'll synthesize temporary installed package
-- IDs to use as keys during install planning.  These should never be written
-- out!  Additionally, they need to be guaranteed unique within the install
-- plan.
fakeInstalledPackageId :: PackageId -> InstalledPackageId
fakeInstalledPackageId = InstalledPackageId . (".fake."++) . display

-- | A 'ConfiguredPackage' is a not-yet-installed package along with the
-- total configuration information. The configuration information is total in
-- the sense that it provides all the configuration information and so the
-- final configure process will be independent of the environment.
--
data ConfiguredPackage = ConfiguredPackage
       SourcePackage       -- package info, including repo
       FlagAssignment      -- complete flag assignment for the package
       [OptionalStanza]    -- list of enabled optional stanzas for the package
       (ComponentDeps [ConfiguredId])
                           -- set of exact dependencies (installed or source).
                           -- These must be consistent with the 'buildDepends'
                           -- in the 'PackageDescription' that you'd get by
                           -- applying the flag assignment and optional stanzas.
  deriving Show

-- | A ConfiguredId is a package ID for a configured package.
--
-- Once we configure a source package we know it's InstalledPackageId
-- (at least, in principle, even if we have to fake it currently). It is still
-- however useful in lots of places to also know the source ID for the package.
-- We therefore bundle the two.
--
-- An already installed package of course is also "configured" (all it's
-- configuration parameters and dependencies have been specified).
--
-- TODO: I wonder if it would make sense to promote this datatype to Cabal
-- and use it consistently instead of InstalledPackageIds?
data ConfiguredId = ConfiguredId {
    confSrcId  :: PackageId
  , confInstId :: InstalledPackageId
  }

instance Show ConfiguredId where
  show = show . confSrcId

instance Package ConfiguredPackage where
  packageId (ConfiguredPackage pkg _ _ _) = packageId pkg

instance PackageFixedDeps ConfiguredPackage where
  depends (ConfiguredPackage _ _ _ deps) = fmap (map confInstId) deps

instance HasInstalledPackageId ConfiguredPackage where
  installedPackageId = fakeInstalledPackageId . packageId

-- | Like 'ConfiguredPackage', but with all dependencies guaranteed to be
-- installed already, hence itself ready to be installed.
data ReadyPackage = ReadyPackage
       SourcePackage                           -- see 'ConfiguredPackage'.
       FlagAssignment                          --
       [OptionalStanza]                        --
       (ComponentDeps [InstalledPackageInfo])  -- Installed dependencies.
  deriving Show

instance Package ReadyPackage where
  packageId (ReadyPackage pkg _ _ _) = packageId pkg

instance PackageFixedDeps ReadyPackage where
  depends (ReadyPackage _ _ _ deps) = fmap (map installedPackageId) deps

instance HasInstalledPackageId ReadyPackage where
  installedPackageId = fakeInstalledPackageId . packageId


-- | Extracts a package key from ReadyPackage, a common operation needed
-- to calculate build paths.
readyPackageKey :: Compiler -> ReadyPackage -> PackageKey
readyPackageKey comp (ReadyPackage pkg _ _ deps) =
    mkPackageKey (packageKeySupported comp) (packageId pkg)
                 (map Info.packageKey (CD.nonSetupDeps deps)) []


-- | Sometimes we need to convert a 'ReadyPackage' back to a
-- 'ConfiguredPackage'. For example, a failed 'PlanPackage' can be *either*
-- Ready or Configured.
readyPackageToConfiguredPackage :: ReadyPackage -> ConfiguredPackage
readyPackageToConfiguredPackage (ReadyPackage srcpkg flags stanzas deps) =
    ConfiguredPackage srcpkg flags stanzas (fmap (map aux) deps)
  where
    aux :: InstalledPackageInfo -> ConfiguredId
    aux info = ConfiguredId {
        confSrcId  = Info.sourcePackageId info
      , confInstId = installedPackageId info
      }


-- | A package description along with the location of the package sources.
--
data SourcePackage = SourcePackage {
    packageInfoId        :: PackageId,
    packageDescription   :: GenericPackageDescription,
    packageSource        :: PackageLocation (Maybe FilePath),
    packageDescrOverride :: PackageDescriptionOverride
  }
  deriving Show

-- | We sometimes need to override the .cabal file in the tarball with
-- the newer one from the package index.
type PackageDescriptionOverride = Maybe ByteString

instance Package SourcePackage where packageId = packageInfoId

data OptionalStanza
    = TestStanzas
    | BenchStanzas
  deriving (Eq, Ord, Show)

enableStanzas
    :: [OptionalStanza]
    -> GenericPackageDescription
    -> GenericPackageDescription
enableStanzas stanzas gpkg = gpkg
    { condBenchmarks = flagBenchmarks $ condBenchmarks gpkg
    , condTestSuites = flagTests $ condTestSuites gpkg
    }
  where
    enableTest t = t { testEnabled = TestStanzas `elem` stanzas }
    enableBenchmark bm = bm { benchmarkEnabled = BenchStanzas `elem` stanzas }
    flagBenchmarks = map (\(n, bm) -> (n, mapTreeData enableBenchmark bm))
    flagTests = map (\(n, t) -> (n, mapTreeData enableTest t))

-- ------------------------------------------------------------
-- * Package locations and repositories
-- ------------------------------------------------------------

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
      remoteRepoRootKeys :: ()
    }

  -- FIXME: discuss this type some more.

  deriving (Show,Eq,Ord)

-- | Construct a partial 'RemoteRepo' value to fold the field parser list over.
emptyRemoteRepo :: String -> RemoteRepo
emptyRemoteRepo name = RemoteRepo name (error "RemoteRepo: empty URI!") ()

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
