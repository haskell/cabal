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
         ( PackageName, PackageId, Package(..), PackageFixedDeps(..)
         , mkPackageKey, PackageKey, InstalledPackageId(..)
         , PackageInstalled(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, packageKey )
import Distribution.PackageDescription
         ( Benchmark(..), GenericPackageDescription(..), FlagAssignment
         , TestSuite(..) )
import Distribution.PackageDescription.Configuration
         ( mapTreeData )
import Distribution.Client.PackageIndex
         ( PackageIndex )
import Distribution.Version
         ( VersionRange )
import Distribution.Simple.Compiler
         ( Compiler, packageKeySupported )
import Distribution.Text (display)

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

-- | TODO: This is a hack to help us transition from Cabal-1.6 to 1.8.
-- What is new in 1.8 is that installed packages and dependencies between
-- installed packages are now identified by an opaque InstalledPackageId
-- rather than a source PackageId.
--
-- We should use simply an 'InstalledPackageInfo' here but to ease the
-- transition we are temporarily using this variant where we pretend that
-- installed packages still specify their deps in terms of PackageIds.
--
-- Crucially this means that 'InstalledPackage' can be an instance of
-- 'PackageFixedDeps' where as 'InstalledPackageInfo' is no longer an instance
-- of that class. This means we can make 'PackageIndex'es of InstalledPackage
-- where as the InstalledPackageInfo now has its own monomorphic index type.
--
data InstalledPackage = InstalledPackage
       InstalledPackageInfo
       [PackageId]

instance Package InstalledPackage where
  packageId (InstalledPackage pkg _) = packageId pkg
instance PackageFixedDeps InstalledPackage where
  depends (InstalledPackage _ deps) = deps
instance PackageInstalled InstalledPackage where
  installedPackageId (InstalledPackage pkg _) = installedPackageId pkg
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
       [PackageId]         -- set of exact dependencies. These must be
                           -- consistent with the 'buildDepends' in the
                           -- 'PackageDescription' that you'd get by applying
                           -- the flag assignment and optional stanzas.
  deriving Show

instance Package ConfiguredPackage where
  packageId (ConfiguredPackage pkg _ _ _) = packageId pkg

instance PackageFixedDeps ConfiguredPackage where
  depends (ConfiguredPackage _ _ _ deps) = deps

instance PackageInstalled ConfiguredPackage where
  installedPackageId = fakeInstalledPackageId . packageId
  installedDepends = map fakeInstalledPackageId . depends

-- | Like 'ConfiguredPackage', but with all dependencies guaranteed to be
-- installed already, hence itself ready to be installed.
data ReadyPackage = ReadyPackage
       SourcePackage           -- see 'ConfiguredPackage'.
       FlagAssignment          --
       [OptionalStanza]        --
       [InstalledPackageInfo]  -- Installed dependencies.
  deriving Show

instance Package ReadyPackage where
  packageId (ReadyPackage pkg _ _ _) = packageId pkg

instance PackageFixedDeps ReadyPackage where
  depends (ReadyPackage _ _ _ deps) = map packageId deps

instance PackageInstalled ReadyPackage where
  installedPackageId = fakeInstalledPackageId . packageId
  installedDepends (ReadyPackage _ _ _ ipis) = map installedPackageId ipis

-- | Extracts a package key from ReadyPackage, a common operation needed
-- to calculate build paths.
readyPackageKey :: Compiler -> ReadyPackage -> PackageKey
readyPackageKey comp (ReadyPackage pkg _ _ deps) =
    mkPackageKey (packageKeySupported comp) (packageId pkg)
                 (map packageKey deps) []


-- | Sometimes we need to convert a 'ReadyPackage' back to a
-- 'ConfiguredPackage'. For example, a failed 'PlanPackage' can be *either*
-- Ready or Configured.
readyPackageToConfiguredPackage :: ReadyPackage -> ConfiguredPackage
readyPackageToConfiguredPackage (ReadyPackage srcpkg flags stanzas deps) =
  ConfiguredPackage srcpkg flags stanzas (map packageId deps)

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

data RemoteRepo = RemoteRepo {
    remoteRepoName :: String,
    remoteRepoURI  :: URI
  }
  deriving (Show,Eq,Ord)

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
