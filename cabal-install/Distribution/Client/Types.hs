{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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
module Distribution.Client.Types where

import Distribution.Package
         ( Package(..), HasMungedPackageId(..), HasUnitId(..)
         , PackageInstalled(..), newSimpleUnitId )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, installedComponentId, sourceComponentName )
import Distribution.PackageDescription
         ( FlagAssignment )
import Distribution.Version
         ( VersionRange )
import Distribution.Types.ComponentId
         ( ComponentId )
import Distribution.Types.MungedPackageId
         ( computeCompatPackageId )
import Distribution.Types.PackageId
         ( PackageId )
import Distribution.Types.AnnotatedId
import Distribution.Types.UnitId
         ( UnitId )
import Distribution.Types.PackageName
         ( PackageName )
import Distribution.Types.ComponentName
         ( ComponentName(..) )

import Distribution.Solver.Types.PackageIndex
         ( PackageIndex )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.ComponentDeps
         ( ComponentDeps )
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageFixedDeps
import Distribution.Solver.Types.SourcePackage
import Distribution.Compat.Graph (IsNode(..))
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.Semigroup
import Distribution.Simple.Utils (ordNub)
import Distribution.Text (Text(..))

import Data.Map (Map)
import Network.URI (URI(..), URIAuth(..), nullURI)
import Control.Exception
         ( Exception, SomeException )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary(..))
import qualified Text.PrettyPrint as Disp


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

-- | Within Cabal the library we no longer have a @InstalledPackageId@ type.
-- That's because it deals with the compilers' notion of a registered library,
-- and those really are libraries not packages. Those are now named units.
--
-- The package management layer does however deal with installed packages, as
-- whole packages not just as libraries. So we do still need a type for
-- installed package ids. At the moment however we track instaled packages via
-- their primary library, which is a unit id. In future this may change
-- slightly and we may distinguish these two types and have an explicit
-- conversion when we register units with the compiler.
--
type InstalledPackageId = ComponentId


-- | A 'ConfiguredPackage' is a not-yet-installed package along with the
-- total configuration information. The configuration information is total in
-- the sense that it provides all the configuration information and so the
-- final configure process will be independent of the environment.
--
-- 'ConfiguredPackage' is assumed to not support Backpack.  Only the
-- @new-build@ codepath supports Backpack.
--
data ConfiguredPackage loc = ConfiguredPackage {
       confPkgId :: InstalledPackageId,
       confPkgSource :: SourcePackage loc, -- package info, including repo
       confPkgFlags :: FlagAssignment,     -- complete flag assignment for the package
       confPkgStanzas :: [OptionalStanza], -- list of enabled optional stanzas for the package
       confPkgDeps :: ComponentDeps [ConfiguredId]
                               -- set of exact dependencies (installed or source).
                               -- These must be consistent with the 'buildDepends'
                               -- in the 'PackageDescription' that you'd get by
                               -- applying the flag assignment and optional stanzas.
    }
  deriving (Eq, Show, Generic)

-- | 'HasConfiguredId' indicates data types which have a 'ConfiguredId'.
-- This type class is mostly used to conveniently finesse between
-- 'ElaboratedPackage' and 'ElaboratedComponent'.
--
instance HasConfiguredId (ConfiguredPackage loc) where
    configuredId pkg = ConfiguredId (packageId pkg) (Just CLibName) (confPkgId pkg)

-- 'ConfiguredPackage' is the legacy codepath, we are guaranteed
-- to never have a nontrivial 'UnitId'
instance PackageFixedDeps (ConfiguredPackage loc) where
    depends = fmap (map (newSimpleUnitId . confInstId)) . confPkgDeps

instance IsNode (ConfiguredPackage loc) where
    type Key (ConfiguredPackage loc) = UnitId
    nodeKey       = newSimpleUnitId . confPkgId
    -- TODO: if we update ConfiguredPackage to support order-only
    -- dependencies, need to include those here.
    -- NB: have to deduplicate, otherwise the planner gets confused
    nodeNeighbors = ordNub . CD.flatDeps . depends

instance (Binary loc) => Binary (ConfiguredPackage loc)


-- | A ConfiguredId is a package ID for a configured package.
--
-- Once we configure a source package we know it's UnitId. It is still
-- however useful in lots of places to also know the source ID for the package.
-- We therefore bundle the two.
--
-- An already installed package of course is also "configured" (all it's
-- configuration parameters and dependencies have been specified).
data ConfiguredId = ConfiguredId {
    confSrcId  :: PackageId
  , confCompName :: Maybe ComponentName
  , confInstId :: ComponentId
  }
  deriving (Eq, Ord, Generic)

annotatedIdToConfiguredId :: AnnotatedId ComponentId -> ConfiguredId
annotatedIdToConfiguredId aid = ConfiguredId {
        confSrcId    = ann_pid aid,
        confCompName = Just (ann_cname aid),
        confInstId   = ann_id aid
    }

instance Binary ConfiguredId

instance Show ConfiguredId where
  show cid = show (confInstId cid)

instance Package ConfiguredId where
  packageId = confSrcId

instance Package (ConfiguredPackage loc) where
  packageId cpkg = packageId (confPkgSource cpkg)

instance HasMungedPackageId (ConfiguredPackage loc) where
  mungedId cpkg = computeCompatPackageId (packageId cpkg) Nothing

-- Never has nontrivial UnitId
instance HasUnitId (ConfiguredPackage loc) where
  installedUnitId = newSimpleUnitId . confPkgId

instance PackageInstalled (ConfiguredPackage loc) where
  installedDepends = CD.flatDeps . depends

class HasConfiguredId a where
    configuredId :: a -> ConfiguredId

-- NB: This instance is slightly dangerous, in that you'll lose
-- information about the specific UnitId you depended on.
instance HasConfiguredId InstalledPackageInfo where
    configuredId ipkg = ConfiguredId (packageId ipkg)
                            (Just (sourceComponentName ipkg))
                            (installedComponentId ipkg)

-- | Like 'ConfiguredPackage', but with all dependencies guaranteed to be
-- installed already, hence itself ready to be installed.
newtype GenericReadyPackage srcpkg = ReadyPackage srcpkg -- see 'ConfiguredPackage'.
  deriving (Eq, Show, Generic, Package, PackageFixedDeps,
            HasMungedPackageId, HasUnitId, PackageInstalled, Binary)

-- Can't newtype derive this
instance IsNode srcpkg => IsNode (GenericReadyPackage srcpkg) where
    type Key (GenericReadyPackage srcpkg) = Key srcpkg
    nodeKey (ReadyPackage spkg) = nodeKey spkg
    nodeNeighbors (ReadyPackage spkg) = nodeNeighbors spkg

type ReadyPackage = GenericReadyPackage (ConfiguredPackage UnresolvedPkgLoc)

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
  deriving (Show, Functor, Eq, Ord, Generic, Typeable)

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

-- | A summary of the outcome for building a single package.
--
type BuildOutcome = Either BuildFailure BuildResult

-- | A summary of the outcome for building a whole set of packages.
--
type BuildOutcomes = Map UnitId BuildOutcome

data BuildFailure = PlanningFailed
                  | DependentFailed PackageId
                  | DownloadFailed  SomeException
                  | UnpackFailed    SomeException
                  | ConfigureFailed SomeException
                  | BuildFailed     SomeException
                  | TestsFailed     SomeException
                  | InstallFailed   SomeException
  deriving (Show, Typeable, Generic)

instance Exception BuildFailure

-- Note that the @Maybe InstalledPackageInfo@ is a slight hack: we only
-- the public library's 'InstalledPackageInfo' is stored here, even if
-- there were 'InstalledPackageInfo' from internal libraries.  This
-- 'InstalledPackageInfo' is not used anyway, so it makes no difference.
data BuildResult = BuildResult DocsResult TestsResult
                               (Maybe InstalledPackageInfo)
  deriving (Show, Generic)

data DocsResult  = DocsNotTried  | DocsFailed  | DocsOk
  deriving (Show, Generic, Typeable)
data TestsResult = TestsNotTried | TestsOk
  deriving (Show, Generic, Typeable)

instance Binary BuildFailure
instance Binary BuildResult
instance Binary DocsResult
instance Binary TestsResult

--FIXME: this is a total cheat
instance Binary SomeException where
  put _ = return ()
  get = fail "cannot serialise exceptions"


-- ------------------------------------------------------------
-- * --allow-newer/--allow-older
-- ------------------------------------------------------------

-- TODO: When https://github.com/haskell/cabal/issues/4203 gets tackled,
-- it may make sense to move these definitions to the Solver.Types
-- module

-- | 'RelaxDeps' in the context of upper bounds (i.e. for @--allow-newer@ flag)
newtype AllowNewer = AllowNewer { unAllowNewer :: RelaxDeps }
                   deriving (Eq, Read, Show, Generic)

-- | 'RelaxDeps' in the context of lower bounds (i.e. for @--allow-older@ flag)
newtype AllowOlder = AllowOlder { unAllowOlder :: RelaxDeps }
                   deriving (Eq, Read, Show, Generic)

-- | Generic data type for policy when relaxing bounds in dependencies.
-- Don't use this directly: use 'AllowOlder' or 'AllowNewer' depending
-- on whether or not you are relaxing an lower or upper bound
-- (respectively).
data RelaxDeps =

  -- | Default: honor the bounds in all dependencies, never choose
  -- versions newer than allowed.
  RelaxDepsNone

  -- | Ignore upper bounds in dependencies on the given packages.
  | RelaxDepsSome [RelaxedDep]

  -- | Ignore upper bounds in dependencies on all packages.
  | RelaxDepsAll
  deriving (Eq, Read, Show, Generic)

-- | Dependencies can be relaxed either for all packages in the install plan, or
-- only for some packages.
data RelaxedDep = RelaxedDep PackageName
                | RelaxedDepScoped PackageName PackageName
                deriving (Eq, Read, Show, Generic)

instance Text RelaxedDep where
  disp (RelaxedDep p0)          = disp p0
  disp (RelaxedDepScoped p0 p1) = disp p0 Disp.<> Disp.colon Disp.<> disp p1

  parse = scopedP Parse.<++ normalP
    where
      scopedP = RelaxedDepScoped `fmap` parse <* Parse.char ':' <*> parse
      normalP = RelaxedDep       `fmap` parse

instance Binary RelaxDeps
instance Binary RelaxedDep
instance Binary AllowNewer
instance Binary AllowOlder

instance Semigroup RelaxDeps where
  RelaxDepsNone       <> r                   = r
  l@RelaxDepsAll      <> _                   = l
  l@(RelaxDepsSome _) <> RelaxDepsNone       = l
  (RelaxDepsSome   _) <> r@RelaxDepsAll      = r
  (RelaxDepsSome   a) <> (RelaxDepsSome b)   = RelaxDepsSome (a ++ b)

instance Monoid RelaxDeps where
  mempty  = RelaxDepsNone
  mappend = (<>)

instance Semigroup AllowNewer where
  AllowNewer x <> AllowNewer y = AllowNewer (x <> y)

instance Semigroup AllowOlder where
  AllowOlder x <> AllowOlder y = AllowOlder (x <> y)

instance Monoid AllowNewer where
  mempty  = AllowNewer mempty
  mappend = (<>)

instance Monoid AllowOlder where
  mempty  = AllowOlder mempty
  mappend = (<>)
