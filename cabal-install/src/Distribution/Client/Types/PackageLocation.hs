{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Client.Types.PackageLocation (
    PackageLocation (..),
    UnresolvedPkgLoc,
    ResolvedPkgLoc,
    UnresolvedSourcePackage,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Network.URI (URI)

import Distribution.Types.PackageId (PackageId)

import Distribution.Client.Types.Repo
import Distribution.Client.Types.SourceRepo    (SourceRepoMaybe)
import Distribution.Solver.Types.SourcePackage (SourcePackage)

type UnresolvedPkgLoc = PackageLocation (Maybe FilePath)

type ResolvedPkgLoc = PackageLocation FilePath

data PackageLocation local =

    -- | An unpacked package in the given directory.
    -- @'LocalUnpackedPackage' directory mCabalFile@, where @directory@ is an
    -- absolute filepath which points to the root of the *package*.
    -- I.e., the filepath points to the directory where the '.cabal' file of
    -- this package can be found.
    -- If @mCabalFile@ is 'Nothing', then the '.cabal' file can be assumed to
    -- be '<package-name>.cabal'.
    -- Otherwise, @mCabalFile@ points to the '.cabal' file within @directory@.
    LocalUnpackedPackage FilePath (Maybe FilePath)

    -- | A package as a tarball that's available as a local tarball
  | LocalTarballPackage FilePath

    -- | A package as a tarball from a remote URI
  | RemoteTarballPackage URI local

    -- | A package available as a tarball from a repository.
    --
    -- It may be from a local repository or from a remote repository, with a
    -- locally cached copy. ie a package available from hackage
  | RepoTarballPackage Repo PackageId local

    -- | A package available from a version control system source repository
  | RemoteSourceRepoPackage SourceRepoMaybe local
  deriving (Show, Functor, Eq, Ord, Generic, Typeable)

instance Binary local => Binary (PackageLocation local)
instance Structured local => Structured (PackageLocation local)

-- | Convenience alias for 'SourcePackage UnresolvedPkgLoc'.
type UnresolvedSourcePackage = SourcePackage UnresolvedPkgLoc
