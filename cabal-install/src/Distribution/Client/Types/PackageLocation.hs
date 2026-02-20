{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.PackageLocation
  ( PackageLocation (..)
  , UnresolvedPkgLoc
  , ResolvedPkgLoc
  , UnresolvedSourcePackage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Network.URI (URI)

import Distribution.Types.PackageId (PackageId)

import Distribution.Client.Types.Repo
import Distribution.Client.Types.SourceRepo (SourceRepoMaybe, SourceRepositoryPackage (..))
import Distribution.Pretty
import Distribution.Solver.Types.SourcePackage (SourcePackage)

type UnresolvedPkgLoc = PackageLocation (Maybe FilePath)

type ResolvedPkgLoc = PackageLocation FilePath

data PackageLocation local
  = -- | An unpacked package in the given dir, or current dir
    LocalUnpackedPackage FilePath
  | -- | A package as a tarball that's available as a local tarball
    LocalTarballPackage FilePath
  | -- | A package as a tarball from a remote URI
    RemoteTarballPackage URI local
  | -- | A package available as a tarball from a repository.
    --
    -- It may be from a local repository or from a remote repository, with a
    -- locally cached copy. ie a package available from hackage
    RepoTarballPackage Repo PackageId local
  | -- | A package available from a version control system source repository
    RemoteSourceRepoPackage SourceRepoMaybe local
  deriving (Show, Functor, Eq, Ord, Generic)

instance Pretty (PackageLocation local) where
  pretty (LocalUnpackedPackage fp) = showFilePath fp
  pretty (LocalTarballPackage fp) = showFilePath fp
  pretty (RemoteTarballPackage uri _) = showToken $ show uri
  pretty (RepoTarballPackage repo pid _) = pretty pid <> showToken "@" <> pretty (repoName repo)
  pretty (RemoteSourceRepoPackage sourceRepo _) =
    pretty (srpType sourceRepo) <+> showToken (srpLocation sourceRepo)

instance Binary local => Binary (PackageLocation local)
instance Structured local => Structured (PackageLocation local)

-- | Convenience alias for 'SourcePackage UnresolvedPkgLoc'.
type UnresolvedSourcePackage = SourcePackage UnresolvedPkgLoc
