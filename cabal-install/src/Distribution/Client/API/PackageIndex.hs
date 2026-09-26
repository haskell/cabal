-- | Tier-0 stable API: reading the local Hackage package index.
--
-- Thin wrapper over the internal @Distribution.Client.IndexUtils@ and
-- @Distribution.Client.Types.SourcePackageDb@.
module Distribution.Client.API.PackageIndex
  ( PackageIndex -- Note: cabal-matrix
  , withPackageIndex -- Note: cabal-matrix
  , lookupPackageName -- Note: cabal-matrix
  ) where

import Distribution.Client.API.Config (GlobalConfig (..))
import Distribution.Client.Config (savedGlobalFlags)
import Distribution.Client.GlobalFlags (withRepoContext)
import Distribution.Client.IndexUtils (getSourcePackages)
import Distribution.Client.Types.SourcePackageDb (SourcePackageDb (..))
import qualified Distribution.Client.Types.SourcePackageDb as SourcePackageDb
import Distribution.Package (Package (..), PackageId)
import Distribution.Types.PackageName (PackageName)
import Distribution.Verbosity (Verbosity)

-- | All packages known from the local Hackage index
-- (the data behind @~/.cabal/packages/hackage.haskell.org@).
data PackageIndex = PackageIndex
  { packageIndexSourceDb :: SourcePackageDb
  -- ^ The internal package database. Not part of the public API.
  }

-- | Open the package index configured by the user's global config
-- and run the continuation with it.
withPackageIndex :: Verbosity -> GlobalConfig -> (PackageIndex -> IO a) -> IO a
withPackageIndex verbosity cfg action =
  withRepoContext verbosity (savedGlobalFlags (globalSavedConfig cfg)) \repoContext -> do
    packageDb <- getSourcePackages verbosity repoContext
    action PackageIndex{packageIndexSourceDb = packageDb}

-- | All versions of the given package in the index, preferred versions first.
lookupPackageName :: PackageIndex -> PackageName -> [PackageId]
lookupPackageName PackageIndex{packageIndexSourceDb = db} =
  map packageId . SourcePackageDb.lookupPackageName db
