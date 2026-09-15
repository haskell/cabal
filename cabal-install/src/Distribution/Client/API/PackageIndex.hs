-- | Tier-0 stable API: reading the local Hackage package index.
--
-- Thin wrapper over the internal @Distribution.Client.IndexUtils@ and
-- @Distribution.Client.Types.SourcePackageDb@.
module Distribution.Client.API.PackageIndex
  ( PackageIndex
  , withPackageIndex
  , lookupPackageName
  ) where

import Distribution.Client.API.Config (GlobalConfig (..))
import Distribution.Client.Config (savedGlobalFlags)
import Distribution.Client.GlobalFlags (withRepoContext)
import Distribution.Client.IndexUtils (getSourcePackages)
import Distribution.Client.Types.PackageLocation (UnresolvedSourcePackage)
import Distribution.Client.Types.SourcePackageDb (SourcePackageDb (..))
import qualified Distribution.Client.Types.SourcePackageDb as SourcePackageDb
import Distribution.Types.PackageName (PackageName)
import Distribution.Verbosity (Verbosity)

-- | All packages known from the local Hackage index
-- (the data behind @~/.cabal/packages/hackage.haskell.org@).
newtype PackageIndex = PackageIndex SourcePackageDb

-- | Open the package index configured by the user's global config
-- and run the continuation with it.
--
-- TODO: allow passing an 'IndexState' and active-repo configuration.
withPackageIndex :: Verbosity -> GlobalConfig -> (PackageIndex -> IO a) -> IO a
withPackageIndex verbosity (GlobalConfig cfg) action =
  withRepoContext verbosity (savedGlobalFlags cfg) $ \repoContext -> do
    packageDb <- getSourcePackages verbosity repoContext
    action (PackageIndex packageDb)

-- | All versions of the given package in the index, preferred versions first.
lookupPackageName :: PackageIndex -> PackageName -> [UnresolvedSourcePackage]
lookupPackageName (PackageIndex packageDb) = SourcePackageDb.lookupPackageName packageDb
