{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.ResolverPackage
    ( ResolverPackage(..)
    ) where

import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.PackageFixedDeps
import qualified Distribution.Solver.Types.ComponentDeps as CD

import Distribution.Compat.Binary (Binary(..))
import Distribution.Compat.Graph (IsNode(..))
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Package (UnitId, Package(..), HasUnitId(..))
import GHC.Generics (Generic)

-- | The dependency resolver picks either pre-existing installed packages
-- or it picks source packages along with package configuration.
--
-- This is like the 'InstallPlan.PlanPackage' but with fewer cases.
--
data ResolverPackage loc = PreExisting InstalledPackageInfo
                         | Configured  (SolverPackage loc)
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (ResolverPackage loc)

instance Package (ResolverPackage loc) where
  packageId (PreExisting ipkg)     = packageId ipkg
  packageId (Configured  spkg)     = packageId spkg

instance PackageFixedDeps (ResolverPackage loc) where
  depends (PreExisting pkg)     = depends pkg
  depends (Configured  pkg)     = depends pkg

instance HasUnitId (ResolverPackage loc) where
  installedUnitId (PreExisting ipkg ) = installedUnitId ipkg
  installedUnitId (Configured  spkg)  = installedUnitId spkg

instance IsNode (ResolverPackage loc) where
  type Key (ResolverPackage loc) = UnitId -- TODO: change me
  nodeKey = installedUnitId
  -- Use dependencies for ALL components
  nodeNeighbors = CD.flatDeps . depends
