{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.ResolverPackage
    ( ResolverPackage(..)
    , resolverPackageDeps
    ) where

import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.SolverPackage
import qualified Distribution.Solver.Types.ComponentDeps as CD

import Distribution.Compat.Binary (Binary(..))
import Distribution.Compat.Graph (IsNode(..))
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Package (Package(..), HasUnitId(..))
import GHC.Generics (Generic)

-- | The dependency resolver picks either pre-existing installed packages
-- or it picks source packages along with package configuration.
--
-- This is like the 'InstallPlan.PlanPackage' but with fewer cases.
--
data ResolverPackage loc = PreExisting InstalledPackageInfo (CD.ComponentDeps [SolverId])
                         | Configured  (SolverPackage loc)
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (ResolverPackage loc)

instance Package (ResolverPackage loc) where
  packageId (PreExisting ipkg _)   = packageId ipkg
  packageId (Configured  spkg)     = packageId spkg

resolverPackageDeps :: ResolverPackage loc -> CD.ComponentDeps [SolverId]
resolverPackageDeps (PreExisting _ deps) = deps
resolverPackageDeps (Configured spkg) = solverPkgDeps spkg

instance IsNode (ResolverPackage loc) where
  type Key (ResolverPackage loc) = SolverId
  nodeKey (PreExisting ipkg _) = PreExistingId (packageId ipkg) (installedUnitId ipkg)
  nodeKey (Configured spkg) = PlannedId (packageId spkg)
  -- Use dependencies for ALL components
  nodeNeighbors pkg = CD.flatDeps (resolverPackageDeps pkg)
