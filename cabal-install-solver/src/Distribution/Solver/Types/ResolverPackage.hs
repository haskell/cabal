{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.ResolverPackage
    ( ResolverPackage(..)
    , resolverPackageLibDeps
    , resolverPackageExeDeps
    , resolverPackageConstraintScope
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Solver.Types.InstSolverPackage
import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.ComponentDeps as CD

import Distribution.Compat.Graph (IsNode(..))
import Distribution.Package (Package(..), HasUnitId(..))
import Distribution.Simple.Utils (ordNub)

-- | The dependency resolver picks either pre-existing installed packages
-- or it picks source packages along with package configuration.
--
-- This is like the 'InstallPlan.PlanPackage' but with fewer cases.
--
-- The constraint scope field refers to the scope in which this package was resolved.
data ResolverPackage loc = PreExisting InstSolverPackage ConstraintScope
                         | Configured  (SolverPackage loc) ConstraintScope
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (ResolverPackage loc)
instance Structured loc => Structured (ResolverPackage loc)

instance Package (ResolverPackage loc) where
  packageId (PreExisting ipkg _)     = packageId ipkg
  packageId (Configured  spkg _)     = packageId spkg

resolverPackageLibDeps :: ResolverPackage loc -> CD.ComponentDeps [SolverId]
resolverPackageLibDeps (PreExisting ipkg _) = instSolverPkgLibDeps ipkg
resolverPackageLibDeps (Configured spkg _) = solverPkgLibDeps spkg

resolverPackageExeDeps :: ResolverPackage loc -> CD.ComponentDeps [SolverId]
resolverPackageExeDeps (PreExisting ipkg _) = instSolverPkgExeDeps ipkg
resolverPackageExeDeps (Configured spkg _) = solverPkgExeDeps spkg

resolverPackageConstraintScope :: ResolverPackage oc -> ConstraintScope
resolverPackageConstraintScope (PreExisting _ cs) = cs
resolverPackageConstraintScope (Configured _ cs) = cs

instance IsNode (ResolverPackage loc) where
  type Key (ResolverPackage loc) = SolverId
  nodeKey (PreExisting ipkg _) = PreExistingId (packageId ipkg) (installedUnitId ipkg)
  nodeKey (Configured spkg _) = PlannedId (packageId spkg)
  -- Use dependencies for ALL components
  nodeNeighbors pkg =
    ordNub $ CD.flatDeps (resolverPackageLibDeps pkg) ++
             CD.flatDeps (resolverPackageExeDeps pkg)
             
