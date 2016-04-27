{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.SolverPackage
    ( SolverPackage(..)
    ) where

import Distribution.Compat.Binary (Binary(..))
import Distribution.Package ( Package(..), HasUnitId(..) )
import Distribution.PackageDescription ( FlagAssignment )
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import Distribution.Solver.Types.Internal.Utils ( unsafeInternalFakeUnitId )
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageFixedDeps
import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.SourcePackage
import GHC.Generics (Generic)

-- | A 'SolverPackage' is a package specified by the dependency solver.
-- It will get elaborated into a 'ConfiguredPackage' or even an
-- 'ElaboratedConfiguredPackage'.
--
-- NB: 'SolverPackage's are essentially always with 'UnresolvedPkgLoc',
-- but for symmetry we have the parameter.  (Maybe it can be removed.)
--
data SolverPackage loc = SolverPackage {
        solverPkgSource :: SourcePackage loc,
        solverPkgFlags :: FlagAssignment,
        solverPkgStanzas :: [OptionalStanza],
        solverPkgDeps :: ComponentDeps [SolverId]
    }
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (SolverPackage loc)

instance Package (SolverPackage loc) where
  packageId = packageId . solverPkgSource

-- | This is a minor hack as 'PackageIndex' assumes keys are
-- 'UnitId's but prior to computing 'UnitId's (i.e., immediately
-- after running the solver, we don't have this information.)
-- But this is strictly temporary: once we convert to a
-- 'ConfiguredPackage' we'll record 'UnitId's for everything.
instance HasUnitId (SolverPackage loc) where
  installedUnitId = unsafeInternalFakeUnitId . packageId . solverPkgSource

instance PackageFixedDeps (SolverPackage loc) where
  depends pkg = fmap (map installedUnitId) (solverPkgDeps pkg)
