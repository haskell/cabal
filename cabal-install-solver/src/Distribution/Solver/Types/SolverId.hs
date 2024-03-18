{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.SolverId
    ( SolverId(..)
    )

where

import Distribution.Solver.Compat.Prelude
import Distribution.Solver.Types.PackageConstraint
import Prelude ()

import Distribution.Package (PackageId, Package(..), UnitId)

-- | The solver can produce references to existing packages or
-- packages we plan to install.  Unlike 'ConfiguredId' we don't
-- yet know the 'UnitId' for planned packages, because it's
-- not the solver's job to compute them.
--
-- We keep the constraint scope as a solved package must also be identified by
-- the scope it was resolved in (in particular to generate proper freeze files)
data SolverId = PreExistingId { solverSrcId :: PackageId, solverInstId :: UnitId, solverScope :: ConstraintScope }
              | PlannedId     { solverSrcId :: PackageId, solverScope :: ConstraintScope }
  deriving (Eq, Ord, Generic)

instance Binary SolverId
instance Structured SolverId

instance Show SolverId where
    show = show . solverSrcId

instance Package SolverId where
  packageId = solverSrcId
