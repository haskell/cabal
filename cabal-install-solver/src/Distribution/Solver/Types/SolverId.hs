module Distribution.Solver.Types.SolverId
    ( SolverId(..)
    )

where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package (PackageId, Package(..), UnitId)
import Distribution.Solver.Types.Machine (Machine)

-- | The solver can produce references to existing packages or
-- packages we plan to install.  Unlike 'ConfiguredId' we don't
-- yet know the 'UnitId' for planned packages, because it's
-- not the solver's job to compute them.
--
-- Each reference also carries the 'Machine' of the package it points at:
-- under cross-compilation the same package may be built for both the build
-- and host machines, so the machine is part of a package's identity.
--
data SolverId = PreExistingId { solverMachine :: Machine, solverSrcId :: PackageId, solverInstId :: UnitId }
              | PlannedId     { solverMachine :: Machine, solverSrcId :: PackageId }
  deriving (Eq, Ord, Generic)

instance Binary SolverId
instance Structured SolverId

instance Show SolverId where
    show = show . solverSrcId

instance Package SolverId where
  packageId = solverSrcId
