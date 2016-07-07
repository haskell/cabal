{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.SolverId
    ( SolverId(..)
    )

where

import Distribution.Compat.Binary (Binary(..))
import Distribution.Package (PackageId, Package(..), UnitId(..), HasUnitId(..))
import Distribution.Solver.Types.Internal.Utils (unsafeInternalFakeUnitId)
import GHC.Generics (Generic)

-- | The solver can produce references to existing packages or
-- packages we plan to install.  Unlike 'ConfiguredId' we don't
-- yet know the 'UnitId' for planned packages, because it's
-- not the solver's job to compute them.
--
data SolverId = PreExistingId { solverSrcId :: PackageId, solverInstId :: UnitId }
              | PlannedId     { solverSrcId :: PackageId }
  deriving (Eq, Ord, Generic)

instance Binary SolverId

instance Show SolverId where
    show = show . solverSrcId

instance Package SolverId where
  packageId = solverSrcId

instance HasUnitId SolverId where
  installedUnitId (PreExistingId _ instId) = instId
  installedUnitId (PlannedId pid) = unsafeInternalFakeUnitId pid
