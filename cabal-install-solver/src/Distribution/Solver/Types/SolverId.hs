module Distribution.Solver.Types.SolverId
    ( SolverId(..)
    )

where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package (PackageId, Package(..), UnitId)
import Distribution.Pretty (flatStyle)
import Distribution.Solver.Types.GoalScope (GoalScope, dispGoalScope)
import Distribution.Solver.Types.Machine (Machine)
import qualified Text.PrettyPrint as Disp

-- | The solver can produce references to existing packages or
-- packages we plan to install.  Unlike 'ConfiguredId' we don't
-- yet know the 'UnitId' for planned packages, because it's
-- not the solver's job to compute them.
--
-- Each reference also carries the 'Machine' of the package it points at:
-- under cross-compilation the same package may be built for both the build
-- and host machines, so the machine is part of a package's identity.
--
-- A plan can also contain more than one instance of the same package version
-- in one machine: a build tool or a setup script is solved in its own qualified
-- scope, and when its dependencies cannot agree with another instance of the
-- same version, it gets its own. 'solverQualifier' tells such instances apart.
-- It is 'Nothing' for the first instance of a package version, which is the
-- only one in most plans, and @'Just' path@ for any other, @path@ being the
-- package path of the goal the instance was chosen for.
--
-- Pre-existing packages are told apart by their 'UnitId' instead.
--
data SolverId = PreExistingId { solverMachine :: Machine, solverSrcId :: PackageId, solverInstId :: UnitId }
              | PlannedId     { solverMachine :: Machine, solverSrcId :: PackageId, solverQualifier :: Maybe GoalScope }
  deriving (Eq, Ord, Generic)

instance Binary SolverId
instance Structured SolverId

instance Show SolverId where
    show (PlannedId _ pid (Just pp)) =
      show pid ++ " for " ++ Disp.renderStyle flatStyle (dispGoalScope pp)
    show sid = show (solverSrcId sid)

instance Package SolverId where
  packageId = solverSrcId
