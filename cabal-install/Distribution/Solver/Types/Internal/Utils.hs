module Distribution.Solver.Types.Internal.Utils
    ( unsafeInternalFakeUnitId
    ) where

import Distribution.Package (PackageId, UnitId, mkUnitId)
import Distribution.Text (display)

-- | In order to reuse the implementation of PackageIndex which relies
-- on 'UnitId' for 'SolverInstallPlan', we need to be able to synthesize
-- these IDs prior to installation.   These should never be written out!
-- Additionally, they need to be guaranteed unique within the install
-- plan; this holds because an install plan only ever contains one
-- instance of a particular package and version.  (To fix this,
-- the IDs not only have to identify a package ID, but also the
-- transitive requirementso n it.)
unsafeInternalFakeUnitId :: PackageId -> UnitId
unsafeInternalFakeUnitId = mkUnitId . (".fake."++) . display

