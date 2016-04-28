module Distribution.Solver.Types.ResolverPackage
    ( ResolverPackage(..)
    ) where

import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Solver.Types.SolverPackage

-- | The dependency resolver picks either pre-existing installed packages
-- or it picks source packages along with package configuration.
--
-- This is like the 'InstallPlan.PlanPackage' but with fewer cases.
--
data ResolverPackage loc = PreExisting InstalledPackageInfo
                         | Configured  (SolverPackage loc)

