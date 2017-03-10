{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.InstSolverPackage 
    ( InstSolverPackage(..)
    ) where

import Distribution.Compat.Binary (Binary(..))
import Distribution.Package ( Package(..), HasMungedPackageId(..), HasUnitId(..) )
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import Distribution.Solver.Types.SolverId
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import GHC.Generics (Generic)

-- | An 'InstSolverPackage' is a pre-existing installed pacakge
-- specified by the dependency solver.
data InstSolverPackage = InstSolverPackage {
      instSolverPkgIPI :: InstalledPackageInfo,
      instSolverPkgLibDeps :: ComponentDeps [SolverId],
      instSolverPkgExeDeps :: ComponentDeps [SolverId]
    }
  deriving (Eq, Show, Generic)

instance Binary InstSolverPackage

instance Package InstSolverPackage where
    packageId = packageId . instSolverPkgIPI

instance HasMungedPackageId InstSolverPackage where
    mungedId = mungedId . instSolverPkgIPI

instance HasUnitId InstSolverPackage where
    installedUnitId = installedUnitId . instSolverPkgIPI
