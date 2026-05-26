{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.InstSolverPackage
    ( InstSolverPackage(..)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package ( Package(..), HasUnitId(..) )
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import Distribution.Solver.Types.SolverId
import Distribution.InstalledPackageInfo (InstalledPackageInfo)

-- | An 'InstSolverPackage' is a pre-existing installed package
-- specified by the dependency solver.
data InstSolverPackage = InstSolverPackage {
      instSolverPkgIPI :: InstalledPackageInfo,
      instSolverPkgLibDeps :: ComponentDeps [SolverId],
      instSolverPkgExeDeps :: ComponentDeps [SolverId]
    }
  deriving (Eq, Show, Generic)

instance Binary InstSolverPackage
instance Structured InstSolverPackage

instance Package InstSolverPackage where
    packageId = packageId . instSolverPkgIPI

instance HasUnitId InstSolverPackage where
    installedUnitId = installedUnitId . instSolverPkgIPI
