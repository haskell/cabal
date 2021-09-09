{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.InstSolverPackage
    ( InstSolverPackage(..)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package ( Package(..), HasMungedPackageId(..), HasUnitId(..) )
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import Distribution.Solver.Types.SolverId
import Distribution.Types.MungedPackageId
import Distribution.Types.PackageId
import Distribution.Types.MungedPackageName
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
    packageId i =
        -- HACK! See Note [Index conversion with internal libraries]
        let MungedPackageId mpn v = mungedId i
        in PackageIdentifier (encodeCompatPackageName mpn) v

instance HasMungedPackageId InstSolverPackage where
    mungedId = mungedId . instSolverPkgIPI

instance HasUnitId InstSolverPackage where
    installedUnitId = installedUnitId . instSolverPkgIPI
