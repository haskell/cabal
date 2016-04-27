module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)

import Distribution.Package (UnitId, packageId)

import qualified Distribution.Simple.PackageIndex as SI

import Distribution.Solver.Modular.Configured
import Distribution.Solver.Modular.Package

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.PackageIndex as CI
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.SourcePackage

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
convCP :: SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          CP QPN -> ResolverPackage loc
convCP iidx sidx (CP qpi fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting
                  (fromJust $ SI.lookupUnitId iidx pi)
    Right pi -> Configured $ SolverPackage
                  srcpkg
                  fa
                  es
                  ds'
      where
        Just srcpkg = CI.lookupPackageId sidx pi
  where
    ds' :: ComponentDeps [SolverId]
    ds' = fmap (map convConfId) ds

convPI :: PI QPN -> Either UnitId PackageId
convPI (PI _ (I _ (Inst pi))) = Left pi
convPI pi                     = Right (packageId (convConfId pi))

convConfId :: PI QPN -> SolverId
convConfId (PI (Q _ pn) (I v loc)) =
    case loc of
        Inst pi -> PreExistingId sourceId pi
        _otherwise -> PlannedId sourceId
  where
    sourceId    = PackageIdentifier pn v
