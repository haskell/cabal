module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)

import Distribution.Package (UnitId)
import qualified Distribution.Simple.PackageIndex as SI

import qualified Distribution.Solver.PackageIndex as CI
import Distribution.Solver.Types (ResolverPackage(..))
import Distribution.Solver.ComponentDeps (ComponentDeps)
import Distribution.Solver.Modular.Configured
import Distribution.Solver.Modular.Package
import Distribution.Solver.Types.SourcePackage
import Distribution.Solver.Types.ConfiguredId
import Distribution.Solver.Types.ConfiguredPackage

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
    Right pi -> Configured $ ConfiguredPackage
                  srcpkg
                  fa
                  es
                  ds'
      where
        Just srcpkg = CI.lookupPackageId sidx pi
  where
    ds' :: ComponentDeps [ConfiguredId]
    ds' = fmap (map convConfId) ds

convPI :: PI QPN -> Either UnitId PackageId
convPI (PI _ (I _ (Inst pi))) = Left pi
convPI qpi                    = Right $ confSrcId $ convConfId qpi

convConfId :: PI QPN -> ConfiguredId
convConfId (PI (Q _ pn) (I v loc)) = ConfiguredId {
      confSrcId  = sourceId
    , confInstId = installedId
    }
  where
    sourceId    = PackageIdentifier pn v
    installedId = case loc of
                    Inst pi    -> pi
                    _otherwise -> fakeUnitId sourceId
