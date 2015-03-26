module Distribution.Client.Dependency.Modular.ConfiguredConversion where

import Data.Maybe
import Prelude hiding (pi)

import Distribution.Client.InstallPlan
import Distribution.Client.Types
import Distribution.Compiler
import qualified Distribution.Client.PackageIndex as CI
import qualified Distribution.Simple.PackageIndex as SI
import Distribution.System

import Distribution.Client.Dependency.Modular.Configured
import Distribution.Client.Dependency.Modular.Package

mkPlan :: Platform -> CompilerInfo ->
          SI.InstalledPackageIndex -> CI.PackageIndex SourcePackage ->
          [CP QPN] -> Either [PlanProblem] InstallPlan
mkPlan plat comp iidx sidx cps =
  new plat comp (SI.fromList (map (convCP iidx sidx) cps))

convCP :: SI.InstalledPackageIndex -> CI.PackageIndex SourcePackage ->
          CP QPN -> PlanPackage
convCP iidx sidx (CP qpi fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting $ InstalledPackage
                  (fromJust $ SI.lookupInstalledPackageId iidx pi)
                  (map confSrcId ds')
    Right pi -> Configured $ ConfiguredPackage
                  (fromJust $ CI.lookupPackageId sidx pi)
                  fa
                  es
                  ds'
  where
    ds' :: [ConfiguredId]
    ds' = map convConfId ds

convPI :: PI QPN -> Either InstalledPackageId PackageId
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
                    _otherwise -> fakeInstalledPackageId sourceId
