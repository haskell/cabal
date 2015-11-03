module Distribution.Client.Dependency.Modular.ConfiguredConversion where

import Data.Maybe
import Prelude hiding (pi)

import Distribution.Client.Types
import Distribution.Client.Dependency.Types (ResolverPackage(..))
import qualified Distribution.Client.PackageIndex as CI
import qualified Distribution.Simple.PackageIndex as SI
import qualified Distribution.PackageDescription  as PD

import Distribution.Client.Dependency.Modular.Configured
import Distribution.Client.Dependency.Modular.Package

import Distribution.Client.ComponentDeps (ComponentDeps)


convCP :: SI.InstalledPackageIndex ->
          CI.PackageIndex SourcePackage ->
          (SourcePackage -> [Dependency]) ->
          CP QPN -> ResolverPackage
convCP iidx sidx sdeps (CP qpi fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting
                  (fromJust $ SI.lookupInstalledPackageId iidx pi)
    Right pi -> Configured $ ConfiguredPackage
                  srcpkg
                  fa
                  es
                  ds'
                  (maybe (sdeps srcpkg) PD.setupDepends
                         (PD.setupBuildInfo pkgdesc))
      where
        Just srcpkg = CI.lookupPackageId sidx pi
        pkgdesc = PD.packageDescription (packageDescription srcpkg)
        ds' :: ComponentDeps [ConfiguredId]
        ds' = fmap (map convConfId) ds

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
