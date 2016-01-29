module Distribution.Client.Dependency.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)

import Distribution.Package (UnitId)

import Distribution.Client.Types
import Distribution.Client.Dependency.Types (ResolverPackage(..),Dependency)
import qualified Distribution.Client.PackageIndex as CI
import qualified Distribution.Simple.PackageIndex as SI
import qualified Distribution.PackageDescription  as PD

import Distribution.Client.Dependency.Modular.Configured
import Distribution.Client.Dependency.Modular.Package

import Distribution.Client.ComponentDeps (ComponentDeps)

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
convCP :: SI.InstalledPackageIndex ->
          CI.PackageIndex SourcePackage ->
          (SourcePackage -> [Dependency]) ->
          CP QPN -> ResolverPackage
convCP iidx sidx sdeps (CP qpi fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting
                  (fromJust $ SI.lookupUnitId iidx pi)
    Right pi -> Configured $ ConfiguredPackage
                  srcpkg'
                  fa
                  es
                  ds'
      where
        Just srcpkg = CI.lookupPackageId sidx pi
        -- This is a bit of a hack, but the alternatives are worse! (or
        -- require more extensive refactoring).
        --
        -- The issue is that InstallPlan validation checks that we select no
        -- more and no fewer choices for deps than there are deps. That is we
        -- cannot add more deps if the package description doesn't call for it.
        -- This interferes with the idea of supplying default Setup.hs script
        -- deps if none are given in the package explictly. The validator would
        -- see that exactly as an instance of us supplying more setup deps than
        -- the package calls for.
        --
        -- So this hack is to cheat by changing the package description that
        -- the validator looks at, to insert setup deps that were not really
        -- there originally.
        --
        -- One alternative is to stick the default setup deps  into an extra
        -- field in ConfiguredPackage, but that's nasty because it changes
        -- things all over the place, and nobody cares about the info after
        -- validation. Another choice is to pass a map of default setup deps
        -- into the InstalPlan construction and valdidation, but again this is
        -- quite disruptive, and it has to stay with the InstalPlan
        -- persistently so it can be revalidated later.
        --
        srcpkg' = srcpkg {
          packageDescription = gpkgdesc {
            PD.packageDescription = pkgdesc {
              PD.setupBuildInfo = 
                case PD.setupBuildInfo pkgdesc of
                  Just setupbi -> Just setupbi
                  Nothing      -> Just PD.SetupBuildInfo {
                                    PD.setupDepends = sdeps srcpkg
                                  }
            }
          }
        }
        gpkgdesc = packageDescription srcpkg
        pkgdesc  = PD.packageDescription gpkgdesc
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
