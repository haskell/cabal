module Distribution.Client.SolverInstallPlan(
    SolverInstallPlan,
    SolverPlanPackage,
    configureInstallPlan,
    new
) where

import Distribution.Solver.Types.SolverPackage
import Distribution.Client.Types
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.PackageDescription as PD
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( PackageIdentifier(..), Package(..)
         , HasUnitId(..), UnitId(..) )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Text
         ( display )

import Distribution.Client.InstallPlan

-- | 'GenericInstallPlan' that the solver produces.  We'll "run this" in
-- order to compute the 'UnitId's for everything we want to build.
type SolverInstallPlan = GenericInstallPlan
                         InstalledPackageInfo (SolverPackage UnresolvedPkgLoc)
                         -- Technically, these are not used here, but
                         -- setting the type this way makes it easier
                         -- to run some operations.
                         BuildSuccess BuildFailure

type SolverPlanPackage = GenericPlanPackage
                         InstalledPackageInfo (SolverPackage UnresolvedPkgLoc)
                         BuildSuccess BuildFailure

-- | Conversion of 'SolverInstallPlan' to 'InstallPlan'.
-- Similar to 'elaboratedInstallPlan'
configureInstallPlan :: SolverInstallPlan -> InstallPlan
configureInstallPlan solverPlan =
    flip mapPreservingGraph solverPlan $ \mapDep planpkg ->
      case planpkg of
        PreExisting pkg ->
          PreExisting pkg

        Configured  pkg ->
          Configured (configureSolverPackage mapDep pkg)

        _ -> error "configureInstallPlan: unexpected package state"
  where
    configureSolverPackage :: (UnitId -> UnitId)
                           -> SolverPackage UnresolvedPkgLoc
                           -> ConfiguredPackage UnresolvedPkgLoc
    configureSolverPackage mapDep spkg =
      ConfiguredPackage {
        confPkgId = SimpleUnitId
                  $ Configure.computeComponentId
                        Cabal.NoFlag
                        (packageId spkg)
                        (PD.CLibName (display (pkgName (packageId spkg))))
                        -- TODO: this is a hack that won't work for Backpack.
                        (map ((\(SimpleUnitId cid0) -> cid0) . confInstId)
                             (CD.libraryDeps deps))
                        (solverPkgFlags spkg),
        confPkgSource = solverPkgSource spkg,
        confPkgFlags  = solverPkgFlags spkg,
        confPkgStanzas = solverPkgStanzas spkg,
        confPkgDeps   = deps
      }
      where
        deps = fmap (map (configureSolverId mapDep)) (solverPkgDeps spkg)

    configureSolverId mapDep sid =
      ConfiguredId {
        confSrcId  = packageId sid, -- accurate!
        confInstId = mapDep (installedUnitId sid)
      }
