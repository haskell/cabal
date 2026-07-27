module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)
import Data.Either (partitionEithers)

import Distribution.Package (UnitId, packageId)

import qualified Distribution.Simple.PackageIndex as SI

import Distribution.Solver.Modular.Configured
import Distribution.Solver.Modular.Package

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.PackageIndex as CI
import           Distribution.Solver.Types.PackagePath
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.InstSolverPackage
import           Distribution.Solver.Types.SourcePackage
import           Distribution.Solver.Types.Stage (Staged, getStage)

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
--
-- Installed (pre-existing) packages are looked up in the installed-package
-- index for /their own stage/: a build-stage dependency resolves against the
-- build toolchain's index, a host-stage dependency against the host index. In
-- a non-cross build both stages share the same index.
convCP :: Staged SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          CP QPN -> ResolverPackage loc
convCP iidxs sidx (CP qpi fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting $
                  InstSolverPackage {
                    instSolverStage = stage,
                    instSolverPkgIPI = fromJust $ SI.lookupUnitId (getStage iidxs stage) pi,
                    instSolverPkgLibDeps = fmap fst ds',
                    instSolverPkgExeDeps = fmap snd ds'
                  }
    Right pi -> Configured $
                  SolverPackage {
                      solverPkgStage = stage,
                      solverPkgSource = srcpkg,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap fst ds',
                      solverPkgExeDeps = fmap snd ds'
                    }
      where
        srcpkg = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi
  where
    -- The stage of this package, taken from its qualified name. Determines
    -- which per-stage installed-package index a pre-existing package is
    -- resolved against.
    stage = case qpi of PI (Q (PackagePath s _ _) _) _ -> s

    ds' :: ComponentDeps ([SolverId] {- lib -}, [SolverId] {- exe -})
    ds' = fmap (partitionEithers . map convConfId) ds

convPI :: PI QPN -> Either UnitId PackageId
convPI (PI _ (I _ (Inst pi))) = Left pi
convPI pi                     = Right (packageId (either id id (convConfId pi)))

convConfId :: PI QPN -> Either SolverId {- is lib -} SolverId {- is exe -}
convConfId (PI (Q (PackagePath s _ q) pn) (I v loc)) =
    case loc of
        Inst pi -> Left (PreExistingId s sourceId pi)
        _otherwise
          | QualExe _ pn' <- q
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> Right (PlannedId s sourceId)
          | otherwise    -> Left  (PlannedId s sourceId)
  where
    sourceId    = PackageIdentifier pn v
