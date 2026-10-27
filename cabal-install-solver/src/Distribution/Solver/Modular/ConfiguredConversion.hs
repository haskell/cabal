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
import           Distribution.Solver.Types.GoalScope
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.InstSolverPackage
import           Distribution.Solver.Types.SourcePackage
import           Distribution.Solver.Types.Machine (PerMachine, onMachine)

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
--
-- Installed (pre-existing) packages are looked up in the installed-package
-- index for /their own machine/: a build-machine dependency resolves against the
-- build toolchain's index, a host-machine dependency against the host index. In
-- a non-cross build both machines share the same index.
convCP :: PerMachine SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          CP QPN -> ResolverPackage loc
convCP iidxs sidx (CP qpi fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting $
                  InstSolverPackage {
                    instSolverMachine = machine,
                    instSolverPkgIPI = fromJust $ SI.lookupUnitId (onMachine iidxs machine) pi,
                    instSolverPkgLibDeps = fmap fst ds',
                    instSolverPkgExeDeps = fmap snd ds'
                  }
    Right pi -> Configured $
                  SolverPackage {
                      solverPkgMachine = machine,
                      solverPkgSource = srcpkg,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap fst ds',
                      solverPkgExeDeps = fmap snd ds'
                    }
      where
        srcpkg = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi
  where
    -- The machine of this package, taken from its qualified name. Determines
    -- which per-machine installed-package index a pre-existing package is
    -- resolved against.
    machine = case qpi of PI (Q (GoalScope s _ _) _) _ -> s

    ds' :: ComponentDeps ([SolverId] {- lib -}, [SolverId] {- exe -})
    ds' = fmap (partitionEithers . map convConfId) ds

convPI :: PI QPN -> Either UnitId PackageId
convPI (PI _ (I _ (Inst pi))) = Left pi
convPI pi                     = Right (packageId (either id id (convConfId pi)))

convConfId :: PI QPN -> Either SolverId {- is lib -} SolverId {- is exe -}
convConfId (PI (Q (GoalScope s _ q) pn) (I v loc)) =
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
