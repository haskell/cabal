module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Distribution.Compat.Prelude (traceShow)
import Data.Maybe
import Prelude hiding (pi)
import Data.Either (partitionEithers)

import Distribution.Package (UnitId, packageId)

import qualified Distribution.Simple.PackageIndex as SI

import Distribution.Solver.Modular.Configured
import Distribution.Solver.Modular.Package
import Distribution.Solver.Types.PackageConstraint

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.PackageIndex as CI
import           Distribution.Solver.Types.PackagePath
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.InstSolverPackage
import           Distribution.Solver.Types.SourcePackage

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
convCP :: SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          CP QPN -> ResolverPackage loc
convCP iidx sidx (CP qpi fa es ds) =
  case convPI qpi of
    (Left  pi, qpn) ->
      PreExisting
        (InstSolverPackage {
          instSolverPkgIPI = fromJust $ SI.lookupUnitId iidx pi,
          instSolverPkgLibDeps = fmap fst ds',
          instSolverPkgExeDeps = fmap snd ds'
        })
        (qpn2Scope qpn)
    (Right pi, qpn) ->
      Configured
        (SolverPackage {
            solverPkgSource = srcpkg,
            solverPkgFlags = fa,
            solverPkgStanzas = es,
            solverPkgLibDeps = fmap fst ds',
            solverPkgExeDeps = fmap snd ds'
        })
        (qpn2Scope qpn)
      where
        srcpkg = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi
  where
    ds' :: ComponentDeps ([SolverId] {- lib -}, [SolverId] {- exe -})
    ds' = fmap (partitionEithers . map convConfId) ds

convPI :: PI QPN -> (Either UnitId PackageId, QPN)
convPI (PI qpn (I _ (Inst pi))) = (Left pi, qpn)
convPI pi@(PI qpn _)            = (Right (packageId (either id id (convConfId pi))), qpn)

convConfId :: PI QPN -> Either SolverId {- is lib -} SolverId {- is exe -}
convConfId (PI qpn@(Q (PackagePath _ q) pn) (I v loc)) =
    case loc of
        Inst pi -> Left (PreExistingId sourceId pi cscope)
        _otherwise
          | QualExe _ pn' <- q
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> Right (PlannedId sourceId cscope)
          | otherwise    -> Left  (PlannedId sourceId cscope)
  where
    sourceId = PackageIdentifier pn v
    cscope = qpn2Scope qpn

-- This will probably become simpler after the private dependencies refactor
-- of Namespace vs Qualifier lands.
qpn2Scope :: QPN -> ConstraintScope
qpn2Scope q@(Q (PackagePath _ns ql) pkg) = traceShow ("qpn2Scope", q, ScopeQualified ql pkg) $ ScopeQualified ql pkg -- case ql of
 -- ROMES:TODO
 --    QualToplevel ->
 --  | QualBase PackageName
 --  | QualSetup PackageName
 --  | QualExe PackageName PackageName

