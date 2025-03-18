module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)
import Data.Either (partitionEithers)

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

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
convCP :: SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          CP QPN -> ResolverPackage loc
convCP iidx sidx (CP qpi fa es ds) =
  case qpi of
    -- Installed
    (PI qpn (I _stage _ (Inst pi)))  ->
      PreExisting $
                  InstSolverPackage {
                    instSolverQPN = qpn,
                    instSolverPkgIPI = fromMaybe (error "convCP: lookupUnitId failed") $ SI.lookupUnitId iidx pi,
                    instSolverPkgLibDeps = fmap fst ds',
                    instSolverPkgExeDeps = fmap snd ds'
                  }
    -- "In repo" i.e. a source package
    (PI qpn@(Q _path pn) (I stage v InRepo)) ->
      let pi = PackageIdentifier pn v in
      Configured $
                  SolverPackage {
                      solverPkgQPN = qpn,
                      solverPkgStage = stage,
                      solverPkgSource = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap fst ds',
                      solverPkgExeDeps = fmap snd ds'
                    }
  where
    ds' :: ComponentDeps ([SolverId] {- lib -}, [SolverId] {- exe -})
    ds' = fmap (partitionEithers . map convConfId) ds

convConfId :: PI QPN -> Either SolverId {- is lib -} SolverId {- is exe -}
convConfId (PI (Q (PackagePath _ q) pn) (I _stage v loc)) =
    case loc of
        Inst pi ->
          Left (PreExistingId sourceId pi)
        InRepo
          | QualExe _ pn' <- q
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> Right (PlannedId sourceId)
          | otherwise -> Left  (PlannedId sourceId)
  where
    sourceId    = PackageIdentifier pn v
