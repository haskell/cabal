module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)

import Distribution.Package (UnitId)

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
import Distribution.ModuleName
import Distribution.Types.Dependency (PrivateAlias)

-- | Converts from the solver specific result @CP QPN@ into
-- a 'ResolverPackage', which can then be converted into
-- the install plan.
convCP :: SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          CP QPN -> ResolverPackage loc
convCP iidx sidx (CP qpi fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting $
                  InstSolverPackage {
                    instSolverPkgIPI = fromJust $ SI.lookupUnitId iidx pi,
                    instSolverPkgLibDeps  = fmap (\(b, _) -> map fst b) ds',
                    instSolverPkgExeDeps  = fmap (\(_, c) -> c) ds'
                  }
    Right pi -> Configured $
                  SolverPackage {
                      solverPkgSource = srcpkg,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap (\(b, _) -> b) ds',
                      solverPkgExeDeps = fmap (\(_, c) -> c) ds'
                    }
      where
        srcpkg = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi
  where

    ds' :: ComponentDeps (([(SolverId, Maybe PrivateAlias)] {- lib -}, [SolverId] {- exe -}))
    ds' = fmap (partitionDeps . map convConfId) ds

partitionDeps :: [Converted] -> (([(SolverId, Maybe PrivateAlias)], [SolverId]))
partitionDeps [] = ([], [])
partitionDeps (dep:deps) =
  let (p, e) = partitionDeps deps
  in case dep of
        AliasPkg sid pn -> ((sid, Just pn) : p, e)
        NormalPkg sid -> ((sid, Nothing) :p, e)
        NormalExe sid -> (p, sid:e)



convPI :: PI QPN -> Either UnitId PackageId
convPI (PI _ (I _ (Inst pi))) = Left pi
convPI (PI (Q _ pn) (I v _)) = Right (PackageIdentifier pn v)

data Converted = NormalPkg SolverId | NormalExe SolverId | AliasPkg SolverId PrivateAlias

convConfId :: PI QPN -> Converted
convConfId (PI (Q (PackagePath ns qn) pn) (I v loc)) =
    case loc of
        Inst pi -> NormalPkg (PreExistingId sourceId pi)
        _otherwise
          | IndependentBuildTool _ pn' <- ns
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> NormalExe (PlannedId sourceId)
          | QualAlias _ _ alias _ <- qn -> AliasPkg (PlannedId sourceId) alias
          | otherwise    -> NormalPkg  (PlannedId sourceId)
  where
    sourceId    = PackageIdentifier pn v
