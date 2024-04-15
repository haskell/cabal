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
import Distribution.Types.Dependency (IsPrivate(..), PrivateAlias)

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
                    instSolverPkgLibDeps  = fmap fst (ds' Nothing),
                    instSolverPkgExeDeps  = fmap snd (ds' Nothing)
                  }
    Right pi -> Configured $
                  let libAndExeDeps = ds' (Just (pkgName pi))
                   in SolverPackage {
                      solverPkgSource = srcpkg,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap fst libAndExeDeps,
                      solverPkgExeDeps = fmap snd libAndExeDeps
                    }
      where
        srcpkg = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi
  where

    ds' :: Maybe PackageName -> ComponentDeps (([(SolverId, IsPrivate)] {- lib -}, [SolverId] {- exe -}))
    ds' pn = fmap (partitionDeps . map (convConfId pn)) ds

partitionDeps :: [Converted] -> (([(SolverId, IsPrivate)], [SolverId]))
partitionDeps [] = ([], [])
partitionDeps (dep:deps) =
  let (p, e) = partitionDeps deps
  in case dep of
        AliasPkg sid pn -> ((sid, Private pn) : p, e)
        NormalPkg sid -> ((sid, Public) :p, e)
        NormalExe sid -> (p, sid:e)

convPI :: PI QPN -> Either UnitId PackageId
convPI (PI _ (I _ (Inst pi))) = Left pi
convPI (PI (Q _ pn) (I v _)) = Right (PackageIdentifier pn v)

data Converted = NormalPkg SolverId | NormalExe SolverId | AliasPkg SolverId PrivateAlias
  deriving Show

convConfId :: Maybe PackageName -> PI QPN -> Converted
convConfId parent (PI (Q (PackagePath ns qn) pn) (I v loc)) =
    case loc of
        Inst pi
          -- As below, we need to identify where `AliasPkg` applies. This is
          -- needed to qualify `solverPkgLibDeps` since we may have multiple
          -- instances of the same package qualified.
          | QualAlias pn' _ alias <- qn
          , parent == Just pn' -> AliasPkg (PreExistingId sourceId pi) alias

          | otherwise
          -> NormalPkg (PreExistingId sourceId pi)
        _otherwise
          -- Same reasoning as for exes, the "top" qualified goal is the one
          -- which is private and needs to be aliased, but there might be other goals underneath which
          -- are solved in the same scope (but are not private)
          | QualAlias pn' _ alias <- qn
          , parent == Just pn' -> AliasPkg (PlannedId sourceId) alias

          | IndependentBuildTool _ pn' <- ns
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> NormalExe (PlannedId sourceId)

          | otherwise    -> NormalPkg  (PlannedId sourceId)
  where
    sourceId    = PackageIdentifier pn v
