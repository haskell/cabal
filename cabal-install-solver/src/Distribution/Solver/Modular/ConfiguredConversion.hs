module Distribution.Solver.Modular.ConfiguredConversion
    ( convCP
    ) where

import Data.Maybe
import Prelude hiding (pi)
import Data.Either (partitionEithers)
import Data.Set (Set)
import qualified Data.Set as S

import Distribution.Package (UnitId, packageId)
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo(installedSublibs))

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
  case convPI qpi of
    Left (pi, subPis) ->
                PreExisting $
                  InstSolverPackage {
                    instSolverPkgIPI = addSublibs iidx subPis $ fromJust $ SI.lookupUnitId iidx pi,
                    instSolverPkgLibDeps = fmap fst ds',
                    instSolverPkgExeDeps = fmap snd ds'
                  }
    Right pi -> Configured $
                  SolverPackage {
                      solverPkgSource = srcpkg,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap fst ds',
                      solverPkgExeDeps = fmap snd ds'
                    }
      where
        srcpkg = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi
  where
    ds' :: ComponentDeps ([SolverId] {- lib -}, [SolverId] {- exe -})
    ds' = fmap (partitionEithers . map convConfId) ds

    addSublibs
      :: SI.InstalledPackageIndex
      -> Set UnitId
      -> InstalledPackageInfo
      -> InstalledPackageInfo
    addSublibs idx subPis info =
      info { installedSublibs =
               mapMaybe
                 (SI.lookupUnitId idx)
                 (S.toList subPis)
           }

convPI :: PI QPN -> Either (UnitId, Set UnitId) PackageId
convPI (PI _ (I _ (Inst pi)))             = Left (pi, mempty)
convPI (PI _ (I _ (InstGroup pi subPis))) = Left (pi, subPis)
convPI pi                                 = Right (packageId (either id id (convConfId pi)))

convConfId :: PI QPN -> Either SolverId {- is lib -} SolverId {- is exe -}
convConfId (PI (Q (PackagePath _ q) pn) (I v loc)) =
    case loc of
        Inst pi -> Left (PreExistingId sourceId pi)
        InstGroup pi _subPis -> Left (PreExistingId sourceId pi)
        InRepo
          | QualExe _ pn' <- q
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> Right (PlannedId sourceId)
          | otherwise    -> Left  (PlannedId sourceId)
  where
    sourceId    = PackageIdentifier pn v
