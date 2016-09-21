module Distribution.Solver.Modular
         ( modularResolver, SolverConfig(..)) where

-- Here, we try to map between the external cabal-install solver
-- interface and the internal interface that the solver actually
-- expects. There are a number of type conversions to perform: we
-- have to convert the package indices to the uniform index used
-- by the solver; we also have to convert the initial constraints;
-- and finally, we have to convert back the resulting install
-- plan.

import Data.Map as M
         ( fromListWith )
import Distribution.Solver.Modular.Assignment
         ( toCPs )
import Distribution.Solver.Modular.ConfiguredConversion
         ( convCP )
import Distribution.Solver.Modular.IndexConversion
         ( convPIs )
import Distribution.Solver.Modular.Log
         ( logToProgress )
import Distribution.Solver.Modular.Package
         ( PN )
import Distribution.Solver.Modular.Solver
         ( SolverConfig(..), solve )
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.DependencyResolver
import Distribution.System
         ( Platform(..) )

-- | Ties the two worlds together: classic cabal-install vs. the modular
-- solver. Performs the necessary translations before and after.
modularResolver :: SolverConfig -> DependencyResolver loc
modularResolver sc (Platform arch os) cinfo iidx sidx pkgConfigDB pprefs pcs pns =
  fmap (uncurry postprocess)      $ -- convert install plan
  logToProgress (maxBackjumps sc) $ -- convert log format into progress format
  solve sc cinfo idx pkgConfigDB pprefs gcs pns
    where
      -- Indices have to be converted into solver-specific uniform index.
      idx    = convPIs os arch cinfo (shadowPkgs sc) (strongFlags sc) (solveExecutables sc) iidx sidx
      -- Constraints have to be converted into a finite map indexed by PN.
      gcs    = M.fromListWith (++) (map pair pcs)
        where
          pair lpc = (pcName $ unlabelPackageConstraint lpc, [lpc])

      -- Results have to be converted into an install plan.
      postprocess a rdm = map (convCP iidx sidx) (toCPs a rdm)

      -- Helper function to extract the PN from a constraint.
      pcName :: PackageConstraint -> PN
      pcName (PackageConstraintVersion   pn _) = pn
      pcName (PackageConstraintInstalled pn  ) = pn
      pcName (PackageConstraintSource    pn  ) = pn
      pcName (PackageConstraintFlags     pn _) = pn
      pcName (PackageConstraintStanzas   pn _) = pn
