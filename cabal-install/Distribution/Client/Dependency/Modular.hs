module Distribution.Client.Dependency.Modular where

-- Here, we try to map between the external cabal-install solver
-- interface and the internal interface that the solver actually
-- expects. There are a number of type conversions to perform: we
-- have to convert the package indices to the uniform index used
-- by the solver; we also have to convert the initial constraints;
-- and finally, we have to convert back the resulting install
-- plan.

import Data.Map as M
         ( empty )
import Distribution.Client.Dependency.Modular.Assignment
         ( Assignment, toCPs )
import Distribution.Client.Dependency.Modular.Dependency
         ( RevDepMap )
import Distribution.Client.Dependency.Modular.ConfiguredConversion
         ( convCP )
import Distribution.Client.Dependency.Modular.IndexConversion
         ( convPIs )
import Distribution.Client.Dependency.Modular.Log
         ( logToProgress )
import Distribution.Client.Dependency.Modular.Solver
         ( defaultSolver )
import Distribution.Client.Dependency.Types
         ( DependencyResolver )
import Distribution.Client.InstallPlan
         ( PlanPackage )
import Distribution.System
         ( Platform(..) )

modularResolver :: DependencyResolver
modularResolver (Platform arch os) cid iidx sidx pprefs pcs pns =
  fmap (uncurry postprocess) $ -- convert install plan
  logToProgress $
  defaultSolver idx gprefs uprefs goals gcs gfcs lfcs
    where
      idx    = convPIs os arch cid iidx sidx
      gprefs = M.empty  -- global preferences
      uprefs = M.empty  -- user preferences
      goals  = pns      -- goals/targets
      gcs    = []       -- global constraints
      gfcs   = M.empty  -- global flag choices
      lfcs   = M.empty  -- local flag choices
      postprocess :: Assignment -> RevDepMap -> [PlanPackage]
      postprocess a rdm = map (convCP iidx sidx) (toCPs a rdm)
