module Distribution.Client.Dependency.Modular where

-- Here, we try to map between the external cabal-install solver
-- interface and the internal interface that the solver actually
-- expects. There are a number of type conversions to perform: we
-- have to convert the package indices to the uniform index used
-- by the solver; we also have to convert the initial constraints;
-- and finally, we have to convert back the resulting install
-- plan.

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

modularSolver :: DependencyResolver
modularSolver (Platform arch os) cid iidx sidx pprefs pcs pns =
  fmap undefined $ -- convert install plan
  logToProgress $
  defaultSolver idx gprefs uprefs goals gcs gfcs lfcs
    where
      idx    = convPIs os arch cid undefined sidx
      gprefs = undefined
      uprefs = undefined
      goals  = undefined
      gcs    = undefined
      gfcs   = undefined
      lfcs   = undefined
