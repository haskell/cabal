module Distribution.Client.Dependency.Modular where

-- Here, we try to map between the external cabal-install solver
-- interface and the internal interface that the solver actually
-- expects. There are a number of type conversions to perform: we
-- have to convert the package indices to the uniform index used
-- by the solver; we also have to convert the initial constraints;
-- and finally, we have to convert back the resulting install
-- plan.

import Distribution.Client.Dependency.Types

import Distribution.Client.Dependency.Modular.IndexConversion
import Distribution.Client.Dependency.Modular.Solver

modularSolver :: DependencyResolver
modularSolver (Platform arch os) cid iidx sidx pprefs pcs pns =
  undefined
