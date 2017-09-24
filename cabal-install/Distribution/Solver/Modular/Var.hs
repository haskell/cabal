{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Var (
    Var(..)
  , showVar
  , varPN
  ) where

import Prelude hiding (pi)

import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Types.PackagePath

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | The type of variables that play a role in the solver.
-- Note that the tree currently does not use this type directly,
-- and rather has separate tree nodes for the different types of
-- variables. This fits better with the fact that in most cases,
-- these have to be treated differently.
data Var qpn = P qpn | F (FN qpn) | S (SN qpn)
  deriving (Eq, Ord, Show, Functor)

showVar :: Var QPN -> String
showVar (P qpn) = showQPN qpn
showVar (F qfn) = showQFN qfn
showVar (S qsn) = showQSN qsn

-- | Extract the package name from a Var
varPN :: Var qpn -> qpn
varPN (P qpn)               = qpn
varPN (F (FN (PI qpn _) _)) = qpn
varPN (S (SN (PI qpn _) _)) = qpn
