{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Var (
    Var(..)
  , showVar
  , varPI
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

-- | Extract the package instance from a Var
varPI :: Var QPN -> (QPN, Maybe I)
varPI (P qpn)               = (qpn, Nothing)
varPI (F (FN (PI qpn i) _)) = (qpn, Just i)
varPI (S (SN (PI qpn i) _)) = (qpn, Just i)
