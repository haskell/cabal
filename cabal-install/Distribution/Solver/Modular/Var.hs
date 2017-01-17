{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Var (
    Var(..)
  , SimpleVar
  , simplifyVar
  , showVar
  , showSimpleVar
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

-- | For computing conflict sets, we map flag choice vars to a
-- single flag choice. This means that all flag choices are treated
-- as interdependent. So if one flag of a package ends up in a
-- conflict set, then all flags are being treated as being part of
-- the conflict set.
simplifyVar :: Var QPN -> SimpleVar
simplifyVar (P qpn)       = SimpleP qpn
simplifyVar (F (FN pi _)) = SimpleF pi
simplifyVar (S qsn)       = SimpleS qsn

-- | Variables used in conflict sets.
data SimpleVar = SimpleP QPN | SimpleF (PI QPN) | SimpleS (SN QPN)
  deriving (Eq, Ord, Show)

showVar :: Var QPN -> String
showVar (P qpn) = showQPN qpn
showVar (F qfn) = showQFN qfn
showVar (S qsn) = showQSN qsn

showSimpleVar :: SimpleVar -> String
showSimpleVar (SimpleP qpn) = showQPN qpn
showSimpleVar (SimpleF pi)  = showQFN $ FN pi (mkFlag "flag")
showSimpleVar (SimpleS qsn) = showQSN qsn

-- | Extract the package instance from a Var
varPI :: Var QPN -> (QPN, Maybe I)
varPI (P qpn)               = (qpn, Nothing)
varPI (F (FN (PI qpn i) _)) = (qpn, Just i)
varPI (S (SN (PI qpn i) _)) = (qpn, Just i)
