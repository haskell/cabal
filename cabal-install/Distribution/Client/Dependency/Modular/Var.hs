{-# LANGUAGE DeriveFunctor #-}
module Distribution.Client.Dependency.Modular.Var (
    Var(..)
  , simplifyVar
  , showVar
  , varPI
  ) where

import Prelude hiding (pi)

import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package

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
simplifyVar :: Var qpn -> Var qpn
simplifyVar (P qpn)       = P qpn
simplifyVar (F (FN pi _)) = F (FN pi (mkFlag "flag"))
simplifyVar (S qsn)       = S qsn

showVar :: Var QPN -> String
showVar (P qpn) = showQPN qpn
showVar (F qfn) = showQFN qfn
showVar (S qsn) = showQSN qsn

-- | Extract the package instance from a Var
varPI :: Var QPN -> (QPN, Maybe I)
varPI (P qpn)               = (qpn, Nothing)
varPI (F (FN (PI qpn i) _)) = (qpn, Just i)
varPI (S (SN (PI qpn i) _)) = (qpn, Just i)
