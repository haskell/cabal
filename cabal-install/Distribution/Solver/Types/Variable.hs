module Distribution.Solver.Types.Variable where

import Prelude (Eq, Show)

import Distribution.Solver.Types.OptionalStanza

import Distribution.PackageDescription (FlagName)

-- | Variables used by the dependency solver. This type is similar to the
-- internal 'Var' type.
data Variable qpn =
    PackageVar qpn
  | FlagVar qpn FlagName
  | StanzaVar qpn OptionalStanza
  deriving (Eq, Show)
