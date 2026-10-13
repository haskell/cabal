{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Distribution.Client.Dependency.Types
  ( PreSolver (..)
  , Solver (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Text.PrettyPrint (text)

import qualified Distribution.Compat.CharParsing as P

-- | All the solvers that can be selected.
data PreSolver = AlwaysModular
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

-- | All the solvers that can be used.
data Solver = Modular
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance Binary PreSolver
instance Binary Solver

instance NFData PreSolver

instance Structured PreSolver
instance Structured Solver

instance Pretty PreSolver where
  pretty AlwaysModular = text "modular"

instance Parsec PreSolver where
  parsec = do
    name <- P.munch1 isAlpha
    case map toLower name of
      "modular" -> return AlwaysModular
      _ -> P.unexpected $ "PreSolver: " ++ name
