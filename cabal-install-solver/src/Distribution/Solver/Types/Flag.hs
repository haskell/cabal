module Distribution.Solver.Types.Flag
    ( FlagType(..)
    ) where

import Prelude (Eq, Show)
import GHC.Generics

import Distribution.Utils.Structured (Structured (..))
import Distribution.Compat.Binary (Binary)

data FlagType = Manual | Automatic
  deriving (Eq, Show, Generic)

instance Structured FlagType
instance Binary FlagType
