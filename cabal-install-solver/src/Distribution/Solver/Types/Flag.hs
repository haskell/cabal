module Distribution.Solver.Types.Flag
    ( FlagType(..)
    ) where

import Prelude (Eq, Show)

data FlagType = Manual | Automatic
  deriving (Eq, Show)
