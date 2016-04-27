{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.OptionalStanza
    ( OptionalStanza(..)
    ) where

import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary(..))

data OptionalStanza
    = TestStanzas
    | BenchStanzas
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Binary OptionalStanza
