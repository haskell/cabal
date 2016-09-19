{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.OptionalStanza
    ( OptionalStanza(..)
    , enableStanzas
    ) where

import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary(..))
import Distribution.Types.ComponentRequestedSpec
            (ComponentRequestedSpec(..), defaultComponentRequestedSpec)
import Data.List (foldl')

data OptionalStanza
    = TestStanzas
    | BenchStanzas
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Convert a list of 'OptionalStanza' into the corresponding
-- 'ComponentRequestedSpec' which records what components are enabled.
enableStanzas :: [OptionalStanza] -> ComponentRequestedSpec
enableStanzas = foldl' addStanza defaultComponentRequestedSpec
  where
    addStanza enabled TestStanzas  = enabled { testsRequested = True }
    addStanza enabled BenchStanzas = enabled { benchmarksRequested = True }

instance Binary OptionalStanza
