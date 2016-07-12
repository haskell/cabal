{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.OptionalStanza
    ( OptionalStanza(..)
    , enableStanzas
    ) where

import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary(..))
import Distribution.Simple.LocalBuildInfo (ComponentEnabledSpec(..), defaultComponentEnabled)
import Data.List (foldl')

data OptionalStanza
    = TestStanzas
    | BenchStanzas
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Convert a list of 'OptionalStanza' into the corresponding
-- 'ComponentEnabledSpec' which records what components are enabled.
enableStanzas :: [OptionalStanza] -> ComponentEnabledSpec
enableStanzas = foldl' addStanza defaultComponentEnabled
  where
    addStanza enabled TestStanzas  = enabled { testsEnabled = True }
    addStanza enabled BenchStanzas = enabled { benchmarksEnabled = True }

instance Binary OptionalStanza
