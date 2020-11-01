{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Solver.Types.OptionalStanza
    ( OptionalStanza(..)
    , showStanza
    , enableStanzas
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()
import Distribution.Types.ComponentRequestedSpec
            (ComponentRequestedSpec(..))

data OptionalStanza
    = TestStanzas
    | BenchStanzas
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, Typeable)

-- | String representation of an OptionalStanza.
showStanza :: OptionalStanza -> String
showStanza TestStanzas  = "test"
showStanza BenchStanzas = "bench"

-- | Convert a list of 'OptionalStanza' into the corresponding
-- 'ComponentRequestedSpec' which records what components are enabled.

-- Note: [OptionalStanza] could become PerOptionalStanza Bool.
-- See https://github.com/haskell/cabal/issues/6918
enableStanzas :: [OptionalStanza] -> ComponentRequestedSpec
enableStanzas optionalStanzas = ComponentRequestedSpec {
    testsRequested      = any (== TestStanzas)  optionalStanzas
  , benchmarksRequested = any (== BenchStanzas) optionalStanzas
  }

instance Binary OptionalStanza
instance Structured OptionalStanza
