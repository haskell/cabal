{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.SetupBuildInfo
  ( SetupBuildInfo (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Dependency

-- ---------------------------------------------------------------------------
-- The SetupBuildInfo type

-- One can see this as a very cut-down version of BuildInfo below.
-- To keep things simple for tools that compile Setup.hs we limit the
-- options authors can specify to just Haskell package dependencies.

data SetupBuildInfo = SetupBuildInfo
  { setupDepends :: [Dependency]
  , defaultSetupDepends :: Bool
  -- ^ Is this a default 'custom-setup' section added by the cabal-install
  -- code (as opposed to user-provided)? This field is only used
  -- internally, and doesn't correspond to anything in the .cabal
  -- file. See #3199.
  }
  deriving (Generic, Show, Eq, Ord, Read, Typeable, Data)

instance Binary SetupBuildInfo
instance Structured SetupBuildInfo
instance NFData SetupBuildInfo where rnf = genericRnf

instance Monoid SetupBuildInfo where
  mempty = SetupBuildInfo [] False
  mappend = (<>)

instance Semigroup SetupBuildInfo where
  a <> b =
    SetupBuildInfo
      (setupDepends a <> setupDepends b)
      (defaultSetupDepends a || defaultSetupDepends b)
