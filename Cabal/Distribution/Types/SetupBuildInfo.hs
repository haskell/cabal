{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.SetupBuildInfo (
    SetupBuildInfo(..)
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package

-- ---------------------------------------------------------------------------
-- The SetupBuildInfo type

-- One can see this as a very cut-down version of BuildInfo below.
-- To keep things simple for tools that compile Setup.hs we limit the
-- options authors can specify to just Haskell package dependencies.

data SetupBuildInfo = SetupBuildInfo {
        setupDepends        :: [Dependency],
        setupTool           :: Maybe ExeDependency,
        defaultSetupDepends :: Bool
        -- ^ Is this a default 'custom-setup' section added by the cabal-install
        -- code (as opposed to user-provided)? This field is only used
        -- internally, and doesn't correspond to anything in the .cabal
        -- file. See #3199.
    }
    deriving (Generic, Show, Eq, Read, Typeable, Data)

instance Binary SetupBuildInfo

instance Monoid SetupBuildInfo where
  mempty  = SetupBuildInfo [] Nothing False
  mappend = (<>)

instance Semigroup SetupBuildInfo where
  a <> b = SetupBuildInfo {
    setupDepends        = combine setupDepends,
    setupTool           = combineMby setupTool,
    defaultSetupDepends = defaultSetupDepends a || defaultSetupDepends b
  }
    where
      combine    field = field a `mappend` field b
      combineNub field = nub (combine field)
      combineMby field = field b `mplus` field a
