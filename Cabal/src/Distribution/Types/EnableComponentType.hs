{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.EnableComponentType (
    EnableComponentType(..),

    defaultEnableComponentType,
) where

import Prelude ()
import Distribution.Compat.Prelude

-- | Which subset of a given component type to enable. Specified by option
-- triples like @--enable-tests@, @--disable-tests@, and
-- @--enable-tests-when-possible@.
--
-- @since 3.7.0.0
data EnableComponentType
    = EnableAll
    | DisableAll
    | EnableWhenPossible
  deriving (Generic, Read, Show, Eq, Typeable, Bounded, Enum)

instance Binary EnableComponentType
instance Structured EnableComponentType

-- | 'EnableComponentType' is only used for tests and benchmarks, for which the
-- default behaviour of @cabal configure@ is to try to include them in the
-- build plan if possible, and to silently drop them otherwise.
--
-- It's not a big deal to drop them because the default behaviour of @cabal
-- build@ is to build the libraries and executables, but not the tests nor the
-- benchmarks. But it's better to include them in the build plan if we can, so
-- that running @cabal test@ after @cabal build@ doesn't unnecessarily rebuild
-- because of a changed build plan.
--
-- @since 3.7.0.0
defaultEnableComponentType :: EnableComponentType
defaultEnableComponentType = EnableWhenPossible
