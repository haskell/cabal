{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.GivenComponent
  ( GivenComponent (..)
  ) where

import Distribution.Compat.Prelude

import Distribution.Types.ComponentId
import Distribution.Types.LibraryName
import Distribution.Types.PackageName

-- | A 'GivenComponent' represents a library depended on and explicitly
-- specified by the user/client with @--dependency@
--
-- It enables Cabal to know which 'ComponentId' to associate with a library
--
-- @since 2.3.0.0
data GivenComponent = GivenComponent
  { givenComponentPackage :: PackageName
  , givenComponentName :: LibraryName -- --dependency is for libraries
  -- only, not for any component
  , givenComponentId :: ComponentId
  }
  deriving (Generic, Read, Show, Eq, Typeable)

instance Binary GivenComponent
instance Structured GivenComponent
