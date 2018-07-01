{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.GivenComponent (
  GivenComponent(..)
) where

import Distribution.Compat.Prelude

import Distribution.Types.ComponentId
import Distribution.Types.ComponentName
import Distribution.Types.PackageName

-- | A 'GivenComponent' represents a component depended on and explicitly
-- specified by the user/client with @--dependency@
--
-- It enables Cabal to know which 'ComponentId' to associate with a component
--
-- @since 2.3.0.0
data GivenComponent =
  GivenComponent
    { givenComponentPackage :: PackageName
    , givenComponentName    :: ComponentName
    , givenComponentId      :: ComponentId }
  deriving (Generic, Read, Show, Eq, Typeable)

instance Binary GivenComponent

