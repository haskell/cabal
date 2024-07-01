module Distribution.Types.DefaultBounds.Lens
  ( DefaultBounds
  , module Distribution.Types.DefaultBounds.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Types.DefaultBounds as T

import Distribution.Types.DefaultBounds (DefaultBounds)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.ExeDependency (ExeDependency)

-------------------------------------------------------------------------------
-- GenericPackageDescription
-------------------------------------------------------------------------------

defaultTargetBuildDepends :: Lens' DefaultBounds [Dependency]
defaultTargetBuildDepends f s = fmap (\x -> s{T.defaultTargetBuildDepends = x}) (f (T.defaultTargetBuildDepends s))
{-# INLINE defaultTargetBuildDepends #-}

defaultBuildToolDepends :: Lens' DefaultBounds [ExeDependency]
defaultBuildToolDepends f s = fmap (\x -> s{T.defaultBuildToolDepends = x}) (f (T.defaultBuildToolDepends s))
{-# INLINE defaultBuildToolDepends #-}
