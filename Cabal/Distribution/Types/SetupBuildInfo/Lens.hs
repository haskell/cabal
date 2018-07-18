module Distribution.Types.SetupBuildInfo.Lens (
    SetupBuildInfo,
    module Distribution.Types.SetupBuildInfo.Lens,
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compiler
import Distribution.Types.Dependency     (Dependency)
import Distribution.Types.SetupBuildInfo (SetupBuildInfo)

import qualified Distribution.Types.SetupBuildInfo as T

setupOptions :: Lens' SetupBuildInfo [(CompilerFlavor,[String])]
setupOptions f s = fmap (\x -> s { T.setupOptions = x}) (f (T.setupOptions s))
{-# INLINE setupOptions #-}

setupDepends :: Lens' SetupBuildInfo [Dependency]
setupDepends f s = fmap (\x -> s { T.setupDepends = x }) (f (T.setupDepends s))
{-# INLINE setupDepends #-}

defaultSetupDepends :: Lens' SetupBuildInfo Bool
defaultSetupDepends f s = fmap (\x -> s { T.defaultSetupDepends = x }) (f (T.defaultSetupDepends s))
{-# INLINE defaultSetupDepends #-}
