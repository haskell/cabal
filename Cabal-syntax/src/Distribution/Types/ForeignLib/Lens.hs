{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Types.ForeignLib.Lens
  ( ForeignLib
  , module Distribution.Types.ForeignLib.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.BuildInfo (BuildInfo, BuildInfoWith)
import Distribution.Types.ForeignLib (ForeignLib, ForeignLibWith, LibVersionInfo)
import Distribution.Types.ForeignLibOption (ForeignLibOption)
import Distribution.Types.ForeignLibType (ForeignLibType)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Utils.Path
import Distribution.Version (Version)

import qualified Distribution.Types.ForeignLib as T

foreignLibName :: Lens' (ForeignLibWith mod) UnqualComponentName
foreignLibName f s = fmap (\x -> s{T.foreignLibName = x}) (f (T.foreignLibName s))
{-# INLINE foreignLibName #-}

foreignLibType :: Lens' (ForeignLibWith mod) ForeignLibType
foreignLibType f s = fmap (\x -> s{T.foreignLibType = x}) (f (T.foreignLibType s))
{-# INLINE foreignLibType #-}

foreignLibOptions :: Lens' (ForeignLibWith mod) [ForeignLibOption]
foreignLibOptions f s = fmap (\x -> s{T.foreignLibOptions = x}) (f (T.foreignLibOptions s))
{-# INLINE foreignLibOptions #-}

foreignLibBuildInfo :: forall mod. Lens' (ForeignLibWith mod) (BuildInfoWith mod)
foreignLibBuildInfo f s = fmap (\x -> s{T.foreignLibBuildInfo = x}) (f (T.foreignLibBuildInfo s))
{-# INLINE foreignLibBuildInfo #-}

foreignLibVersionInfo :: Lens' (ForeignLibWith mod) (Maybe LibVersionInfo)
foreignLibVersionInfo f s = fmap (\x -> s{T.foreignLibVersionInfo = x}) (f (T.foreignLibVersionInfo s))
{-# INLINE foreignLibVersionInfo #-}

foreignLibVersionLinux :: Lens' (ForeignLibWith mod) (Maybe Version)
foreignLibVersionLinux f s = fmap (\x -> s{T.foreignLibVersionLinux = x}) (f (T.foreignLibVersionLinux s))
{-# INLINE foreignLibVersionLinux #-}

foreignLibModDefFile :: Lens' (ForeignLibWith mod) [RelativePath Source File]
foreignLibModDefFile f s = fmap (\x -> s{T.foreignLibModDefFile = x}) (f (T.foreignLibModDefFile s))
{-# INLINE foreignLibModDefFile #-}
