{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.HookedBuildInfo (
    HookedBuildInfo,
    emptyHookedBuildInfo,
  ) where

import Distribution.Types.ComponentName
import Distribution.Types.BuildInfo

type HookedBuildInfo = [(ComponentName, BuildInfo)]

emptyHookedBuildInfo :: HookedBuildInfo
emptyHookedBuildInfo = []
