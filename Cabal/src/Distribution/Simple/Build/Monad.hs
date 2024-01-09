{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Simple.Build.Monad
  ( BuildM (..)
  , runBuildM
  , PreBuildComponentInputs (..)

    -- * Re-exports
  , BuildingWhat (..)
  , LocalBuildInfo (..)
  , TargetInfo (..)
  , buildingWhatVerbosity
  , buildingWhatDistPref
  )
where

import Control.Monad.Reader

import Distribution.Simple.Setup (BuildingWhat (..), buildingWhatDistPref, buildingWhatVerbosity)
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo

-- | The information required for a build computation (@'BuildM'@)
-- which is available right before building each component, i.e. the pre-build
-- component inputs.
data PreBuildComponentInputs = PreBuildComponentInputs
  { buildingWhat :: BuildingWhat
  -- ^ What kind of build are we doing?
  , localBuildInfo :: LocalBuildInfo
  -- ^ Information about the package
  , targetInfo :: TargetInfo
  -- ^ Information about an individual component
  }

-- | Computations carried out in the context of building a component (e.g. @'buildAllExtraSources'@)
newtype BuildM a = BuildM (PreBuildComponentInputs -> IO a)
  deriving (Functor, Applicative, Monad) via ReaderT PreBuildComponentInputs IO

-- | Run a 'BuildM' action, i.e. a computation in the context of building a component.
runBuildM :: BuildingWhat -> LocalBuildInfo -> TargetInfo -> BuildM a -> IO a
runBuildM buildingWhat localBuildInfo targetInfo (BuildM f) =
  f PreBuildComponentInputs{buildingWhat, localBuildInfo, targetInfo}
