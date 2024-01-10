{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Simple.Build.Monad
  ( BuildM (..)
  , runBuildM
  , PreBuildComponentInputs (..)

    -- * A few queries on @'BuildM'@
  , buildVerbosity
  , buildWhat
  , buildComponent
  , buildCLBI
  , buildBI
  , buildLBI
  , buildCompiler
  , buildTarget

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
import Distribution.Verbosity
import Distribution.Types.Component
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.BuildInfo
import Distribution.Simple.Compiler

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
  deriving (Functor, Applicative, Monad, MonadReader PreBuildComponentInputs, MonadIO) via ReaderT PreBuildComponentInputs IO

-- | Run a 'BuildM' action, i.e. a computation in the context of building a component.
runBuildM :: BuildingWhat -> LocalBuildInfo -> TargetInfo -> BuildM a -> IO a
runBuildM buildingWhat localBuildInfo targetInfo (BuildM f) =
  f PreBuildComponentInputs{buildingWhat, localBuildInfo, targetInfo}
{-# INLINE runBuildM #-}

-- | Get the @'BuildingWhat'@ representing the kind of build we are doing with what flags (Normal vs Repl vs ...)
buildWhat :: BuildM BuildingWhat
buildWhat = asks buildingWhat
{-# INLINE buildWhat #-}

-- | Get the @'Verbosity'@ from the context the component being built is in.
buildVerbosity :: BuildM Verbosity
buildVerbosity = buildingWhatVerbosity <$> buildWhat
{-# INLINE buildVerbosity #-}

-- | Get the @'Component'@ being built.
buildComponent :: BuildM Component
buildComponent = asks (targetComponent . targetInfo)
{-# INLINE buildComponent #-}

-- | Get the @'ComponentLocalBuildInfo'@ for the component being built.
buildCLBI :: BuildM ComponentLocalBuildInfo
buildCLBI = asks (targetCLBI . targetInfo)
{-# INLINE buildCLBI #-}

-- | Get the @'BuildInfo'@ of the component being built.
buildBI :: BuildM BuildInfo
buildBI = componentBuildInfo <$> buildComponent
{-# INLINE buildBI #-}

-- | Get the @'LocalBuildInfo'@ of the component being built.
buildLBI :: BuildM LocalBuildInfo
buildLBI = asks localBuildInfo
{-# INLINE buildLBI #-}

buildCompiler :: BuildM Compiler
buildCompiler = compiler <$> buildLBI
{-# INLINE buildCompiler #-}

-- | Get the @'TargetInfo'@ of the current component being built.
buildTarget :: BuildM TargetInfo
buildTarget = asks targetInfo

