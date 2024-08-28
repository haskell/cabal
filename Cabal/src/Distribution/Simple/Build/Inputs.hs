{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Distribution.Simple.Build.Inputs
  ( -- * Inputs of actions for building components
    PreBuildComponentInputs (..)

    -- * Queries over the component being built
  , buildVerbosity
  , buildComponent
  , buildIsLib
  , buildCLBI
  , buildBI
  , buildCompiler

    -- * Re-exports
  , BuildingWhat (..)
  , LocalBuildInfo (..)
  , TargetInfo (..)
  , buildingWhatCommonFlags
  , buildingWhatVerbosity
  , buildingWhatWorkingDir
  , buildingWhatDistPref
  )
where

import Distribution.Simple.Compiler
import Distribution.Simple.Setup hiding
  ( BuildFlags (buildVerbosity)
  )
import Distribution.Types.BuildInfo
import Distribution.Types.Component
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo
import Distribution.Verbosity

-- | The information required for a build computation which is available right
-- before building each component, i.e. the pre-build component inputs.
data PreBuildComponentInputs = PreBuildComponentInputs
  { buildingWhat :: BuildingWhat
  -- ^ What kind of build are we doing?
  , localBuildInfo :: LocalBuildInfo
  -- ^ Information about the package
  , targetInfo :: TargetInfo
  -- ^ Information about an individual component
  }

-- | Get the @'Verbosity'@ from the context the component being built is in.
buildVerbosity :: PreBuildComponentInputs -> Verbosity
buildVerbosity = buildingWhatVerbosity . buildingWhat

-- | Get the @'Component'@ being built.
buildComponent :: PreBuildComponentInputs -> Component
buildComponent = targetComponent . targetInfo

-- | Is the @'Component'@ being built a @'Library'@?
buildIsLib :: PreBuildComponentInputs -> Bool
buildIsLib = do
  component <- buildComponent
  let isLib
        | CLib{} <- component = True
        | otherwise = False
  return isLib
{-# INLINE buildIsLib #-}

-- | Get the @'ComponentLocalBuildInfo'@ for the component being built.
buildCLBI :: PreBuildComponentInputs -> ComponentLocalBuildInfo
buildCLBI = targetCLBI . targetInfo

-- | Get the @'BuildInfo'@ of the component being built.
buildBI :: PreBuildComponentInputs -> BuildInfo
buildBI = componentBuildInfo . buildComponent

-- | Get the @'Compiler'@ being used to build the component.
buildCompiler :: PreBuildComponentInputs -> Compiler
buildCompiler = compiler . localBuildInfo
