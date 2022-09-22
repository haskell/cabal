{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Executable
  ( Executable (..)
  , emptyExecutable
  , exeModules
  , exeModulesAutogen
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Types.BuildInfo
import Distribution.Types.ExecutableScope
import Distribution.Types.UnqualComponentName

import qualified Distribution.Types.BuildInfo.Lens as L

data Executable = Executable
  { exeName :: UnqualComponentName
  , modulePath :: FilePath
  , exeScope :: ExecutableScope
  , buildInfo :: BuildInfo
  }
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance L.HasBuildInfo Executable where
  buildInfo f l = (\x -> l{buildInfo = x}) <$> f (buildInfo l)

instance Binary Executable
instance Structured Executable
instance NFData Executable where rnf = genericRnf

instance Monoid Executable where
  mempty = gmempty
  mappend = (<>)

instance Semigroup Executable where
  a <> b =
    Executable
      { exeName = combineName a b exeName "executable"
      , modulePath = combine modulePath
      , exeScope = combine exeScope
      , buildInfo = combine buildInfo
      }
    where
      combine field = field a `mappend` field b

emptyExecutable :: Executable
emptyExecutable = mempty

-- | Get all the module names from an exe
exeModules :: Executable -> [ModuleName]
exeModules exe = otherModules (buildInfo exe)

-- | Get all the auto generated module names from an exe
-- This are a subset of 'exeModules'.
exeModulesAutogen :: Executable -> [ModuleName]
exeModulesAutogen exe = autogenModules (buildInfo exe)
