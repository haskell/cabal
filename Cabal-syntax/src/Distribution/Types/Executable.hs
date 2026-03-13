{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Distribution.Types.Executable
  ( Executable
  , ExecutableWith (..)
  , emptyExecutable
  , exeModules
  , exeModulesAutogen
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Types.Annotation
import Distribution.Types.BuildInfo
import Distribution.Types.ExecutableScope
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path

import qualified Distribution.Types.BuildInfo.Lens as L

type Executable = ExecutableWith Abst

data ExecutableWith (mod :: ParsingPhase) = Executable
  { exeName :: UnqualComponentName
  , modulePath :: RelativePath Source File
  , exeScope :: ExecutableScope
  , buildInfo :: BuildInfoWith mod
  }
deriving instance Generic Executable
deriving instance Show Executable
deriving instance Read Executable
deriving instance Eq Executable
deriving instance Ord Executable
deriving instance Data Executable

deriving instance Show (ExecutableWith Conc)

instance L.HasBuildInfoWith mod (ExecutableWith mod) where
  buildInfo f l = (\x -> l{buildInfo = x}) <$> f (buildInfo l)

instance Binary Executable
instance Structured Executable
instance NFData Executable where rnf = genericRnf

instance Monoid Executable where
  mempty =
    Executable
      { exeName = mempty
      , modulePath = unsafeMakeSymbolicPath ""
      , exeScope = mempty
      , buildInfo = mempty
      }
  mappend = (<>)

instance Semigroup Executable where
  a <> b =
    Executable
      { exeName = combineNames a b exeName "executable"
      , modulePath = unsafeMakeSymbolicPath $ combineNames a b (getSymbolicPath . modulePath) "modulePath"
      , exeScope = combine exeScope
      , buildInfo = combine buildInfo
      }
    where
      combine field = field a `mappend` field b

instance Monoid (ExecutableWith Conc) where
  mempty = emptyExecutable'

instance Semigroup (ExecutableWith Conc) where
  a <> b =
    Executable
      { exeName = combineNames a b exeName "executable"
      , modulePath = unsafeMakeSymbolicPath $ combineNames a b (getSymbolicPath . modulePath) "modulePath"
      , exeScope = combine exeScope
      , buildInfo = combine buildInfo
      }
    where
      combine field = field a `mappend` field b

emptyExecutable :: Executable
emptyExecutable = mempty

emptyExecutable' :: ExecutableWith Conc
emptyExecutable' =
  Executable
    { exeName = mempty
    , modulePath = unsafeMakeSymbolicPath ""
    , exeScope = mempty
    , buildInfo = mempty
    }

-- | Get all the module names from an exe
exeModules :: Executable -> [ModuleName]
exeModules exe = otherModules (buildInfo exe)

-- | Get all the auto generated module names from an exe
-- This are a subset of 'exeModules'.
exeModulesAutogen :: Executable -> [ModuleName]
exeModulesAutogen exe = autogenModules (buildInfo exe)
