{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Executable
  ( Executable (..)
  , emptyExecutable
  , exeModules
  , exeModulesAutogen
  , insertExeImports
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.ConfVar
import Distribution.Types.Dependency
import Distribution.Types.ExecutableScope
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path

import qualified Distribution.Types.BuildInfo.Lens as L

data Executable = Executable
  { exeName :: UnqualComponentName
  , exeImports :: [(ImportName, CondTree ConfVar [Dependency] BuildInfo)]
  -- ^ Retained condTree imports, not merged
  , modulePath :: RelativePath Source File
  , exeScope :: ExecutableScope
  , buildInfo :: BuildInfo
  -- ^ the BuildInfo defined locally, unmerged with imports
  }
  -- TODO(leana8959): instances
  deriving (Generic, Show {- Read, -}, Eq {- Ord, -}, Data)

insertExeImports
  :: CondTree ConfVar [Dependency] (WithImports Executable)
  -> CondTree ConfVar [Dependency] Executable
insertExeImports = mapCondTree f id id
  where
    f :: WithImports Executable -> Executable
    f (WithImports importNames exe) = exe{exeImports = importNames}

instance L.HasBuildInfo Executable where
  buildInfo f l = (\x -> l{buildInfo = x}) <$> f (buildInfo l)

instance Binary Executable
instance Structured Executable
instance NFData Executable where rnf = genericRnf

instance Monoid Executable where
  mempty =
    Executable
      { exeName = mempty
      , exeImports = mempty
      , modulePath = unsafeMakeSymbolicPath ""
      , exeScope = mempty
      , buildInfo = mempty
      }
  mappend = (<>)

instance Semigroup Executable where
  a <> b =
    Executable
      { exeName = combineNames a b exeName "executable"
      , exeImports = combine exeImports
      , modulePath = unsafeMakeSymbolicPath $ combineNames a b (getSymbolicPath . modulePath) "modulePath"
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
