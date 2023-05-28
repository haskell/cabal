{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ConfVar
  ( ConfVar (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compiler
import Distribution.System
import Distribution.Types.Flag
import Distribution.Types.VersionRange

-- | A @ConfVar@ represents the variable type used.
data ConfVar
  = OS OS
  | Arch Arch
  | PackageFlag FlagName
  | Impl CompilerFlavor VersionRange
  deriving (Eq, Show, Typeable, Data, Generic)

instance Binary ConfVar
instance Structured ConfVar

instance NFData ConfVar where rnf = genericRnf
