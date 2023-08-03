{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Verbosity.Internal
  ( VerbosityLevel (..)
  , VerbosityFlag (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

data VerbosityLevel = Silent | Normal | Verbose | Deafening
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Typeable)

instance Binary VerbosityLevel
instance Structured VerbosityLevel

data VerbosityFlag
  = VCallStack
  | VCallSite
  | VMarkOutput
  | VTimestamp
  | -- | @since 3.4.0.0
    VStderr
  | VNoWarn
  | -- | @since 3.12.1.0
    VWrap
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Typeable)

instance Binary VerbosityFlag
instance Structured VerbosityFlag
