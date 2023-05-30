{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.DumpBuildInfo
  ( DumpBuildInfo (..)
  ) where

import Distribution.Compat.Prelude

data DumpBuildInfo
  = NoDumpBuildInfo
  | DumpBuildInfo
  deriving (Read, Show, Eq, Ord, Enum, Bounded, Generic, Typeable)

instance Binary DumpBuildInfo
instance Structured DumpBuildInfo
