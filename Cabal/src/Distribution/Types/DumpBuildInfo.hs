{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.DumpBuildInfo
  ( DumpBuildInfo (..)
  ) where

import Distribution.Compat.Prelude
import Distribution.Parsec

data DumpBuildInfo
  = NoDumpBuildInfo
  | DumpBuildInfo
  deriving (Read, Show, Eq, Ord, Enum, Bounded, Generic, Typeable)

instance Binary DumpBuildInfo
instance Structured DumpBuildInfo

instance Parsec DumpBuildInfo where
  parsec = parsecDumpBuildInfo

parsecDumpBuildInfo :: CabalParsing m => m DumpBuildInfo
parsecDumpBuildInfo = boolToDumpBuildInfo <$> parsec

boolToDumpBuildInfo :: Bool -> DumpBuildInfo
boolToDumpBuildInfo bool = case bool of
  True -> DumpBuildInfo
  _ -> NoDumpBuildInfo
