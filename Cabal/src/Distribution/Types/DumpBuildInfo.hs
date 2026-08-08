{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Types.DumpBuildInfo
  ( DumpBuildInfo (..)
  , toString
  ) where

import Distribution.Compat.Prelude (Binary, Generic, NFData, Structured)
import Distribution.Parsec (CabalParsing, Parsec (..))

data DumpBuildInfo
  = NoDumpBuildInfo
  | DumpBuildInfo
  deriving stock (Read, Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Binary, NFData, Structured)

instance Parsec DumpBuildInfo where
  parsec :: CabalParsing m => m DumpBuildInfo
  parsec = boolToDumpBuildInfo <$> parsec

boolToDumpBuildInfo :: Bool -> DumpBuildInfo
boolToDumpBuildInfo bool = if bool then DumpBuildInfo else NoDumpBuildInfo

toString :: DumpBuildInfo -> String
toString = \case
  NoDumpBuildInfo -> "False"
  DumpBuildInfo -> "True"
