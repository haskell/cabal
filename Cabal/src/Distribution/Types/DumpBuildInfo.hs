{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Types.DumpBuildInfo
  ( DumpBuildInfo (..)
  , toString
  ) where

import Distribution.Compat.Prelude (Binary, Generic, NFData, Structured)
import Distribution.Parsec (CabalParsing, Parsec (..))
import Distribution.Simple.Flag (NoFlagValue (..))

data DumpBuildInfo
  = NoDumpBuildInfo
  | DumpBuildInfo
  deriving stock (Read, Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary DumpBuildInfo
instance NFData DumpBuildInfo
instance Structured DumpBuildInfo

instance NoFlagValue DumpBuildInfo where
  noFlagValue :: DumpBuildInfo
  noFlagValue = NoDumpBuildInfo

instance Parsec DumpBuildInfo where
  parsec :: CabalParsing m => m DumpBuildInfo
  parsec = boolToDumpBuildInfo <$> parsec

boolToDumpBuildInfo :: Bool -> DumpBuildInfo
boolToDumpBuildInfo bool = if bool then DumpBuildInfo else NoDumpBuildInfo

toString :: DumpBuildInfo -> String
toString = \case
  NoDumpBuildInfo -> "False"
  DumpBuildInfo -> "True"
