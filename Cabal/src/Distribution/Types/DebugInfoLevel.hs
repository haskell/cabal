{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Distribution.Types.DebugInfoLevel
  ( DebugInfoLevel (..)
  , toString
  )
where

import Distribution.Compat.CharParsing (integral)
import Distribution.Compat.Prelude
import Distribution.Parsec (CabalParsing, Parsec (..))
import Prelude ()

import Data.Bool (bool)
import Distribution.Simple.Flag (NoFlagValue (..))

-- ------------------------------------------------------------

-- * Debug info levels

-- ------------------------------------------------------------

-- | Some compilers support emitting debug info. Some have different
-- levels.  For compilers that do not the level is just capped to the
-- level they do support.
data DebugInfoLevel
  = NoDebugInfo
  | MinimalDebugInfo
  | NormalDebugInfo
  | MaximalDebugInfo
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)

instance Binary DebugInfoLevel
instance NFData DebugInfoLevel
instance Structured DebugInfoLevel

instance NoFlagValue DebugInfoLevel where
  noFlagValue :: DebugInfoLevel
  noFlagValue = NoDebugInfo

instance Parsec DebugInfoLevel where
  parsec :: CabalParsing m => m DebugInfoLevel
  parsec = boolParser <|> intParser
    where
      boolParser = bool NoDebugInfo NormalDebugInfo <$> parsec
      intParser = intToDebugInfoLevel <$> integral

instance IsString DebugInfoLevel where
  fromString :: String -> DebugInfoLevel
  fromString s = case reads s of
    [(i, "")] -> intToDebugInfoLevel i
    _ -> error $ "Can't parse debug info level " ++ s

toString :: DebugInfoLevel -> String
toString = show . fromEnum

intToDebugInfoLevel :: Int -> DebugInfoLevel
intToDebugInfoLevel i
  | i >= minLevel && i <= maxLevel = toEnum i
  | otherwise =
      error $
        "Bad debug info level: "
          ++ show i
          ++ ". Valid values are "
          ++ show minLevel
          ++ ".."
          ++ show maxLevel
  where
    minLevel = fromEnum (minBound :: DebugInfoLevel)
    maxLevel = fromEnum (maxBound :: DebugInfoLevel)
