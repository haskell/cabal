{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Distribution.Types.OptimisationLevel
  ( OptimisationLevel (..)
  , toString
  )
where

import Distribution.Compat.CharParsing (integral)
import Distribution.Compat.Prelude
import Distribution.Parsec (CabalParsing, Parsec (..))
import Prelude ()

import Data.Bool (bool)

-- | Some compilers support optimising. Some have different levels.
-- For compilers that do not the level is just capped to the level
-- they do support.
data OptimisationLevel
  = NoOptimisation
  | NormalOptimisation
  | MaximumOptimisation
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (Binary, NFData, Structured)

instance Parsec OptimisationLevel where
  parsec :: CabalParsing m => m OptimisationLevel
  parsec = boolParser <|> intParser
    where
      boolParser = bool NoOptimisation NormalOptimisation <$> parsec
      intParser = intToOptimisationLevel <$> integral

instance IsString OptimisationLevel where
  fromString :: String -> OptimisationLevel
  fromString s = case reads s of
    [(i, "")] -> intToOptimisationLevel i
    _ -> error $ "Can't parse optimisation level " ++ s

toString :: OptimisationLevel -> String
toString = show . fromEnum

intToOptimisationLevel :: Int -> OptimisationLevel
intToOptimisationLevel i
  | i >= minLevel && i <= maxLevel = toEnum i
  | otherwise =
      error $
        "Bad optimisation level: "
          ++ show i
          ++ ". Valid values are "
          ++ show minLevel
          ++ ".."
          ++ show maxLevel
  where
    minLevel = fromEnum (minBound :: OptimisationLevel)
    maxLevel = fromEnum (maxBound :: OptimisationLevel)
