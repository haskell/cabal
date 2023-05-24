{-# LANGUAGE DeriveGeneric #-}

module Distribution.Parsec.Position
  ( Position (..)
  , incPos
  , retPos
  , showPos
  , zeroPos
  , positionCol
  , positionRow
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- | 1-indexed row and column positions in a file.
data Position
  = Position
      {-# UNPACK #-} !Int -- row
      {-# UNPACK #-} !Int -- column
  deriving (Eq, Ord, Show, Generic)

instance Binary Position
instance NFData Position where rnf = genericRnf

-- | Shift position by n columns to the right.
incPos :: Int -> Position -> Position
incPos n (Position row col) = Position row (col + n)

-- | Shift position to beginning of next row.
retPos :: Position -> Position
retPos (Position row _col) = Position (row + 1) 1

showPos :: Position -> String
showPos (Position row col) = show row ++ ":" ++ show col

zeroPos :: Position
zeroPos = Position 0 0

-- | @since 3.0.0.0
positionCol :: Position -> Int
positionCol (Position _ c) = c

-- | @since 3.0.0.0
positionRow :: Position -> Int
positionRow (Position r _) = r
