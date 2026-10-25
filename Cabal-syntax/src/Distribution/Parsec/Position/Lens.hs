module Distribution.Parsec.Position.Lens
  ( HasPosition (..)
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Position (Position (Position))

class HasPosition a where
  position :: Lens' a Position

  positionCol :: Lens' a Int
  positionCol = position . col
    where
      col f (Position r c) = Position r <$> f c
  {-# INLINE positionCol #-}

  positionRow :: Lens' a Int
  positionRow = position . row
    where
      row f (Position r c) = flip Position c <$> f r
  {-# INLINE positionRow #-}

instance HasPosition Position where
  position = id
