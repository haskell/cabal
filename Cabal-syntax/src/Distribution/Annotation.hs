{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}

module Distribution.Annotation where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Distribution.Parsec
import Distribution.Parsec.Position

-- | Designates the source position within a joined 'FieldLineStream'.
--   Inclusive in the start position, exclusive in the ending position.
data SrcSpan = SrcSpan {-# UNPACK #-} !Position {-# UNPACK #-} !Position
  deriving (Show, Eq, Ord)

data Located a = MkLocated {getSrcSpan :: !SrcSpan, unLocated :: !a}
  deriving (Show, Functor, Foldable, Traversable)

sortBySrcSpanAsc :: [Located a] -> [Located a]
sortBySrcSpanAsc = sortOn getSrcSpan

sortBySrcSpanDes :: [Located a] -> [Located a]
sortBySrcSpanDes = sortOn (Down . getSrcSpan)

instance Parsec a => Parsec (Located a) where
  parsec = do
    begin <- getPosition
    x <- parsec
    end <- getPosition
    pure (MkLocated (SrcSpan begin end) x)
