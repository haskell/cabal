module Distribution.Annotation where

-- | Designates the source position within a joined 'FieldLineStream'.
--   Inclusive in the start position, exclusive in the ending position.
data LocalSrcSpan = LocalSrcSpan {-# UNPACK #-} !RelPosition {-# UNPACK #-} !RelPosition
  deriving (Show)
