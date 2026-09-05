{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Fields.Field.Lens where

import Distribution.Compat.Lens
import qualified Distribution.Fields.Field as T
import Distribution.Parsec.Position.Lens

justComments :: Lens' (T.WithComments ann) [T.Comment ann]
justComments f s = fmap (\x -> s{T.justComments = x}) (f (T.justComments s))
{-# INLINE justComments #-}

unComments :: Lens' (T.WithComments ann) ann
unComments f s = fmap (\x -> s{T.unComments = x}) (f (T.unComments s))
{-# INLINE unComments #-}

instance HasPosition ann => HasPosition (T.WithComments ann) where
  position = unComments . position

instance HasPosition ann => HasPosition (T.Comment ann) where
  position f (T.Comment bs ann) = T.Comment bs <$> position f ann
