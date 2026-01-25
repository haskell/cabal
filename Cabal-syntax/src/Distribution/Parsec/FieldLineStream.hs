{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Distribution.Parsec.FieldLineStream
  ( FieldLineStream (..)
  , fieldLineStreamFromString
  , fieldLineStreamFromBS
  , fieldLineStreamEnd
  ) where

import Data.ByteString (ByteString)
import Distribution.Compat.Prelude
import Distribution.Utils.Generic (toUTF8BS)
import Prelude ()

import qualified Data.ByteString as BS
import Data.Text.Internal.Encoding.Utf8
import qualified Text.Parsec as Parsec

-- | This is essentially a lazy bytestring, but chunks are glued with newline @\'\\n\'@.
data FieldLineStream
  = FLSLast !ByteString
  | FLSCons {-# UNPACK #-} !ByteString FieldLineStream
  deriving (Show)

fieldLineStreamEnd :: FieldLineStream
fieldLineStreamEnd = FLSLast mempty

-- | Convert 'String' to 'FieldLineStream'.
--
-- /Note:/ inefficient!
fieldLineStreamFromString :: String -> FieldLineStream
fieldLineStreamFromString = FLSLast . toUTF8BS

fieldLineStreamFromBS :: ByteString -> FieldLineStream
fieldLineStreamFromBS = FLSLast

instance Monad m => Parsec.Stream FieldLineStream m Char where
  uncons (FLSLast bs) = return $ case BS.uncons bs of
    Nothing -> Nothing
    Just (c, bs') -> Just (unconsChar c bs' (\bs'' -> FLSLast bs'') fieldLineStreamEnd)
  uncons (FLSCons bs s) = return $ case BS.uncons bs of
    -- as lines are glued with '\n', we return '\n' here!
    Nothing -> Just ('\n', s)
    Just (c, bs') -> Just (unconsChar c bs' (\bs'' -> FLSCons bs'' s) s)

unconsChar :: forall a. Word8 -> ByteString -> (ByteString -> a) -> a -> (Char, a)
unconsChar c0 bs0 f next = go (utf8DecodeStart c0) bs0
  where
    go decoderResult bs = case decoderResult of
      Accept ch -> (ch, f bs)
      Reject -> (replacementChar, f bs)
      Incomplete state codePoint -> case BS.uncons bs of
        Nothing -> (replacementChar, next)
        Just (w, bs') -> go (utf8DecodeContinue w state codePoint) bs'

replacementChar :: Char
replacementChar = '\xfffd'
