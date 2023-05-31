{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Distribution.Parsec.FieldLineStream
  ( FieldLineStream (..)
  , fieldLineStreamFromString
  , fieldLineStreamFromBS
  , fieldLineStreamEnd
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Distribution.Compat.Prelude
import Distribution.Utils.Generic (toUTF8BS)
import Prelude ()

import qualified Data.ByteString as BS
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

-- Based on implementation 'decodeStringUtf8'
unconsChar :: forall a. Word8 -> ByteString -> (ByteString -> a) -> a -> (Char, a)
unconsChar c0 bs0 f next
  | c0 <= 0x7F = (chr (fromIntegral c0), f bs0)
  | c0 <= 0xBF = (replacementChar, f bs0)
  | c0 <= 0xDF = twoBytes
  | c0 <= 0xEF = moreBytes 3 0x800 bs0 (fromIntegral $ c0 .&. 0xF)
  | c0 <= 0xF7 = moreBytes 4 0x10000 bs0 (fromIntegral $ c0 .&. 0x7)
  | c0 <= 0xFB = moreBytes 5 0x200000 bs0 (fromIntegral $ c0 .&. 0x3)
  | c0 <= 0xFD = moreBytes 6 0x4000000 bs0 (fromIntegral $ c0 .&. 0x1)
  | otherwise = error $ "not implemented " ++ show c0
  where
    twoBytes = case BS.uncons bs0 of
      Nothing -> (replacementChar, next)
      Just (c1, bs1)
        | c1 .&. 0xC0 == 0x80 ->
            if d >= 0x80
              then (chr d, f bs1)
              else (replacementChar, f bs1)
        | otherwise -> (replacementChar, f bs1)
        where
          d = (fromIntegral (c0 .&. 0x1F) `shiftL` 6) .|. fromIntegral (c1 .&. 0x3F)

    moreBytes :: Int -> Int -> ByteString -> Int -> (Char, a)
    moreBytes 1 overlong bs' acc
      | overlong <= acc
      , acc <= 0x10FFFF
      , acc < 0xD800 || 0xDFFF < acc =
          (chr acc, f bs')
      | otherwise =
          (replacementChar, f bs')
    moreBytes byteCount overlong bs' acc = case BS.uncons bs' of
      Nothing -> (replacementChar, f bs')
      Just (cn, bs1)
        | cn .&. 0xC0 == 0x80 ->
            moreBytes
              (byteCount - 1)
              overlong
              bs1
              ((acc `shiftL` 6) .|. fromIntegral cn .&. 0x3F)
        | otherwise -> (replacementChar, f bs1)

replacementChar :: Char
replacementChar = '\xfffd'
