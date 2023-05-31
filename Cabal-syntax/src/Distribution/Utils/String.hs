module Distribution.Utils.String
  ( -- * Encode to/from UTF8
    decodeStringUtf8
  , encodeStringUtf8
  , trim
  ) where

import Data.Bits
import Data.Char (chr, ord)
import Data.List (dropWhileEnd)
import Data.Word
import GHC.Unicode (isSpace)

-- | Decode 'String' from UTF8-encoded octets.
--
-- Invalid data in the UTF8 stream (this includes code-points @U+D800@
-- through @U+DFFF@) will be decoded as the replacement character (@U+FFFD@).
--
-- See also 'encodeStringUtf8'
decodeStringUtf8 :: [Word8] -> String
decodeStringUtf8 = go
  where
    go :: [Word8] -> String
    go [] = []
    go (c : cs)
      | c <= 0x7F = chr (fromIntegral c) : go cs
      | c <= 0xBF = replacementChar : go cs
      | c <= 0xDF = twoBytes c cs
      | c <= 0xEF = moreBytes 3 0x800 cs (fromIntegral $ c .&. 0xF)
      | c <= 0xF7 = moreBytes 4 0x10000 cs (fromIntegral $ c .&. 0x7)
      | c <= 0xFB = moreBytes 5 0x200000 cs (fromIntegral $ c .&. 0x3)
      | c <= 0xFD = moreBytes 6 0x4000000 cs (fromIntegral $ c .&. 0x1)
      | otherwise = replacementChar : go cs

    twoBytes :: Word8 -> [Word8] -> String
    twoBytes c0 (c1 : cs')
      | c1 .&. 0xC0 == 0x80 =
          let d =
                (fromIntegral (c0 .&. 0x1F) `shiftL` 6)
                  .|. fromIntegral (c1 .&. 0x3F)
           in if d >= 0x80
                then chr d : go cs'
                else replacementChar : go cs'
    twoBytes _ cs' = replacementChar : go cs'

    moreBytes :: Int -> Int -> [Word8] -> Int -> [Char]
    moreBytes 1 overlong cs' acc
      | overlong <= acc
      , acc <= 0x10FFFF
      , acc < 0xD800 || 0xDFFF < acc =
          chr acc : go cs'
      | otherwise =
          replacementChar : go cs'
    moreBytes byteCount overlong (cn : cs') acc
      | cn .&. 0xC0 == 0x80 =
          moreBytes
            (byteCount - 1)
            overlong
            cs'
            ((acc `shiftL` 6) .|. fromIntegral cn .&. 0x3F)
    moreBytes _ _ cs' _ =
      replacementChar : go cs'

    replacementChar = '\xfffd'

-- | Encode 'String' to a list of UTF8-encoded octets
--
-- Code-points in the @U+D800@-@U+DFFF@ range will be encoded
-- as the replacement character (i.e. @U+FFFD@).
--
-- See also 'decodeUtf8'
encodeStringUtf8 :: String -> [Word8]
encodeStringUtf8 [] = []
encodeStringUtf8 (c : cs)
  | c <= '\x07F' =
      w8
        : encodeStringUtf8 cs
  | c <= '\x7FF' =
      (0xC0 .|. w8ShiftR 6)
        : (0x80 .|. (w8 .&. 0x3F))
        : encodeStringUtf8 cs
  | c <= '\xD7FF' =
      (0xE0 .|. w8ShiftR 12)
        : (0x80 .|. (w8ShiftR 6 .&. 0x3F))
        : (0x80 .|. (w8 .&. 0x3F))
        : encodeStringUtf8 cs
  | c <= '\xDFFF' =
      0xEF
        : 0xBF
        : 0xBD -- U+FFFD
        : encodeStringUtf8 cs
  | c <= '\xFFFF' =
      (0xE0 .|. w8ShiftR 12)
        : (0x80 .|. (w8ShiftR 6 .&. 0x3F))
        : (0x80 .|. (w8 .&. 0x3F))
        : encodeStringUtf8 cs
  | otherwise =
      (0xf0 .|. w8ShiftR 18)
        : (0x80 .|. (w8ShiftR 12 .&. 0x3F))
        : (0x80 .|. (w8ShiftR 6 .&. 0x3F))
        : (0x80 .|. (w8 .&. 0x3F))
        : encodeStringUtf8 cs
  where
    w8 = fromIntegral (ord c) :: Word8
    w8ShiftR :: Int -> Word8
    w8ShiftR = fromIntegral . shiftR (ord c)

-- @since 3.8.0.0
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
