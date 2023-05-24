{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- @since 3.0
module Distribution.Types.PkgconfigVersion
  ( PkgconfigVersion (..)
  , rpmvercmp
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic (isAsciiAlphaNum)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

-- | @pkg-config@ versions.
--
-- In fact, this can be arbitrary 'BS.ByteString',
-- but 'Parsec' instance is a little pickier.
--
-- @since 3.0
newtype PkgconfigVersion = PkgconfigVersion BS.ByteString
  deriving (Generic, Read, Show, Typeable, Data)

instance Eq PkgconfigVersion where
  PkgconfigVersion a == PkgconfigVersion b = rpmvercmp a b == EQ

instance Ord PkgconfigVersion where
  PkgconfigVersion a `compare` PkgconfigVersion b = rpmvercmp a b

instance Binary PkgconfigVersion
instance Structured PkgconfigVersion
instance NFData PkgconfigVersion where rnf = genericRnf

instance Pretty PkgconfigVersion where
  pretty (PkgconfigVersion bs) = PP.text (BS8.unpack bs)

-- |
--
-- >>> simpleParsec "1.0.2n" :: Maybe PkgconfigVersion
-- Just (PkgconfigVersion "1.0.2n")
--
-- >>> simpleParsec "0.3.5+ds" :: Maybe PkgconfigVersion
-- Nothing
instance Parsec PkgconfigVersion where
  parsec = PkgconfigVersion . BS8.pack <$> P.munch1 predicate
    where
      predicate c = isAsciiAlphaNum c || c == '.' || c == '-'

-------------------------------------------------------------------------------
-- rpmvercmp - pure Haskell implementation
-------------------------------------------------------------------------------

-- | Compare two version strings as @pkg-config@ would compare them.
--
-- @since 3.0
rpmvercmp :: BS.ByteString -> BS.ByteString -> Ordering
rpmvercmp a b = go0 (BS.unpack a) (BS.unpack b)
  where
    go0 :: [Word8] -> [Word8] -> Ordering
    -- if there is _any_ trailing "garbage", it seems to affect result
    -- https://github.com/haskell/cabal/issues/6805
    go0 [] [] = EQ
    go0 [] _ = LT
    go0 _ [] = GT
    go0 xs ys = go1 (dropNonAlnum8 xs) (dropNonAlnum8 ys)

    go1 :: [Word8] -> [Word8] -> Ordering
    go1 [] [] = EQ
    go1 [] _ = LT
    go1 _ [] = GT
    go1 xs@(x : _) ys
      | isDigit8 x =
          let (xs1, xs2) = span isDigit8 xs
              (ys1, ys2) = span isDigit8 ys
           in -- numeric segments are always newer than alpha segments
              if null ys1
                then GT
                else compareInt xs1 ys1 <> go0 xs2 ys2
      -- isAlpha
      | otherwise =
          let (xs1, xs2) = span isAlpha8 xs
              (ys1, ys2) = span isAlpha8 ys
           in if null ys1
                then LT
                else compareStr xs1 ys1 <> go0 xs2 ys2

-- compare as numbers
compareInt :: [Word8] -> [Word8] -> Ordering
compareInt xs ys =
  -- whichever number has more digits wins
  compare (length xs') (length ys')
    <>
    -- equal length: use per character compare, "strcmp"
    compare xs' ys'
  where
    -- drop  leading zeros
    xs' = dropWhile (== 0x30) xs
    ys' = dropWhile (== 0x30) ys

-- strcmp
compareStr :: [Word8] -> [Word8] -> Ordering
compareStr = compare

dropNonAlnum8 :: [Word8] -> [Word8]
dropNonAlnum8 = dropWhile (\w -> not (isDigit8 w || isAlpha8 w))

isDigit8 :: Word8 -> Bool
isDigit8 w = 0x30 <= w && w <= 0x39

isAlpha8 :: Word8 -> Bool
isAlpha8 w = (0x41 <= w && w <= 0x5A) || (0x61 <= w && w <= 0x7A)
