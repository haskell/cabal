module Distribution.Types.Version.Parsec where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty

import Distribution.Types.Version.Internal

import qualified Data.Version as Base
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp
import qualified Text.Read as Read

instance Parsec Version where
  parsec = mkVersion <$> toList <$> P.sepByNonEmpty versionDigitParser (P.char '.') <* tags
    where
      tags = do
        ts <- many $ P.char '-' *> some (P.satisfy isAlphaNum)
        case ts of
          [] -> pure ()
          (_ : _) -> parsecWarning PWTVersionTag "version with tags"

-- | An integral without leading zeroes.
--
-- @since 3.0
versionDigitParser :: CabalParsing m => m Int
versionDigitParser = (some d >>= toNumber) P.<?> "version digit (integral without leading zeroes)"
  where
    toNumber :: CabalParsing m => [Int] -> m Int
    toNumber [0] = return 0
    toNumber (0 : _) = P.unexpected "Version digit with leading zero"
    toNumber xs
      -- 10^9 = 1000000000
      -- 2^30 = 1073741824
      --
      -- GHC Int is at least 32 bits, so 2^31-1 is the 'maxBound'.
      | length xs > 9 = P.unexpected "At most 9 numbers are allowed per version number part"
      | otherwise = return $ foldl' (\a b -> a * 10 + b) 0 xs

    d :: P.CharParsing m => m Int
    d = f <$> P.satisfyRange '0' '9'
    f c = ord c - ord '0'

