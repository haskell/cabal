{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.CabalParsing where

import Data.Char (digitToInt, intToDigit)
import Data.List (transpose)
import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude

import Distribution.Parsec.Warning
import Numeric (showIntAtBase)
import Prelude ()

import Distribution.Types.AnnotationTrivium
import Distribution.Types.AnnotationNamespace

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.MonadFail as Fail

-- | Parsing class which
--
-- * can report Cabal parser warnings.
--
-- * knows @cabal-version@ we work with
--
-- * can add trivia annotations
class (P.CharParsing m, MonadPlus m, Fail.MonadFail m) => CabalParsing m where
  -- type XXX m

  parsecWarning :: PWarnType -> String -> m ()

  parsecHaskellString :: m String
  parsecHaskellString = stringLiteral

  askCabalSpecVersion :: m CabalSpecVersion

  -- annotate :: XXX m -> Trivium -> m ()
  annotate :: Namespace -> Trivium -> m ()

stringLiteral :: forall m. P.CharParsing m => m String
stringLiteral = lit
  where
    lit :: m String
    lit =
      foldr (maybe id (:)) ""
        <$> P.between (P.char '"') (P.char '"' P.<?> "end of string") (many stringChar)
        P.<?> "string"

    stringChar :: m (Maybe Char)
    stringChar =
      Just <$> stringLetter
        <|> stringEscape
        P.<?> "string character"

    stringLetter :: m Char
    stringLetter = P.satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape :: m (Maybe Char)
    stringEscape = P.char '\\' *> esc
      where
        esc :: m (Maybe Char)
        esc =
          Nothing <$ escapeGap
            <|> Nothing <$ escapeEmpty
            <|> Just <$> escapeCode

    escapeEmpty, escapeGap :: m Char
    escapeEmpty = P.char '&'
    escapeGap = P.skipSpaces1 *> (P.char '\\' P.<?> "end of string gap")

escapeCode :: forall m. P.CharParsing m => m Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) P.<?> "escape code"
  where
    charControl, charNum :: m Char
    charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (P.char '^' *> (P.upper <|> P.char '@'))
    charNum = toEnum <$> num
      where
        num :: m Int
        num =
          bounded 10 maxchar
            <|> (P.char 'o' *> bounded 8 maxchar)
            <|> (P.char 'x' *> bounded 16 maxchar)
        maxchar = fromEnum (maxBound :: Char)

    bounded :: Int -> Int -> m Int
    bounded base bnd =
      foldl' (\x d -> base * x + digitToInt d) 0
        <$> bounded' (take base thedigits) (map digitToInt $ showIntAtBase base intToDigit bnd "")
      where
        thedigits :: [m Char]
        thedigits = map P.char ['0' .. '9'] ++ map P.oneOf (transpose [['A' .. 'F'], ['a' .. 'f']])

        toomuch :: m a
        toomuch = P.unexpected "out-of-range numeric escape sequence"

        bounded', bounded'' :: [m Char] -> [Int] -> m [Char]
        bounded' dps@(zero : _) bds =
          P.skipSome zero *> ([] <$ P.notFollowedBy (P.choice dps) <|> bounded'' dps bds)
            <|> bounded'' dps bds
        bounded' [] _ = error "bounded called with base 0"
        bounded'' dps [] = [] <$ P.notFollowedBy (P.choice dps) <|> toomuch
        bounded'' dps (bd : bds) =
          let anyd :: m Char
              anyd = P.choice dps

              nomore :: m ()
              nomore = P.notFollowedBy anyd <|> toomuch

              (low, ex, high) = case splitAt bd dps of
                (low', ex' : high') -> (low', ex', high')
                (_, _) -> error "escapeCode: Logic error"
           in ((:) <$> P.choice low <*> atMost (length bds) anyd) <* nomore
                <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
                <|> if not (null bds)
                  then (:) <$> P.choice high <*> atMost (length bds - 1) anyd <* nomore
                  else empty
        atMost n p
          | n <= 0 = pure []
          | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []

    charEsc :: m Char
    charEsc = P.choice $ parseEsc <$> escMap

    parseEsc (c, code) = code <$ P.char c
    escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

    charAscii :: m Char
    charAscii = P.choice $ parseAscii <$> asciiMap

    parseAscii (asc, code) = P.try $ code <$ P.string asc
    asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
    ascii2codes, ascii3codes :: [String]
    ascii2codes =
      [ "BS"
      , "HT"
      , "LF"
      , "VT"
      , "FF"
      , "CR"
      , "SO"
      , "SI"
      , "EM"
      , "FS"
      , "GS"
      , "RS"
      , "US"
      , "SP"
      ]
    ascii3codes =
      [ "NUL"
      , "SOH"
      , "STX"
      , "ETX"
      , "EOT"
      , "ENQ"
      , "ACK"
      , "BEL"
      , "DLE"
      , "DC1"
      , "DC2"
      , "DC3"
      , "DC4"
      , "NAK"
      , "SYN"
      , "ETB"
      , "CAN"
      , "SUB"
      , "ESC"
      , "DEL"
      ]
    ascii2, ascii3 :: String
    ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
    ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"
