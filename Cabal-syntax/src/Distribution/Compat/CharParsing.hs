{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fspec-constr -fspec-constr-count=8 #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Compat.CharParsing
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsers for character streams
--
-- Originally in @parsers@ package.
module Distribution.Compat.CharParsing
  ( -- * Combinators
    oneOf -- :: CharParsing m => [Char] -> m Char
  , noneOf -- :: CharParsing m => [Char] -> m Char
  , spaces -- :: CharParsing m => m ()
  , space -- :: CharParsing m => m Char
  , newline -- :: CharParsing m => m Char
  , tab -- :: CharParsing m => m Char
  , upper -- :: CharParsing m => m Char
  , lower -- :: CharParsing m => m Char
  , alphaNum -- :: CharParsing m => m Char
  , letter -- :: CharParsing m => m Char
  , digit -- :: CharParsing m => m Char
  , hexDigit -- :: CharParsing m => m Char
  , octDigit -- :: CharParsing m => m Char
  , satisfyRange -- :: CharParsing m => Char -> Char -> m Char

    -- * Class
  , CharParsing (..)

    -- * Cabal additions
  , integral
  , signedIntegral
  , munch1
  , munch
  , skipSpaces1
  , module Distribution.Compat.Parsing
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Char
import Data.Text (Text, unpack)

import qualified Text.Parsec as Parsec

import Distribution.Compat.Parsing

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"
oneOf :: CharParsing m => [Char] -> m Char
oneOf xs = satisfy (\c -> c `elem` xs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character is /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOf :: CharParsing m => [Char] -> m Char
noneOf xs = satisfy (\c -> c `notElem` xs)
{-# INLINE noneOf #-}

-- | Skips /zero/ or more white space characters. See also 'skipMany'.
spaces :: CharParsing m => m ()
spaces = skipMany space <?> "white space"
{-# INLINE spaces #-}

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character.
space :: CharParsing m => m Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Parses a newline character (\'\\n\'). Returns a newline character.
newline :: CharParsing m => m Char
newline = char '\n' <?> "new-line"
{-# INLINE newline #-}

-- | Parses a tab character (\'\\t\'). Returns a tab character.
tab :: CharParsing m => m Char
tab = char '\t' <?> "tab"
{-# INLINE tab #-}

-- | Parses an upper case letter. Returns the parsed character.
upper :: CharParsing m => m Char
upper = satisfy isUpper <?> "uppercase letter"
{-# INLINE upper #-}

-- | Parses a lower case character. Returns the parsed character.
lower :: CharParsing m => m Char
lower = satisfy isLower <?> "lowercase letter"
{-# INLINE lower #-}

-- | Parses a letter or digit. Returns the parsed character.
alphaNum :: CharParsing m => m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"
{-# INLINE alphaNum #-}

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character.
letter :: CharParsing m => m Char
letter = satisfy isAlpha <?> "letter"
{-# INLINE letter #-}

-- | Parses a digit. Returns the parsed character.
digit :: CharParsing m => m Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character.
hexDigit :: CharParsing m => m Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"
{-# INLINE hexDigit #-}

-- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
-- the parsed character.
octDigit :: CharParsing m => m Char
octDigit = satisfy isOctDigit <?> "octal digit"
{-# INLINE octDigit #-}

satisfyRange :: CharParsing m => Char -> Char -> m Char
satisfyRange a z = satisfy (\c -> c >= a && c <= z)
{-# INLINE satisfyRange #-}

-- | Additional functionality needed to parse character streams.
class Parsing m => CharParsing m where
  -- | Parse a single character of the input, with UTF-8 decoding
  satisfy :: (Char -> Bool) -> m Char

  -- | @char c@ parses a single character @c@. Returns the parsed
  -- character (i.e. @c@).
  --
  -- /e.g./
  --
  -- @semiColon = 'char' ';'@
  char :: Char -> m Char
  char c = satisfy (c ==) <?> show [c]
  {-# INLINE char #-}

  -- | @notChar c@ parses any single character other than @c@. Returns the parsed
  -- character.
  notChar :: Char -> m Char
  notChar c = satisfy (c /=)
  {-# INLINE notChar #-}

  -- | This parser succeeds for any character. Returns the parsed character.
  anyChar :: m Char
  anyChar = satisfy (const True)
  {-# INLINE anyChar #-}

  -- | @string s@ parses a sequence of characters given by @s@. Returns
  -- the parsed string (i.e. @s@).
  --
  -- >  divOrMod    =   string "div"
  -- >              <|> string "mod"
  string :: String -> m String
  string s = s <$ try (traverse_ char s) <?> show s
  {-# INLINE string #-}

  -- | @text t@ parses a sequence of characters determined by the text @t@ Returns
  -- the parsed text fragment (i.e. @t@).
  --
  -- Using @OverloadedStrings@:
  --
  -- >  divOrMod    =   text "div"
  -- >              <|> text "mod"
  text :: Text -> m Text
  text t = t <$ string (unpack t)
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m) => CharParsing (Lazy.StateT s m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m) => CharParsing (Strict.StateT s m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m) => CharParsing (ReaderT e m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m, Monoid w) => CharParsing (Strict.WriterT w m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m, Monoid w) => CharParsing (Lazy.WriterT w m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m, Monoid w) => CharParsing (Lazy.RWST r w s m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m, Monoid w) => CharParsing (Strict.RWST r w s m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m) => CharParsing (IdentityT m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance Parsec.Stream s m Char => CharParsing (Parsec.ParsecT s u m) where
  satisfy = Parsec.satisfy
  char = Parsec.char
  notChar c = Parsec.satisfy (/= c)
  anyChar = Parsec.anyChar
  string = Parsec.string

-------------------------------------------------------------------------------
-- Our additions
-------------------------------------------------------------------------------

integral :: (CharParsing m, Integral a) => m a
integral = toNumber <$> some d <?> "integral"
  where
    toNumber = foldl' (\a b -> a * 10 + b) 0
    d = f <$> satisfyRange '0' '9'
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f _ = error "panic! integral"
{-# INLINE integral #-}

-- | Accepts negative (starting with @-@) and positive (without sign) integral
-- numbers.
--
-- @since 3.4.0.0
signedIntegral :: (CharParsing m, Integral a) => m a
signedIntegral = negate <$ char '-' <*> integral <|> integral
{-# INLINE signedIntegral #-}

-- | Greedily munch characters while predicate holds.
-- Require at least one character.
munch1 :: CharParsing m => (Char -> Bool) -> m String
munch1 = some . satisfy
{-# INLINE munch1 #-}

-- | Greedily munch characters while predicate holds.
-- Always succeeds.
munch :: CharParsing m => (Char -> Bool) -> m String
munch = many . satisfy
{-# INLINE munch #-}

skipSpaces1 :: CharParsing m => m ()
skipSpaces1 = skipSome space
{-# INLINE skipSpaces1 #-}
