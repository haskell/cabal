{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Parsec
  ( Parsec (..)
  , ParsecParser (..)
  , runParsecParser
  , runParsecParser'
  , simpleParsec
  , simpleParsecBS
  , simpleParsec'
  , simpleParsecW'
  , lexemeParsec
  , eitherParsec
  , explicitEitherParsec
  , explicitEitherParsec'

    -- * CabalParsing and diagnostics
  , CabalParsing (..)
  , PPUserState (..)
  , emptyPPUserState

    -- ** Warnings
  , PWarnType (..)
  , PWarning (..)
  , PWarningWithSource (..)
  , PSource (..)
  , showPWarning
  , showPWarningWithSource

    -- ** Errors
  , PError (..)
  , PErrorWithSource (..)
  , showPError
  , showPErrorWithSource

    -- * Position
  , Position (..)
  , incPos
  , retPos
  , showPos
  , zeroPos

    -- * Utilities
  , parsecToken
  , parsecToken'
  , parsecFilePath
  , parsecQuoted
  , parsecMaybeQuoted
  , parsecCommaList
  , parsecCommaNonEmpty
  , parsecLeadingCommaList
  , parsecLeadingCommaNonEmpty
  , parsecOptCommaList
  , parsecLeadingOptCommaList
  , parsecStandard
  , parsecUnqualComponentName
  ) where

import Data.ByteString (ByteString)
import Data.Char (digitToInt, intToDigit)
import Data.List (transpose)
import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Distribution.Parsec.Error (PError (..), PErrorWithSource (..), showPError, showPErrorWithSource)

import Data.Monoid (Last (..))
import Distribution.Parsec.FieldLineStream (FieldLineStream, fieldLineStreamFromBS, fieldLineStreamFromString)
import Distribution.Parsec.Position (Position (..), incPos, retPos, showPos, zeroPos)
import Distribution.Parsec.Warning
import Numeric (showIntAtBase)
import Prelude ()

import Distribution.ParsecParser
import Distribution.CabalParsing
import Distribution.PPUserState

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Distribution.Compat.MonadFail as Fail
import qualified Text.Parsec as Parsec

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class for parsing with @parsec@. Mainly used for @.cabal@ file fields.
--
-- For parsing @.cabal@ like file structure, see "Distribution.Fields".
class Parsec a where
  parsec :: CabalParsing m => m a

-- | 'parsec' /could/ consume trailing spaces, this function /will/ consume.
lexemeParsec :: (CabalParsing m, Parsec a) => m a
lexemeParsec = parsec <* P.spaces

-- | Parse a 'String' with 'lexemeParsec'.
simpleParsec :: Parsec a => String -> Maybe a
simpleParsec =
  either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromString

-- | Like 'simpleParsec' but for 'ByteString'
simpleParsecBS :: Parsec a => ByteString -> Maybe a
simpleParsecBS =
  either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromBS

-- | Parse a 'String' with 'lexemeParsec' using specific 'CabalSpecVersion'.
--
-- @since 3.4.0.0
simpleParsec' :: Parsec a => CabalSpecVersion -> String -> Maybe a
simpleParsec' spec =
  either (const Nothing) Just
    . runParsecParser' spec lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec' using specific 'CabalSpecVersion'.
-- Fail if there are any warnings.
--
-- @since 3.4.0.0
simpleParsecW' :: Parsec a => CabalSpecVersion -> String -> Maybe a
simpleParsecW' spec =
  either (const Nothing) (\(x, PPUserState ws _) -> if null ws then Just x else Nothing)
    . runParsecParser' spec ((,) <$> lexemeParsec <*> liftParsec Parsec.getState) "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec'.
eitherParsec :: Parsec a => String -> Either String a
eitherParsec = explicitEitherParsec parsec

instance Parsec a => Parsec (Identity a) where
  parsec = Identity <$> parsec

instance Parsec Bool where
  parsec = P.munch1 isAlpha >>= postprocess
    where
      postprocess str
        | str == "True" = pure True
        | str == "False" = pure False
        | lstr == "true" = parsecWarning PWTBoolCase caseWarning *> pure True
        | lstr == "false" = parsecWarning PWTBoolCase caseWarning *> pure False
        | otherwise = fail $ "Not a boolean: " ++ str
        where
          lstr = map toLower str
          caseWarning =
            "Boolean values are case sensitive, use 'True' or 'False'."

instance Parsec a => Parsec (Last a) where
  parsec = parsecLast

parsecLast :: (Parsec a, CabalParsing m) => m (Last a)
parsecLast = (Last . Just <$> parsec) <|> pure mempty

-- | @[^ ,]@
parsecToken :: CabalParsing m => m String
parsecToken = parsecHaskellString <|> ((P.munch1 (\x -> not (isSpace x) && x /= ',') P.<?> "identifier") >>= checkNotDoubleDash)

-- | @[^ ]@
parsecToken' :: CabalParsing m => m String
parsecToken' = parsecHaskellString <|> ((P.munch1 (not . isSpace) P.<?> "token") >>= checkNotDoubleDash)

checkNotDoubleDash :: CabalParsing m => String -> m String
checkNotDoubleDash s = do
  when (s == "--") $
    parsecWarning PWTDoubleDash $
      unwords
        [ "Double-dash token found."
        , "Note: there are no end-of-line comments in .cabal files, only whole line comments."
        , "Use \"--\" (quoted double dash) to silence this warning, if you actually want -- token"
        ]

  return s

parsecFilePath :: CabalParsing m => m FilePath
parsecFilePath = parsecToken

-- | Parse a benchmark/test-suite types.
parsecStandard :: (CabalParsing m, Parsec ver) => (ver -> String -> a) -> m a
parsecStandard f = do
  cs <- some $ P.try (component <* P.char '-')
  ver <- parsec
  let name = map toLower (intercalate "-" cs)
  return $! f ver name
  where
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs then fail "all digit component" else return cs

-- each component must contain an alphabetic character, to avoid
-- ambiguity in identifiers like foo-1 (the 1 is the version number).

parsecCommaList :: CabalParsing m => m a -> m [a]
parsecCommaList p = P.sepBy (p <* P.spaces) (P.char ',' *> P.spaces P.<?> "comma")

parsecCommaNonEmpty :: CabalParsing m => m a -> m (NonEmpty a)
parsecCommaNonEmpty p = P.sepByNonEmpty (p <* P.spaces) (P.char ',' *> P.spaces P.<?> "comma")

-- | Like 'parsecCommaList' but accept leading or trailing comma.
--
-- @
-- p (comma p)*  -- p `sepBy` comma
-- (comma p)*    -- leading comma
-- (p comma)*    -- trailing comma
-- @
parsecLeadingCommaList :: CabalParsing m => m a -> m [a]
parsecLeadingCommaList p = do
  c <- P.optional comma
  case c of
    Nothing -> toList <$> P.sepEndByNonEmpty lp comma <|> pure []
    Just _ -> toList <$> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

-- |
--
-- @since 3.4.0.0
parsecLeadingCommaNonEmpty :: CabalParsing m => m a -> m (NonEmpty a)
parsecLeadingCommaNonEmpty p = do
  c <- P.optional comma
  case c of
    Nothing -> P.sepEndByNonEmpty lp comma
    Just _ -> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

parsecOptCommaList :: CabalParsing m => m a -> m [a]
parsecOptCommaList p = P.sepBy (p <* P.spaces) (P.optional comma)
  where
    comma = P.char ',' *> P.spaces

-- | Like 'parsecOptCommaList' but
--
-- * require all or none commas
-- * accept leading or trailing comma.
--
-- @
-- p (comma p)*  -- p `sepBy` comma
-- (comma p)*    -- leading comma
-- (p comma)*    -- trailing comma
-- p*            -- no commas: many p
-- @
--
-- @since 3.0.0.0
parsecLeadingOptCommaList :: CabalParsing m => m a -> m [a]
parsecLeadingOptCommaList p = do
  c <- P.optional comma
  case c of
    Nothing -> sepEndBy1Start <|> pure []
    Just _ -> toList <$> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

    sepEndBy1Start = do
      x <- lp
      c <- P.optional comma
      case c of
        Nothing -> (x :) <$> many lp
        Just _ -> (x :) <$> P.sepEndBy lp comma

-- | Content isn't unquoted
parsecQuoted :: CabalParsing m => m a -> m a
parsecQuoted = P.between (P.char '"') (P.char '"')

-- | @parsecMaybeQuoted p = 'parsecQuoted' p <|> p@.
parsecMaybeQuoted :: CabalParsing m => m a -> m a
parsecMaybeQuoted p = parsecQuoted p <|> p

parsecUnqualComponentName :: forall m. CabalParsing m => m String
parsecUnqualComponentName = state0 DList.empty
  where
    --
    -- using @kleene@ package we can easily see that
    -- we need only two states to recognize
    -- unqual-component-name
    --
    -- Compare with declarative
    -- 'Distribution.FieldGrammar.Described.reUnqualComponent'.
    --
    -- @
    -- import Kleene
    -- import Kleene.Internal.Pretty
    -- import Algebra.Lattice
    -- import Data.Char
    --
    -- import qualified Data.RangeSet.Map as RSet
    --
    -- main = do
    --     -- this is an approximation, to get an idea.
    --     let component :: RE Char
    --         component = star alphaNum <> alpha <> star alphaNum
    --
    --         alphaNum = alpha \/ num
    --         alpha    = unions $ map char ['a'..'z']
    --         num      = unions $ map char ['0'..'9']
    --
    --         re :: RE Char
    --         re = component <> star (char '-' <> component)
    --
    --     putPretty re
    --     putPretty $ fromTM re
    -- @

    state0 :: DList.DList Char -> m String
    state0 acc = do
      c <- ch -- <|> fail ("Invalid component, after " ++ DList.toList acc)
      case () of
        _
          | isDigit c -> state0 (DList.snoc acc c)
          | isAlphaNum c -> state1 (DList.snoc acc c)
          | c == '-' -> fail ("Empty component, after " ++ DList.toList acc)
          | otherwise -> fail ("Internal error, after " ++ DList.toList acc)

    state1 :: DList.DList Char -> m String
    state1 acc = state1' acc `alt` return (DList.toList acc)

    state1' :: DList.DList Char -> m String
    state1' acc = do
      c <- ch
      case () of
        _
          | isAlphaNum c -> state1 (DList.snoc acc c)
          | c == '-' -> state0 (DList.snoc acc c)
          | otherwise -> fail ("Internal error, after " ++ DList.toList acc)

    ch :: m Char
    !ch = P.satisfy (\c -> isAlphaNum c || c == '-')

    alt :: m String -> m String -> m String
    !alt = (<|>)


