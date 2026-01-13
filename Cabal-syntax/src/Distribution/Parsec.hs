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
import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Distribution.Parsec.Error (PError (..), PErrorWithSource (..), showPError, showPErrorWithSource)

import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS, fieldLineStreamFromString)
import Distribution.Parsec.Position (Position (..), incPos, retPos, showPos, zeroPos)
import Distribution.Parsec.Warning
import Prelude ()

import Distribution.Parsec.Class
import Distribution.ParsecParser
import Distribution.CabalParsing

import qualified Distribution.Compat.CharParsing as P
import qualified Text.Parsec as Parsec

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
  either (const Nothing) (\(x, ws) -> if null ws then Just x else Nothing)
    . runParsecParser' spec ((,) <$> lexemeParsec <*> liftParsec Parsec.getState) "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec'.
eitherParsec :: Parsec a => String -> Either String a
eitherParsec = explicitEitherParsec parsec
