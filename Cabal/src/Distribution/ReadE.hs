{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.ReadE
-- Copyright   :  Jose Iborra 2008
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Simple parsing with failure
module Distribution.ReadE
  ( -- * ReadE
    ReadE (..)
  , succeedReadE
  , failReadE

    -- * Projections
  , parsecToReadE
  , parsecToReadEErr

    -- * Parse Errors
  , unexpectMsgString
  ) where

import qualified Data.Bifunctor as Bi (first)
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Parsec.FieldLineStream
import qualified Text.Parsec.Error as Parsec

-- | Parser with simple error reporting
newtype ReadE a = ReadE {runReadE :: String -> Either ErrorMsg a}

type ErrorMsg = String

instance Functor ReadE where
  fmap f (ReadE p) = ReadE $ \txt -> case p txt of
    Right a -> Right (f a)
    Left err -> Left err

succeedReadE :: (String -> a) -> ReadE a
succeedReadE f = ReadE (Right . f)

failReadE :: ErrorMsg -> ReadE a
failReadE = ReadE . const . Left

runParsecFromString :: ParsecParser a -> String -> Either Parsec.ParseError a
runParsecFromString p txt =
  runParsecParser p "<parsecToReadE>" (fieldLineStreamFromString txt)

parsecToReadE :: (String -> ErrorMsg) -> ParsecParser a -> ReadE a
parsecToReadE err p = ReadE $ \txt ->
  const (err txt) `Bi.first` runParsecFromString p txt

parsecToReadEErr :: (Parsec.ParseError -> ErrorMsg) -> ParsecParser a -> ReadE a
parsecToReadEErr err p =
  ReadE $
    Bi.first err . runParsecFromString p

-- Show only unexpected error messages
unexpectMsgString :: Parsec.ParseError -> String
unexpectMsgString =
  unlines
    . map Parsec.messageString
    . filter (\case Parsec.UnExpect _ -> True; _ -> False)
    . Parsec.errorMessages
