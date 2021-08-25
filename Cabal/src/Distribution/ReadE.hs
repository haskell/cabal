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

module Distribution.ReadE (
   -- * ReadE
   ReadE(..), succeedReadE, failReadE,
   -- * Projections
   parsecToReadE,
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Parsec.FieldLineStream

-- | Parser with simple error reporting
newtype ReadE a = ReadE {runReadE :: String -> Either ErrorMsg a}
type ErrorMsg   = String

instance Functor ReadE where
  fmap f (ReadE p) = ReadE $ \txt -> case p txt of
                                       Right a  -> Right (f a)
                                       Left err -> Left err

succeedReadE :: (String -> a) -> ReadE a
succeedReadE f = ReadE (Right . f)

failReadE :: ErrorMsg -> ReadE a
failReadE = ReadE . const . Left

parsecToReadE :: (String -> ErrorMsg) -> ParsecParser a -> ReadE a
parsecToReadE err p = ReadE $ \txt ->
    case runParsecParser p "<parsecToReadE>" (fieldLineStreamFromString txt) of
        Right x -> Right x
        Left _e -> Left (err txt)
-- TODO: use parsec error to make 'ErrorMsg'.
