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
   parseReadE, readEOrFail,
   readP_to_E,
   parsecToReadE,
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.ReadP
import Distribution.Parsec.Class

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

parseReadE :: ReadE a -> ReadP r a
parseReadE (ReadE p) = do
  txt <- look
  either fail return (p txt)

readEOrFail :: ReadE a -> String -> a
readEOrFail r = either error id . runReadE r

-- {-# DEPRECATED readP_to_E "Use parsecToReadE. This symbol will be removed in Cabal-3.0 (est. Oct 2018)." #-}
readP_to_E :: (String -> ErrorMsg) -> ReadP a a -> ReadE a
readP_to_E err r =
    ReadE $ \txt -> case [ p | (p, s) <- readP_to_S r txt
                         , all isSpace s ]
                    of [] -> Left (err txt)
                       (p:_) -> Right p

parsecToReadE :: (String -> ErrorMsg) -> ParsecParser a -> ReadE a
parsecToReadE err p = ReadE $ \txt ->
    case runParsecParser p "<parsecToReadE>" txt of
        Right x -> Right x
        Left _e -> Left (err txt)
-- TODO: use parsec error to make 'ErrorMsg'.
