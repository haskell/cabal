-- |
-- Module      :  Distribution.Lex
-- Copyright   :  Ben Gamari 2015-2019
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains a simple lexer supporting quoted strings
module Distribution.Lex
  ( tokenizeQuotedWords
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- | A simple parser supporting quoted strings.
--
-- Please be aware that this will only split strings when seeing whitespace
-- outside of quotation marks;
--
-- > foo"bar baz"qux quux
--
-- >>> tokenizeQuotedWords "foo\"bar baz\"qux quux"
-- ["foobar bazqux","quux"]
--
-- This behavior can be useful when parsing text like;
--
-- > ghc-options: -Wl,"some option with spaces"
--
-- >>> tokenizeQuotedWords "ghc-options: -Wl,\"some option with spaces\""
-- ["ghc-options:","-Wl,some option with spaces"]
tokenizeQuotedWords :: String -> [String]
tokenizeQuotedWords xs = repack $ foldr alg (const ([] :| [])) xs False
  where
    repack :: NonEmpty String -> [String]
    repack (zs :| acc) = if null zs then acc else zs : acc

    alg
      :: Char -- current character
      -> (Bool -> NonEmpty String) -- continuation, depending on whether we are in a quoted region or not
      -> Bool -- are we in a quoted region?
      -> NonEmpty String
    alg '"' rest mode = rest (not mode)
    alg c rest False
      | isSpace c = [] :| repack (rest False)
    alg c rest mode = case rest mode of
      w :| ws -> (c : w) :| ws
