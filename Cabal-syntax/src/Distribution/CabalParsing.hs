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

import Distribution.Types.Annotation

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
  parsecWarning :: PWarnType -> String -> m ()

  parsecHaskellString :: m String
  parsecHaskellString = stringLiteral

  askCabalSpecVersion :: m CabalSpecVersion

