{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides a way to specify a grammar of @.cabal@ -like files.
module Distribution.FieldGrammar
  ( -- * Field grammar type
    FieldGrammar (..)
  , uniqueField
  , optionalField
  , optionalFieldDef
  , monoidalField

    -- * Concrete grammar implementations
  , ParsecFieldGrammar
  , ParsecFieldGrammar'
  , parseFieldGrammar
  , fieldGrammarKnownFieldList
  , PrettyFieldGrammar
  , PrettyFieldGrammar'
  , prettyFieldGrammar

    -- * Auxiliary
  , (^^^)
  , Section (..)
  , Fields
  , partitionFields
  , takeFields
  , runFieldParser
  , runFieldParser'
  , defaultFreeTextFieldDefST

    -- * Newtypes
  , module Distribution.FieldGrammar.Newtypes
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Data.Map.Strict as Map

import Distribution.FieldGrammar.Class
import Distribution.FieldGrammar.Newtypes
import Distribution.FieldGrammar.Parsec
import Distribution.FieldGrammar.Pretty
import Distribution.Fields.Field
import Distribution.Utils.Generic (spanMaybe)

type ParsecFieldGrammar' a = ParsecFieldGrammar a a
type PrettyFieldGrammar' a = PrettyFieldGrammar a a

infixl 5 ^^^

-- | Reverse function application which binds tighter than '<$>' and '<*>'.
-- Useful for refining grammar specification.
--
-- @
-- \<*\> 'monoidalFieldAla' "extensions"           (alaList' FSep MQuoted)       oldExtensions
--     ^^^ 'deprecatedSince' [1,12] "Please use 'default-extensions' or 'other-extensions' fields."
-- @
(^^^) :: a -> (a -> b) -> b
x ^^^ f = f x

-- | Partitioning state
data PS ann = PS (Fields ann) [Section ann] [[Section ann]]

-- | Partition field list into field map and groups of sections.
-- Groups sections between fields. This means that the following snippet contains
-- two section groups:
--
-- @
-- -- first group
-- some-section
--     field: value
-- another-section
--     field: value
-- foo: bar
-- -- second group
-- yet-another-section
--     field: value
-- @
partitionFields :: [Field ann] -> (Fields ann, [[Section ann]])
partitionFields = finalize . foldl' f (PS mempty mempty mempty)
  where
    finalize :: PS ann -> (Fields ann, [[Section ann]])
    finalize (PS fs s ss)
      | null s = (fs, reverse ss)
      | otherwise = (fs, reverse (reverse s : ss))

    f :: PS ann -> Field ann -> PS ann
    f (PS fs s ss) (Field (Name ann name) fss) =
      PS (Map.insertWith (flip (++)) name [MkNamelessField ann fss] fs) [] ss'
      where
        ss'
          | null s = ss
          | otherwise = reverse s : ss
    f (PS fs s ss) (Section name sargs sfields) =
      PS fs (MkSection name sargs sfields : s) ss

-- | Take all fields from the front.
takeFields :: [Field ann] -> (Fields ann, [Field ann])
takeFields = finalize . spanMaybe match
  where
    finalize (fs, rest) = (Map.fromListWith (flip (++)) fs, rest)

    match (Field (Name ann name) fs) = Just (name, [MkNamelessField ann fs])
    match _ = Nothing
