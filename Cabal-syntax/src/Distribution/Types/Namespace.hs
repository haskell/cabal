{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- This module defines class 'Namespace' and 'SomeNamespace'.
-- The idea is taken from here https://hackage.haskell.org/package/xmonad-0.18.0/docs/src/XMonad.Core.html#Message.
--
-- The goal is to create a "open" sum type where it becomes possible to put
-- any (with constraints) datatype we want as a key so it is associated with some trivia.
--
-- This solves module cycle and allows us to deal with FieldGrammar polymorphically.
module Distribution.Types.Namespace where

import Data.Typeable
import Distribution.Compat.Prelude
import Prelude ()

type Namespace a =
  ( Typeable a
  , Eq a
  , Ord a
  , Show a -- The Show constraint is simply for debugging
  )

data SomeNamespace = forall a. Namespace a => SomeNamespace a
deriving instance Show SomeNamespace

instance Eq SomeNamespace where
  (SomeNamespace x) == (SomeNamespace y) = case cast x of
    Just x' -> x' == y
    Nothing -> False

instance Ord SomeNamespace where
  (SomeNamespace x) <= (SomeNamespace y) = case cast x of
    Just x' -> x' <= y
    Nothing -> typeOf x <= typeOf y

fromNamespace :: Namespace a => SomeNamespace -> Maybe a
fromNamespace (SomeNamespace ns) = cast ns

isNamespace :: Namespace a => a -> SomeNamespace -> Bool
isNamespace a someB = maybe False (== a) (fromNamespace someB)
