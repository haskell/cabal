{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- This module defines class 'Namespace' and 'SomeNamespace'.
-- The idea is taken from here https://hackage.haskell.org/package/xmonad-0.18.0/docs/src/XMonad.Core.html#Message.
--
-- The goal is to create a "open" sum type where it becomes possible to put
-- any (with constraints) datatype we want as a key so it is associated with some trivia.
--
-- This solves module cycle and allows us to deal with FieldGrammar polymorphically.
module Distribution.Types.Namespace where

import Distribution.Compat.Prelude
import Prelude ()

import Control.DeepSeq
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Distribution.Utils.ShortText as ST

class
  ( Typeable a
  , Eq a
  , Ord a
  , Show a -- The Show constraint is simply for debugging
  ) => Namespace a
instance Namespace Char
instance Namespace Int
instance Namespace Bool
instance Namespace BS.ByteString
instance Namespace ST.ShortText
instance Namespace a => Namespace [a]
instance Namespace a => Namespace (Identity a)
instance (Namespace a, Namespace b) => Namespace (a, b)
instance (Namespace a, Namespace b, Namespace c) => Namespace (a, b, c)
instance (Namespace a, Namespace b, Namespace c, Namespace d) => Namespace (a, b, c, d)
instance (Namespace a, Namespace b, Namespace c, Namespace d, Namespace e) => Namespace (a, b, c, d, e)

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
isNamespace a someB = maybe False (==a) (fromNamespace someB)
