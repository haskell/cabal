{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- This module defines 'TriviaTree', a recursive map structure.
-- It helps us remember which data is associated with which trivia, by mirroring the shape of the parse tree.
--
-- A trivia tree is (currently) defined as a tree that has "unnamed" and those that are named.
-- The unnamed ones 'justAnnotation' is associated to the current data structure we have in scope,
-- and the named ones 'namedAnnotations' corresponds to the substructures.
--
-- See Distribution.Types.Namespace to see how we compare data as a key of the tree.
--
-- Two important ways to manipulate a 'TriviaTree' is 'mark' and 'unmark'.
--
-- 'mark' takes a namespace and a trivia tree, and "marks" it as below a Namespace
-- 'unmark' is the inverse of 'mark', asks the question "what are the trivia associated with Namespace ns".
module Distribution.Types.Annotation
  ( Namespace
  , SomeNamespace (..)
  , fromNamespace
  , isNamespace
  , Trivia
  , Trivium (..)
  , TriviaTree (..)
  , Markable (..)
  , fromNamedTrivia
  , emptyTriviaTree
  , atFieldNth
  , atNth
  , atPosition
  , patchPosition
  , triviaToDoc
  , triviumToDoc
  )
where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Position

import Data.Kind
import Data.Monoid
import Data.Typeable
import Data.Monoid

import qualified Data.Map as M
import qualified Text.PrettyPrint as Disp

import qualified Data.ByteString as BS

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

class Markable a where
  markTriviaTree :: a -> TriviaTree -> TriviaTree
  default markTriviaTree :: Namespace a => a -> TriviaTree -> TriviaTree
  markTriviaTree x t = TriviaTree mempty (M.singleton (SomeNamespace x) t)

  unmarkTriviaTree :: a -> TriviaTree -> TriviaTree
  default unmarkTriviaTree :: Namespace a => a -> TriviaTree -> TriviaTree
  unmarkTriviaTree x t = fromMaybe mempty (M.lookup (SomeNamespace x) (namedAnnotations t))

instance Markable BS.ByteString
instance (Markable a, Namespace a) => Markable (Identity a)
instance (Markable a, Namespace a) => Markable (Last a)

-- This instance is for String and alike
-- For actual lists where multiplicity matters, see List sep b a
instance (Typeable a, Namespace a) => Markable [a]

instance (Namespace a, Markable a, Namespace b, Markable b) => Markable (a, b)

type Trivia = [Trivium]
data Trivium
  = FieldNth Int
  | Nth Int
  | PreTrivia String
  | PostTrivia String
  | ExactPosition Position
  | IsInjected
  deriving (Generic, Show, Eq)

instance NFData Trivium

-- TODO(leana8959): can I encode namespace type in the signature or somehow make this more type safe?
data TriviaTree = TriviaTree
  { justAnnotation :: Trivia
  , namedAnnotations :: Map SomeNamespace TriviaTree
  }
  deriving (Generic, Show, Eq)

instance Semigroup TriviaTree where
  TriviaTree locala belowa <> TriviaTree localb belowb =
    TriviaTree
      (locala <> localb)
      (M.unionWith (<>) belowa belowb) -- The (<>) of map is clobbering, we need to use (<>) to join the values

instance Monoid TriviaTree where
  mempty = emptyTriviaTree

fromNamedTrivia :: Markable a => a -> Trivia -> TriviaTree
fromNamedTrivia x ts = markTriviaTree x (TriviaTree ts mempty)

emptyTriviaTree :: TriviaTree
emptyTriviaTree = TriviaTree mempty mempty

atFieldNth :: Trivia -> Maybe Int
atFieldNth [] = Nothing
atFieldNth (t : ts) = case t of
  FieldNth n -> Just n
  _ -> atFieldNth ts

atNth :: Trivia -> Maybe Int
atNth [] = Nothing
atNth (t : ts) = case t of
  Nth n -> Just n
  _ -> atNth ts

atPosition :: Trivia -> Maybe Position
atPosition [] = Nothing
atPosition (t : ts) = case t of
  ExactPosition pos -> Just pos
  _ -> atPosition ts

triviaToDoc :: Trivia -> Disp.Doc -> Disp.Doc
triviaToDoc [] x = x
triviaToDoc (t : ts) x = triviaToDoc ts (triviumToDoc t x)

triviumToDoc :: Trivium -> Disp.Doc -> Disp.Doc
triviumToDoc t x = case t of
  PreTrivia s -> Disp.text s <> x
  PostTrivia s -> x <> Disp.text s
  IsInjected -> mempty -- the doc in question shouldn't be rendered
  _ -> x -- TODO(leana8959): ignore the rest for now

-- There's no hardbreak primitive in printer
--
-- Precondition:
-- The previous element must be before current element
--
-- https://github.com/haskell/pretty/issues/26
patchPosition :: Position -> Position -> Disp.Doc -> Disp.Doc
patchPosition (Position prevRow _) (Position curRow _) =
  let rowDiff = curRow - prevRow

  in  appEndo . foldMap Endo
        $ replicate rowDiff ((Disp.text "") Disp.$+$)
