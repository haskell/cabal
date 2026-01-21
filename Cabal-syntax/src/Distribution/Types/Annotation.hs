{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

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
module Distribution.Types.Annotation where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Namespace

import qualified Data.Map as M
import qualified Text.PrettyPrint as Disp

type Trivia = [Trivium]
data Trivium
  = FieldNth Int
  | Nth Int
  | PreTrivia String
  | PostTrivia String
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

fromNamedTrivia :: SomeNamespace -> Trivia -> TriviaTree
fromNamedTrivia ns ts = TriviaTree mempty (M.singleton ns (TriviaTree ts mempty))

emptyTriviaTree :: TriviaTree
emptyTriviaTree = TriviaTree mempty mempty

-- | Wrap the trivia within a namespace
mark :: SomeNamespace -> TriviaTree -> TriviaTree
mark ns ts = TriviaTree mempty (M.singleton ns ts)

-- | If the trivia map is for this scope
unmark :: SomeNamespace -> TriviaTree -> TriviaTree
unmark ns tt = fromMaybe mempty (M.lookup ns (namedAnnotations tt))

atFieldNth :: Trivia -> Maybe Int
atFieldNth [] = Nothing
atFieldNth ( t : ts ) = case t of
  FieldNth n -> Just n
  _ -> atFieldNth ts

atNth :: Trivia -> Maybe Int
atNth [] = Nothing
atNth ( t : ts ) = case t of
  Nth n -> Just n
  _ -> atNth ts

triviaToDoc :: Trivia -> Disp.Doc -> Disp.Doc
triviaToDoc [] x = x
triviaToDoc (t : ts) x = triviaToDoc ts (triviumToDoc t x)

triviumToDoc :: Trivium -> Disp.Doc -> Disp.Doc
triviumToDoc t x = case t of
  PreTrivia s -> Disp.text s <> x
  PostTrivia s -> x <> Disp.text s
  IsInjected -> mempty -- the doc in question shouldn't be rendered
