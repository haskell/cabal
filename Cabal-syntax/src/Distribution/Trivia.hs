{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Distribution.Trivia
  ( Trivia (..)
  , preTrivia
  , postTrivia
  , posTrivia
  , Ann (..)
  , mapAnn
  , mapAnnA
  , applyTriviaDoc
  )
where

import Data.Data
import Control.Applicative
import Distribution.Parsec.Position
import qualified Text.PrettyPrint as Disp

-- TODO(leana8959): implement position trivia somewhere
data Trivia
  = HasTrivia (Maybe Position) String String
  | ExactRepresentation String
  | IsInserted
  | NoTrivia
  deriving (Show, Eq, Ord, Read, Data)

preTrivia :: String -> Trivia
preTrivia s = HasTrivia Nothing s mempty

postTrivia :: String -> Trivia
postTrivia s = HasTrivia Nothing mempty s

posTrivia :: Position -> Trivia
posTrivia pos = HasTrivia (Just pos) mempty mempty

instance Semigroup Trivia where
  HasTrivia mpos s t <> HasTrivia mpos' a b = HasTrivia (mpos <|> mpos') (s <> a) (t <> b)

  ExactRepresentation u <> ExactRepresentation v = ExactRepresentation (u <> v)
  u@(ExactRepresentation _) <> _ = u
  _ <> v@(ExactRepresentation _) = v

  NoTrivia <> v = v
  u <> NoTrivia = u

  IsInserted <> _ = IsInserted
  _ <> IsInserted = IsInserted

instance Monoid Trivia where
  mempty = NoTrivia

data Ann a = Ann
  { getAnn :: Trivia
  , unAnn :: a
  }
  deriving (Show, Eq, Ord, Functor, Read, Data)

mapAnn
  :: (Trivia -> Trivia)
  -> Ann a
  -> Ann a
mapAnn f (Ann t x) = Ann (f t) x

mapAnnA
  :: (Trivia -> Trivia)
  -> (a -> a)
  -> Ann a
  -> Ann a
mapAnnA f g (Ann t x) = Ann (f t) (g x)

applyTriviaDoc
  :: Trivia
  -> Disp.Doc
  -> Disp.Doc
applyTriviaDoc t = case t of
  -- TODO(leana8959): do not ignore the position here
  HasTrivia _ pre post -> \d -> Disp.text pre <> d <> Disp.text post
  ExactRepresentation repr -> const (Disp.text repr)
  IsInserted -> const Disp.empty
  NoTrivia -> id
