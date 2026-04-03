{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Trivia
  ( Trivia (..)
  , SurroundingText (..)
  , preTrivia
  , postTrivia
  , Ann (..)
  , mapAnn
  , mapAnnA
  , applyTriviaDoc
  )
where

import Control.Applicative
import Data.Data
import Distribution.Parsec.Position
import qualified Text.PrettyPrint as Disp

-- | Leading and trailing whitespaces
data SurroundingText = SurroundingText String String
  deriving (Show, Eq, Ord, Read, Data)

instance Semigroup SurroundingText where
  SurroundingText s t <> SurroundingText a b = SurroundingText (s <> a) (t <> b)

data Trivia t
  = HasTrivia t
  | ExactRepresentation String
  | IsInserted
  | NoTrivia
  deriving (Show, Eq, Ord, Read, Data)

preTrivia :: String -> Trivia SurroundingText
preTrivia s = HasTrivia (SurroundingText s mempty)

postTrivia :: String -> Trivia SurroundingText
postTrivia s = HasTrivia (SurroundingText mempty s)

instance Semigroup t => Semigroup (Trivia t) where
  HasTrivia x <> HasTrivia y = HasTrivia (x <> y)
  ExactRepresentation u <> ExactRepresentation v = ExactRepresentation (u <> v)
  u@(ExactRepresentation _) <> _ = u
  _ <> v@(ExactRepresentation _) = v
  NoTrivia <> v = v
  u <> NoTrivia = u
  IsInserted <> _ = IsInserted
  _ <> IsInserted = IsInserted

instance Semigroup t => Monoid (Trivia t) where
  mempty = NoTrivia

data Ann t a = Ann
  { getAnn :: Trivia t
  , unAnn :: a
  }
  deriving (Show, Eq, Ord, Functor, Read, Data)

mapAnn
  :: (Trivia s -> Trivia t)
  -> Ann s a
  -> Ann t a
mapAnn f (Ann t x) = Ann (f t) x

mapAnnA
  :: (Trivia s -> Trivia t)
  -> (a -> b)
  -> Ann s a
  -> Ann t b
mapAnnA f g (Ann t x) = Ann (f t) (g x)

applyTriviaDoc
  :: Trivia SurroundingText
  -> Disp.Doc
  -> Disp.Doc
applyTriviaDoc t = case t of
  HasTrivia (SurroundingText pre post) -> \d -> Disp.text pre <> d <> Disp.text post
  ExactRepresentation repr -> const (Disp.text repr)
  IsInserted -> const Disp.empty
  NoTrivia -> id
