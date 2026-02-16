{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- This module is a twist on the existing pretty library, mainly making it
-- possible to place elements relatively.
module Distribution.Pretty.ExactDoc
  (
  -- * Type
    ExactDoc

  -- * Constructors
  , text
  , multilineText
  , nil

  -- * Primitive combinators
  , concat
  , place
  , nest

  -- * Helpers
  , newline
  , sep

  -- * Rendering
  , renderText
  )
  where

import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data ExactDoc where
  Text :: !T.Text -> ExactDoc
  Nil :: ExactDoc
  Concat :: !ExactDoc -> !ExactDoc -> ExactDoc
  Place :: !Int -> !Int -> !ExactDoc -> ExactDoc
  Nest :: !Int -> !ExactDoc -> ExactDoc

  deriving (Show, Eq)

instance Semigroup ExactDoc where
  (<>) = concatDoc

instance Monoid ExactDoc where
  mempty = Nil
  mconcat = foldl concatDoc Nil

renderText :: ExactDoc -> T.Text
renderText d0 = case d0 of
  Text t -> t
  Nil -> mempty
  Concat d1 d2 -> renderText d1 <> renderText d2
  Place n m d -> T.replicate n "\n" <> T.replicate m " " <> renderText d
  Nest n d -> T.replicate n " " <> renderText d

-- | Invariant: this assumes the input text doesn't have more than one line
text :: T.Text -> ExactDoc
text = Text

multilineText :: [T.Text] -> ExactDoc
multilineText = mconcat . intersperse newline . map Text

-- We use the exact offset primitive to define the newline primitive
newline :: ExactDoc
newline = place 1 0 (text "")

nil :: ExactDoc
nil = Nil

concatDoc :: ExactDoc -> ExactDoc -> ExactDoc
concatDoc d1 d2 = case (d1, d2) of
  (Nil, _) -> d2
  (_, Nil) -> d1
  _ -> d1 `Concat` d2

-- | Absolute offset "row, col" from the previous item
place :: Int -> Int -> ExactDoc -> ExactDoc
place s t d0 = case d0 of
  Nil -> Nil
  Place u v d -> place u v d -- Once placed, can't be moved
  Concat d1 d2 -> place s 0 d1 <> place 0 t d2
  _ -> Place s t d0

nest :: Int -> ExactDoc -> ExactDoc
nest k d0 = case d0 of
  Nil -> Nil
  Nest m d -> Nest (k + m) d
  Place u v d -> place u v d -- Once placed, can't be moved
  Concat d1 d2 -> nest k d1 <> nest k d2
  _ -> Nest k d0

sep :: ExactDoc -> [ExactDoc] -> ExactDoc
sep by = mconcat . intersperse by
