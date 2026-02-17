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

import Distribution.Parsec.Position

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Debug.Pretty.Simple

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

type RenderState = Position

-- |
-- Outputs the padding Text to currect the cursor position if needed,
-- changes the state otherwise.
updateCursorRow :: Int -> State RenderState Text
updateCursorRow row = do
  Position currentRow currentCol <- get
  let rowDiff = row - currentRow
      padding = T.replicate rowDiff "\n"

  when (rowDiff < 0) $
    put (Position row currentCol)

  pure padding

updateCursorCol :: Int -> State RenderState Text
updateCursorCol col = do
  Position currentRow currentCol <- get
  let colDiff = col - currentCol - 1
      padding = T.replicate colDiff " "

  when (colDiff < 0) $
    put (Position currentRow col)

  pure padding

renderText :: ExactDoc -> Text
renderText doc = evalState (renderTextStep doc) state0
  where
    state0 = zeroPos

renderTextStep :: ExactDoc -> State RenderState Text
renderTextStep d0 = get >>= \(Position row col) -> case d0 of
  Nil -> pure mempty

  Place atRow atCol d ->
    liftA3 (\x y z -> x <> y <> z)
      (updateCursorRow atRow)
      (updateCursorCol atCol)
      (renderTextStep d)

  Nest indentSize d -> liftA2 (<>) (updateCursorCol indentSize) (renderTextStep d)

  Text t -> liftA2 (<>) (updateCursorCol (T.length t)) (pure t)

  Concat d1 d2 -> liftA2 (<>) (renderTextStep d1) (renderTextStep d2)

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
