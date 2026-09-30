{-# LANGUAGE OverloadedStrings #-}

module Distribution.Fields.ExactPretty
  ( -- * Render fields
    runRenderFields

    -- * Line ending handling
  , guessLineEnding
  , lineEndingChar
  , LineEnding (..)
  )
where

import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)

import Control.Monad.RWS (MonadWriter (tell), RWS, asks, execRWS)
import Data.Foldable (traverse_)
import Distribution.Fields.Field
import Distribution.Parsec.Position
import Distribution.Utils.Generic (safeLast)

-- For now it has no fields, but it will hold line endings, for example.
data RenderConfig = RenderConfig
  { renderLineEnding :: LineEnding
  }

data LineEnding = CRLF | LF deriving (Eq)

guessLineEnding :: BS.ByteString -> Maybe LineEnding
guessLineEnding bs0
  | all hasCR ls = Just CRLF
  | all (not . hasCR) ls = Just LF
  | otherwise = Nothing
  where
    hasCR l = case BS8.unsnoc l of
      Just (_, '\r') -> True
      _ -> False
    ls = BS8.lines bs0

lineEndingChar :: LineEnding -> BS.ByteString
lineEndingChar LF = "\n"
lineEndingChar CRLF = "\r\n"

type RenderM = RWS RenderConfig BSB.Builder Position

runRenderFields :: [Field (WithComments Position)] -> BS.ByteString
runRenderFields = runRenderFields' cfg
  where
    cfg = RenderConfig{renderLineEnding = LF}

runRenderFields' :: RenderConfig -> [Field (WithComments Position)] -> BS.ByteString
runRenderFields' cfg fs =
  BS.toStrict $
    BSB.toLazyByteString $
      -- TODO(leana8959): we artifically terminate the file with a trailing newline.
      -- Think of a more polished way later.
      (<> BSB.byteString newlineChar) $
        snd $
          execRWS (renderFields fs) cfg initialPosition
  where
    initialPosition = Position 1 1
    newlineChar = lineEndingChar (renderLineEnding cfg)

renderFields :: [Field (WithComments Position)] -> RenderM ()
renderFields = traverse_ renderField

renderField :: Field (WithComments Position) -> RenderM ()
renderField (Field colonPos fname fls) = renderFieldName fname colonPos >> renderFieldLines fls
renderField (Section sname sargs fs) = renderSectionName sname >> renderSectionArgs sargs >> renderFields fs

renderFieldName :: Name (WithComments Position) -> Position -> RenderM ()
renderFieldName (Name ann name) colonPos = renderComments ann $ do
  renderBS name
  padUpToPosition colonPos >> renderBS ":"

renderSectionName :: Name (WithComments Position) -> RenderM ()
renderSectionName (Name ann name) = renderComments ann (renderBS name)

renderSectionArgs :: [SectionArg (WithComments Position)] -> RenderM ()
renderSectionArgs = traverse_ renderSectionArg

renderSectionArg :: SectionArg (WithComments Position) -> RenderM ()
renderSectionArg (SecArgName ann bs) = renderComments ann (renderBS bs)
renderSectionArg (SecArgStr ann bs) = renderComments ann (renderBS $ "\"" <> bs <> "\"")
renderSectionArg (SecArgOther ann bs) = renderComments ann (renderBS bs)

renderFieldLines :: [FieldLine (WithComments Position)] -> RenderM ()
renderFieldLines = traverse_ renderFieldLine

renderFieldLine :: FieldLine (WithComments Position) -> RenderM ()
renderFieldLine (FieldLine ann bs) = renderComments ann (renderBS bs)

padUpToPosition :: Position -> RenderM ()
padUpToPosition p@(Position r c) = do
  Position r0 c0 <- get
  ending <- asks renderLineEnding
  let padding
        -- forward jump of more than one line
        | r0 < r =
            let vPadding = mconcat $ replicate (r - r0) (if ending == CRLF then "\r\n" else "\n")
                hPadding = BS8.replicate (c - 1) ' '
             in vPadding <> hPadding
        -- same line
        | r0 == r =
            if c0 < c
              then BS8.replicate (c - c0) ' '
              else -- TODO(leana8959): same line backward jump, make it a warning
                if c0 == c then mempty else mempty
        -- backward jump
        | otherwise = "\n" <> BS8.replicate (c0 - 1) ' '
  put p
  tell (BSB.byteString padding)

renderBS :: BS.ByteString -> RenderM ()
renderBS bs = do
  modify incPositionByBS
  tell (BSB.byteString bs)
  where
    (rDim, cDim) = dimensionBS bs
    incPositionByBS (Position r c) = Position (r + rDim) (c + cDim)

renderComments :: WithComments Position -> RenderM () -> RenderM ()
renderComments (WithComments cmts pos) inner = do
  let (pre, post) = splitCommentsByPosition pos cmts
  traverse_ renderComment pre
  padUpToPosition pos >> inner
  traverse_ renderComment post

splitCommentsByPosition :: Position -> [Comment Position] -> ([Comment Position], [Comment Position])
splitCommentsByPosition p0 = span (\(Comment _ p) -> p <= p0)

renderComment :: Comment Position -> RenderM ()
renderComment (Comment bs pos) = liftA2 (<>) (padUpToPosition pos) (renderBS bs)

-- | The line count annd last column count of a 'ByteString'
dimensionBS :: BS.ByteString -> (Int, Int)
dimensionBS bs =
  let ls = BS8.lines bs
      mc = BS8.length <$> safeLast ls
   in (length ls - 1, fromMaybe 0 mc)
