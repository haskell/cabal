{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Cabal-like file AST types: 'Field', 'Section' etc,
--
-- This (intermediate) data type is used for pretty-printing.
--
-- @since 3.0.0.0
module Distribution.Fields.Pretty
  ( -- * Fields
    CommentPosition (..)
  , PrettyField (..)
  , PrettyFieldLine (..)
  , showFields
  , showFields'
  , exactShowFields

    -- * Transformation from 'P.Field'
  , fromParsecFields
  , genericFromParsecFields
  , prettyFieldLines
  , prettySectionArgs
  ) where

import Control.Monad
import Distribution.Compat.Prelude
import Distribution.Parsec.Position
import Distribution.Pretty (showToken)
import Prelude ()

import Distribution.Types.Annotation

import Distribution.Fields.Field (FieldName)
import Distribution.Utils.Generic (fromUTF8BS, safeHead)

import qualified Distribution.Fields.Parser as P

import qualified Data.ByteString as BS
import qualified Data.List as List (tail, sortOn)
import qualified Text.PrettyPrint as PP

import Debug.Pretty.Simple

-- | This type is used to discern when a comment block should go
--   before or after a cabal-like file field, otherwise it would
--   be hardcoded to a single position. It is often used in
--   conjunction with @PrettyField@.
data CommentPosition = CommentBefore [String] | CommentAfter [String] | NoComment

unAnnotatePrettyFieldLines :: [PrettyFieldLine ann] -> [PP.Doc]
unAnnotatePrettyFieldLines = map unAnnotatePrettyFieldLine

unAnnotatePrettyFieldLine :: PrettyFieldLine ann -> PP.Doc
unAnnotatePrettyFieldLine (PrettyFieldLine _ doc) = doc

-- |
-- Each PrettyFieldLine within a PrettyField is a line
-- They defer the merging of Doc and allow meaningful line-wise
-- position annotation.
data PrettyFieldLine ann = PrettyFieldLine ann PP.Doc
  deriving (Functor, Foldable, Traversable, Show {-NOTE(leana8959): for debugging-})

prettyFieldLineAnn :: PrettyFieldLine ann -> ann
prettyFieldLineAnn (PrettyFieldLine ann _) = ann

prettyFieldLinePosition :: PrettyFieldLine Trivia -> Maybe Position
prettyFieldLinePosition = atPosition . prettyFieldLineAnn

data PrettyField ann
  = PrettyField ann FieldName [PrettyFieldLine ann]
  | PrettySection ann FieldName [PP.Doc] [PrettyField ann]
  | PrettyEmpty
  deriving (Functor, Foldable, Traversable, Show {- NOTE(leana8959): for debugging -})

prettyFieldAnn :: PrettyField ann -> Maybe ann
prettyFieldAnn (PrettyField ann _ _) = Just ann
prettyFieldAnn (PrettySection ann _ _ _) = Just ann
prettyFieldAnn _ = Nothing

prettyFieldPosition :: PrettyField Trivia -> Maybe Position
prettyFieldPosition = atFieldPosition <=< prettyFieldAnn

-- | Prettyprint a list of fields.
--
-- Note: the first argument should return 'String's without newlines
-- and properly prefixes (with @--@) to count as comments.
-- This unsafety is left in place so one could generate empty lines
-- between comment lines.
showFields :: [PrettyField ann] -> String
showFields = showFields' getPos (const $ const id)
  where
    getPos = PositionFromPrettyField (const Nothing) (const Nothing)

exactShowFields :: [PrettyField Trivia] -> String
exactShowFields = PP.renderStyle PP.style . mconcat . snd . exactRenderPrettyFields ctx0
  where
    ctx0 = (Nothing, Nothing)

data PositionFromPrettyField ann = PositionFromPrettyField
  { fieldPosition :: ann -> Maybe Position
  , fieldLinePosition :: ann -> Maybe Position
  }

-- | 'showFields' with user specified indentation.
showFields'
  :: PositionFromPrettyField ann
  -- ^ Extract the position information from the fields
  -> (Maybe Position -> Maybe Position -> PP.Doc -> PP.Doc)
  -- ^ Post-process non-annotation produced lines.
  -> [PrettyField ann]
  -- ^ Fields/sections to show.
  -> String
showFields' getPos post fs = concat $ renderFields startPos (Opts getPos post) $ fs
  where
    startPos = foldl (<|>) Nothing ((fieldPosition getPos <=< prettyFieldAnn) <$> fs)

data Opts ann = Opts
  { _optPosition :: PositionFromPrettyField ann
  , _optPostprocess :: Maybe Position -> Maybe Position -> PP.Doc -> PP.Doc
  }

renderFields :: forall ann. Maybe Position -> Opts ann -> [PrettyField ann] -> [String]
renderFields startPos opts@(Opts getPos _) fields = flattenBlocks blocks
  where
    blocks =
      filter (not . null . _contentsBlock) -- empty blocks cause extra newlines #8236
        $ (\(accBlocks, _lastFieldLinePos) -> reverse accBlocks)
        $ foldl
          ( \(accBlocks, lastFieldLinePos) (pos, x) ->
              let (newPos, block) = renderField opts (lastFieldLinePos <|> pos) x
               in (block : accBlocks, newPos)
          )
          ([], Nothing)
        $ zip (startPos : map (fieldLinePosition getPos <=< prettyFieldAnn) fields) fields

-- | Block of lines with flags for optional blank lines before and after
data Block = Block
  { _beforeBlock :: Margin
  , _afterBlock :: Margin
  , _contentsBlock :: [String]
  }

data Margin = Margin | NoMargin
  deriving (Eq)

-- | Collapse margins, any margin = margin
instance Semigroup Margin where
  NoMargin <> NoMargin = NoMargin
  _ <> _ = Margin

flattenBlocks :: [Block] -> [String]
flattenBlocks = go0
  where
    go0 [] = []
    go0 (Block _before after strs : blocks) = strs ++ go after blocks

    go _surr' [] = []
    go surr' (Block before after strs : blocks) = ins $ strs ++ go after blocks
      where
        ins
          | surr' <> before == Margin = ("" :)
          | otherwise = id

renderField :: forall ann. Opts ann -> Maybe Position -> PrettyField ann -> (Maybe Position, Block)
renderField opts@(Opts getPos fixupPosition) prevPos (PrettyField ann name fieldLines) =
  (fieldLinesLastPos, Block NoMargin NoMargin content)
  where
    fieldLinesPos :: [Maybe Position]
    fieldLinesPos = map (fieldLinePosition getPos . prettyFieldLineAnn) fieldLines

    fieldLinesLastPos :: Maybe Position
    fieldLinesLastPos = foldl (flip (<|>)) Nothing fieldLinesPos

    fieldLines' :: [String]
    fieldLines' =
      map
        ( \(prevLinePos, fl) ->
            renderPrettyFieldLine opts prevLinePos fl
        )
        $ zip
          (startPos : fieldLinesPos)
          fieldLines
      where
        startPos = foldl (<|>) Nothing (fieldPosition getPos . prettyFieldLineAnn <$> fieldLines)

    -- TODO(leana8959): use the pretty library to render the field names
    content = case fieldLines' of
      [] -> [name' ++ ":"]
      _ ->
        let selfPos = fieldPosition getPos ann
            rowDiff = liftA2 subtractRow selfPos prevPos
            subtractRow (Position rowx _) (Position rowy _) = rowx - rowy - 1

            maybeNewlines = mconcat $ replicate (fromMaybe 0 rowDiff) ["\n"]
         in -- The POSIX definition of a line always ends with a newline
            -- We patch up the last newline
            -- FIXME(leana8959): the newline after name is artificial
            -- It should depend on the original cabal source file
            maybeNewlines ++ [name' ++ ":"] ++ fieldLines' ++ ["\n"]
    name' = fromUTF8BS name
renderField opts@(Opts getPos fixupPosition) prevPos (PrettySection ann name args fields) =
  (lastPos,) $
    Block Margin Margin $ -- TODO(leana8959): fix indentation with exact positioning
      [PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args]
        ++ renderFields (fieldLinePosition getPos ann) opts fields
  where
    lastPos :: Maybe Position
    lastPos = foldl (flip (<|>)) Nothing $ concatMap fPos fields
      where
        fPos :: PrettyField ann -> [Maybe Position]
        fPos field = case field of
          PrettyField _ _ fls -> concatMap flPos fls
          PrettySection _ _ _ fs' -> concatMap fPos fs'
          PrettyEmpty -> []

        flPos :: PrettyFieldLine ann -> [Maybe Position]
        flPos = pure . fieldLinePosition getPos . prettyFieldLineAnn
renderField _ prevPos PrettyEmpty = (prevPos, Block NoMargin NoMargin mempty)

-- |
-- Invariant: a PrettyFieldLine is never more than one line
renderPrettyFieldLine :: Opts ann -> Maybe Position -> PrettyFieldLine ann -> String
renderPrettyFieldLine (Opts getPos fixupPosition) prevPos (PrettyFieldLine ann doc) =
  let curPos = fieldLinePosition getPos $ ann
   in PP.renderStyle PP.style $
        fixupPosition prevPos curPos $
          doc

-------------------------------------------------------------------------------
-- Transform from Parsec.Field
-------------------------------------------------------------------------------

genericFromParsecFields
  :: Applicative f
  => (FieldName -> [P.FieldLine ann] -> f [PrettyFieldLine ann])
  -- ^ transform field contents
  -> (FieldName -> [P.SectionArg ann] -> f [PP.Doc])
  -- ^ transform section arguments
  -> [P.Field ann]
  -> f [PrettyField ann]
genericFromParsecFields f g = goMany
  where
    goMany = traverse go

    go (P.Field (P.Name ann name) fls) = PrettyField ann name <$> f name fls
    go (P.Section (P.Name ann name) secargs fs) = PrettySection ann name <$> g name secargs <*> goMany fs

-- -- | Used in 'fromParsecFields'.
-- prettyFieldLines :: FieldName -> [P.FieldLine ann] -> PP.Doc
-- prettyFieldLines _ fls =
--   PP.vcat
--     [ PP.text $ fromUTF8BS bs
--     | P.FieldLine _ bs <- fls
--     ]

prettyFieldLines :: [P.FieldLine ann] -> [PrettyFieldLine ann]
prettyFieldLines = map go
  where
    go (P.FieldLine ann bs) =
      PrettyFieldLine ann (PP.text $ fromUTF8BS bs)

-- | Used in 'fromParsecFields'.
prettySectionArgs :: FieldName -> [P.SectionArg ann] -> [PP.Doc]
prettySectionArgs _ = map $ \case
  P.SecArgName _ bs -> showToken $ fromUTF8BS bs
  P.SecArgStr _ bs -> showToken $ fromUTF8BS bs
  P.SecArgOther _ bs -> PP.text $ fromUTF8BS bs

-- | Simple variant of 'genericFromParsecField'
fromParsecFields :: [P.Field ann] -> [PrettyField ann]
fromParsecFields =
  runIdentity
    . genericFromParsecFields
      (Identity .: const prettyFieldLines)
      (Identity .: prettySectionArgs)
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)

-- Experiment on preprocessing [PrettyField ann], mainly fixing up the position

type PrettyFieldPositionContext ann =
  ( Maybe (PrettyField ann)
  , Maybe (PrettyFieldLine ann)
  )

exactRenderPrettyFields
  :: PrettyFieldPositionContext Trivia
  -> [PrettyField Trivia]
  -> (PrettyFieldPositionContext Trivia, [PP.Doc])
exactRenderPrettyFields ctx0 = fmap reverse . foldl go state0 . sortPrettyFields
  where
    state0 :: (PrettyFieldPositionContext Trivia, [PP.Doc])
    state0 = (ctx0, [])

    go (ctx, processed) field =
      let (ctx', field') = exactRenderPrettyField ctx field
      in  (ctx', field' : processed)

exactRenderPrettyField
  :: PrettyFieldPositionContext Trivia
  -> PrettyField Trivia
  -> (PrettyFieldPositionContext Trivia, PP.Doc)
exactRenderPrettyField ctx0@(lastField, lastFieldLine) field = case field of
  -- Absorb empty
  PrettyEmpty -> (ctx0, mempty)
  PrettyField ann fieldName fieldLines ->
    let ctx' :: PrettyFieldPositionContext Trivia
        fieldLines' :: [PP.Doc]
        (ctx', fieldLines') = exactRenderPrettyFieldLines (Just field, lastFieldLine) fieldLines

        fieldNamePosition :: Maybe Position
        fieldNamePosition = prettyFieldPosition field

        fieldLinesFirstPos :: Maybe Position
        fieldLinesFirstPos = prettyFieldLinePosition <=< safeHead $ fieldLines

        -- The fieldLines are all patched and we only need to concat them together
        fieldLinesFinal :: PP.Doc
        fieldLinesFinal = fixupPosition fieldNamePosition fieldLinesFirstPos
                          $ mconcat fieldLines'

    in  (ctx', fieldLinesFinal)
  PrettySection ann fieldName sectionArgs fields ->
    let ctx' :: PrettyFieldPositionContext Trivia
        fields' :: [PP.Doc]
        (ctx', fields') = exactRenderPrettyFields (Just field, lastFieldLine) fields

        sectionNamePosition :: Maybe Position
        sectionNamePosition = prettyFieldPosition field

        fieldsFirstPosition :: Maybe Position
        fieldsFirstPosition = prettyFieldPosition <=< safeHead $ fields

        -- TODO(leana8959): section args are currently not exactly positioned
        fieldsFinal :: PP.Doc
        fieldsFinal = fixupPosition sectionNamePosition fieldsFirstPosition
                      $ mconcat fields'
    in  (ctx', fieldsFinal)

exactRenderPrettyFieldLines
  :: PrettyFieldPositionContext Trivia
  -> [PrettyFieldLine Trivia]
  -> (PrettyFieldPositionContext Trivia, [PP.Doc])
exactRenderPrettyFieldLines ctx0 = fmap reverse . foldl go state0 . sortPrettyFieldLines
  where
    state0 :: (PrettyFieldPositionContext Trivia, [PP.Doc])
    state0 = (ctx0, [])

    go (ctx, processed) fieldLine =
      let (ctx', fieldLine') = exactRenderPrettyFieldLine ctx fieldLine
      in  (ctx', fieldLine' : processed)

exactRenderPrettyFieldLine
  :: PrettyFieldPositionContext Trivia
  -> PrettyFieldLine Trivia
  -> (PrettyFieldPositionContext Trivia, PP.Doc)
exactRenderPrettyFieldLine (lastField, lastFieldLine) fieldLine@(PrettyFieldLine _ doc) =
  let lastPosition :: Maybe Position
      lastPosition = (prettyFieldLinePosition =<< lastFieldLine) <|> (prettyFieldPosition =<< lastField)

      fieldLinePosition :: Maybe Position
      fieldLinePosition = prettyFieldLinePosition fieldLine

      ctx' :: PrettyFieldPositionContext Trivia
      ctx' = (lastField, Just fieldLine)

      fieldLine' :: PP.Doc
      fieldLine' = fixupPosition lastPosition fieldLinePosition doc
  in  (ctx', fieldLine')

sortPrettyFields :: [PrettyField Trivia] -> [PrettyField Trivia]
sortPrettyFields = List.sortOn $ fromMaybe zeroPos . prettyFieldPosition

sortPrettyFieldLines :: [PrettyFieldLine Trivia] -> [PrettyFieldLine Trivia]
sortPrettyFieldLines = List.sortOn $ fromMaybe zeroPos . prettyFieldLinePosition

fixupPosition :: Maybe Position -> Maybe Position -> PP.Doc -> PP.Doc
fixupPosition prevPos curPos =
  foldr (.) id $ case (curPos, prevPos) of
    -- TODO(leana8959): make indentation modification apply to more than one lines
    (Just (Position rx cx), Just (Position ry _cy)) ->
        let (rDiff, cDiff) = (rx - ry, if rx /= ry then cx else 0)
        in  replicate (rDiff - 1) (PP.text "" PP.$$) ++ replicate (cDiff - 1) (PP.text " " <>)

    -- No previous position to calculate line jump, but still compute column offset
    (Nothing, Just (Position _ cy)) -> replicate (cy - 1) (PP.text " " <>)

    -- Has previous position but current entry has no position
    -- Probably inserted programmatically, default to indent of 4
    (Just _, Nothing) -> [(PP.text "    " <>)]

    -- Prepend space purely for readability
    (Nothing, Nothing) -> [(PP.text " " <>)]
