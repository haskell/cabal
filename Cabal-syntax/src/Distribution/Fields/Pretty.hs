{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Cabal-like file AST types: 'Field', 'Section' etc,
--
-- This (intermediate) data type is used for pretty-printing.
--
-- @since 3.0.0.0
module Distribution.Fields.Pretty
  ( -- * Fields
    CommentPosition (..)
  , PrettyField
  , PrettyFieldWith (..)
  , filterFields
  , sortPrettyFields
  , showFields
  , exactShowFields
  , showFields'

    -- * Transformation from 'P.Field'
  , fromParsecFields
  , genericFromParsecFields
  , prettyFieldLines
  , prettySectionArgs
  ) where

import Control.Monad ((<=<))
import Distribution.Compat.Prelude
import Distribution.Pretty (showToken)
import Prelude ()

import Distribution.Fields.Field (FieldName)
import Distribution.Parsec.Position
import Distribution.Utils.Generic (fromUTF8BS, safeHead)

import qualified Data.Text as T

import qualified Distribution.Fields.Parser as P
import Distribution.Pretty.ExactDoc (ExactDoc)
import qualified Distribution.Pretty.ExactDoc as EPP
import Distribution.Types.Annotation
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import qualified Data.ByteString as BS
import Data.List (sortOn)

-- | This type is used to discern when a comment block should go
--   before or after a cabal-like file field, otherwise it would
--   be hardcoded to a single position. It is often used in
--   conjunction with @PrettyField@.
data CommentPosition = CommentBefore [String] | CommentAfter [String] | NoComment

type PrettyField = PrettyFieldWith Abst

deriving instance Eq (PrettyFieldWith Conc)

-- NOTE(leana8959): some pretty field considerations
--  - do section args need to be a _list_ of PP.Doc
--  - do we need pretty empty with exact doc
--
-- TODO(leana8959): we need to reproduce the field line
--  - each Doc in field (FieldLine) should come with its positioning
data PrettyFieldWith (mod :: ParsingPhase)
  = PrettyField (AttachPosition mod FieldName) (AttachPosition mod PP.Doc)
  | PrettySection (AttachPosition mod FieldName) [PP.Doc] [PrettyFieldWith mod]

prettyFieldPosition :: PrettyFieldWith Conc -> Maybe Position
prettyFieldPosition (PrettyField (pos, _) _) = Just pos
prettyFieldPosition _ = Nothing

-- prettySectionPosition :: PrettyFieldWith Conc -> Maybe Position
-- prettySectionPosition (PrettySection (pos, _) _ _) = Just pos
-- prettySectionPosition _ = Nothing

deriving instance Show (PrettyFieldWith Abst)
deriving instance Show (PrettyFieldWith Conc)

-- | Prettyprint a list of fields.
--
-- Note: the first argument should return 'String's without newlines
-- and properly prefixes (with @--@) to count as comments.
-- This unsafety is left in place so one could generate empty lines
-- between comment lines.
showFields :: (ann -> CommentPosition) -> [PrettyField] -> String
showFields rann = showFields' rann (const id) 4

-- | Only for the prototype.
--   The printer's printing order depends on the order of 'Applicative' effects in the 'FieldGrammar' definition.
sortPrettyFields :: [PrettyFieldWith Conc] -> [PrettyFieldWith Conc]
sortPrettyFields =
  map sortPrettySections
  . sortOn
    ( \case
      PrettyField (p, _) _ -> p
      PrettySection (p, _) _ _ -> p
    )

sortPrettySections :: PrettyFieldWith Conc -> PrettyFieldWith Conc
sortPrettySections f = case f of
  PrettySection n args fls -> PrettySection n args (sortPrettyFields fls)
  _ -> f

-- | Only for the prototype.
--   We use zeroPos (Position 0 0) as a marker for data that shouldn't be printed.
--   This function filters them out.
filterFields :: [PrettyFieldWith Conc] -> [PrettyFieldWith Conc]
filterFields = mapMaybe $ \field -> case field of
  PrettyField (fnamePos, _) _ -> do
    guard (fnamePos /= zeroPos)
    pure field
  PrettySection sname args fields -> do
    let fields' = filterFields fields
    guard (not (null fields'))
    pure (PrettySection sname args fields')

exactShowFields :: [PrettyFieldWith Conc] -> String
exactShowFields =
  T.unpack
    . EPP.renderText
    . prettyFieldsToExactDoc

prettyFieldsToExactDoc :: [PrettyFieldWith Conc] -> ExactDoc
prettyFieldsToExactDoc = mconcat . snd . exactRenderPrettyFields ctx0
  where
    ctx0 = (Nothing, Nothing)

-- | 'showFields' with user specified indentation.
showFields'
  :: (ann -> CommentPosition)
  -- ^ Convert an annotation to lined to precede the field or section.
  -> (ann -> [String] -> [String])
  -- ^ Post-process non-annotation produced lines.
  -> Int
  -- ^ Indentation level.
  -> [PrettyField]
  -- ^ Fields/sections to show.
  -> String
showFields' rann post n = unlines . renderFields (Opts rann indent post)
  where
    -- few hardcoded, "unrolled"  variants.
    indent
      | n == 4 = indent4
      | n == 2 = indent2
      | otherwise = (replicate (max n 1) ' ' ++)

    indent4 :: String -> String
    indent4 [] = []
    indent4 xs = ' ' : ' ' : ' ' : ' ' : xs

    indent2 :: String -> String
    indent2 [] = []
    indent2 xs = ' ' : ' ' : xs

data Opts ann = Opts
  { _optAnnotation :: ann -> CommentPosition
  , _optIndent :: String -> String
  , _optPostprocess :: ann -> [String] -> [String]
  }

renderFields :: Opts ann -> [PrettyField] -> [String]
renderFields opts fields = flattenBlocks blocks
  where
    len = maxNameLength 0 fields
    blocks =
      filter (not . null . _contentsBlock) $ -- empty blocks cause extra newlines #8236
        map (renderField opts len) fields

    maxNameLength !acc [] = acc
    maxNameLength !acc (PrettyField name _ : rest) = maxNameLength (max acc (BS.length name)) rest
    maxNameLength !acc (PrettySection{} : rest) = maxNameLength acc rest

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

renderField :: Opts ann -> Int -> PrettyField -> Block
renderField (Opts _ indent _) fw (PrettyField name doc) =
  Block before after content
  where
    content = lines'
    before = NoMargin

    (lines', after) = case lines narrow of
      [] -> ([name' ++ ":"], NoMargin)
      [singleLine]
        | length singleLine < 60 ->
            ([name' ++ ": " ++ replicate (fw - length name') ' ' ++ narrow], NoMargin)
      _ -> ((name' ++ ":") : map indent (lines (PP.render doc)), Margin)

    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style{PP.lineLength = PP.lineLength PP.style - fw}
renderField opts@(Opts _ indent _) _ (PrettySection name args fields) =
  Block Margin Margin $
    [PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args]
      ++ map indent (renderFields opts fields)
  where

-------------------------------------------------------------------------------
-- Transform from Parsec.Field
-------------------------------------------------------------------------------

genericFromParsecFields
  :: Applicative f
  => (FieldName -> [P.FieldLine ann] -> f PP.Doc)
  -- ^ transform field contents
  -> (FieldName -> [P.SectionArg ann] -> f [PP.Doc])
  -- ^ transform section arguments
  -> [P.Field ann]
  -> f [PrettyField]
genericFromParsecFields f g = goMany
  where
    goMany = traverse go

    go (P.Field (P.Name _ name) fls) = PrettyField name <$> f name fls
    go (P.Section (P.Name _ name) secargs fs) = PrettySection name <$> g name secargs <*> goMany fs

-- | Used in 'fromParsecFields'.
prettyFieldLines :: FieldName -> [P.FieldLine ann] -> PP.Doc
prettyFieldLines _ fls =
  PP.vcat
    [ PP.text $ fromUTF8BS bs
    | P.FieldLine _ bs <- fls
    ]

-- | Used in 'fromParsecFields'.
prettySectionArgs :: FieldName -> [P.SectionArg ann] -> [PP.Doc]
prettySectionArgs _ = map $ \case
  P.SecArgName _ bs -> showToken $ fromUTF8BS bs
  P.SecArgStr _ bs -> showToken $ fromUTF8BS bs
  P.SecArgOther _ bs -> PP.text $ fromUTF8BS bs

-- | Simple variant of 'genericFromParsecField'
fromParsecFields :: [P.Field ann] -> [PrettyField]
fromParsecFields =
  runIdentity
    . genericFromParsecFields
      (Identity .: prettyFieldLines)
      (Identity .: prettySectionArgs)
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)

type PrettyFieldPositionContext =
  ( Maybe (PrettyFieldWith Conc)
  , Maybe (Position, PP.Doc)
  )

placeAt :: Position -> ExactDoc -> ExactDoc
placeAt (Position r c) = EPP.place r c

-- | Post condition: Fields are sorted in ascending order
exactRenderPrettyFields
  :: PrettyFieldPositionContext
  -> [PrettyFieldWith Conc]
  -> (PrettyFieldPositionContext, [ExactDoc])
exactRenderPrettyFields ctx0 = foldr (flip go) state0
  where
    state0 :: (PrettyFieldPositionContext, [ExactDoc])
    state0 = (ctx0, [])

    go (ctx, processed) field =
      let (ctx', field') = exactRenderPrettyField ctx field
       in (ctx', field' : processed)

exactRenderPrettyField
  :: PrettyFieldPositionContext
  -> PrettyFieldWith Conc
  -> (PrettyFieldPositionContext, ExactDoc)
exactRenderPrettyField ctx0@(_, lastFieldLine) field = case field of
  -- Absorb empty
  PrettyField (pos, fieldName) fieldLines ->
    let ctx' :: PrettyFieldPositionContext
        fieldLines' :: ExactDoc
        (ctx', fieldLines') =
          exactRenderPrettyFieldLines (Just field, lastFieldLine) fieldLines

        fieldNamePosition :: Position
        fieldNamePosition = pos

        fieldLinesFirstPos :: Position
        fieldLinesFirstPos = fst fieldLines

        -- The fieldLines are all patched and we only need to concat them together
        fieldLinesFinal :: ExactDoc
        fieldLinesFinal = placeAt fieldLinesFirstPos $ fieldLines'

        docOut :: ExactDoc
        docOut =
          placeAt fieldNamePosition $
            EPP.text (T.pack $ fromUTF8BS fieldName <> ":") <> fieldLinesFinal
     in (ctx', docOut)
  PrettySection (sectionNamePosition, fieldName) sectionArgs fields ->
    let ctx' :: PrettyFieldPositionContext
        fields' :: [ExactDoc]
        (ctx', fields') = exactRenderPrettyFields (Just field, lastFieldLine) fields

        fieldsFirstPosition :: Maybe Position
        fieldsFirstPosition = prettyFieldPosition <=< safeHead $ fields

        guessedIndentation :: Int
        guessedIndentation = fromMaybe 4 $ subtract 1 . positionCol <$> fieldsFirstPosition

        -- TODO(leana8959): section args are currently not exactly positioned
        fieldsFinal :: ExactDoc
        fieldsFinal =
          maybe id placeAt fieldsFirstPosition $
            mconcat fields'

        docOut :: ExactDoc
        docOut =
          placeAt sectionNamePosition $
            EPP.text (T.pack $ fromUTF8BS fieldName)
            -- HACK: Prepend a space when the argument is not null.
              <> ( let args = mconcat (map docToExactDoc sectionArgs)
                   in if args == EPP.Nil then mempty else EPP.text " " <> args
                 )
              <> EPP.nest guessedIndentation fieldsFinal
     in ( ctx'
        , docOut
        )

-- | Post condition: fieldlines are sorted in ascending order
-- exactRenderPrettyFieldLines
--   :: PrettyFieldPositionContext
--   -> (Position, Doc)
--   -> (PrettyFieldPositionContext, [ExactDoc])
-- exactRenderPrettyFieldLines ctx0 = fmap reverse . foldl go state0
--   where
--     state0 :: (PrettyFieldPositionContext, [ExactDoc])
--     state0 = (ctx0, [])
--
--     go (ctx, processed) fieldLine =
--       let (ctx', fieldLine') = exactRenderPrettyFieldLine ctx fieldLine
--        in (ctx', fieldLine' : processed)
exactRenderPrettyFieldLines
  :: PrettyFieldPositionContext
  -> (Position, Doc)
  -> (PrettyFieldPositionContext, ExactDoc)
exactRenderPrettyFieldLines (lastField, _) fieldLine@(_, doc) =
  let
    -- lastPosition :: Maybe Position
    -- lastPosition = liftA2 max (fst <$> lastFieldLine) (prettyFieldPosition =<< lastField)

    fieldLinePosition :: Position
    fieldLinePosition = fst fieldLine

    ctx' :: PrettyFieldPositionContext
    ctx' = (lastField, Just fieldLine)

    fieldLine' :: ExactDoc
    fieldLine' = placeAt fieldLinePosition $ docToExactDoc doc
   in
    (ctx', fieldLine')

-- sortPrettyFields :: [PrettyFieldWith Conc] -> [PrettyFieldWith Conc]
-- sortPrettyFields = List.sortOn $ fromMaybe zeroPos . prettyFieldPosition

-- sortPrettyFieldLines :: [(Position, Doc)] -> [(Position, Doc)]
-- sortPrettyFieldLines = List.sortOn (fromMaybe zeroPos . fst)

docToExactDoc :: PP.Doc -> ExactDoc
docToExactDoc = EPP.multilineText . T.lines . T.pack . PP.renderStyle PP.style
