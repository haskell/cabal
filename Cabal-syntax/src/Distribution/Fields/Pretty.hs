{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

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
  , showFields
  , showFields'

    -- * Transformation from 'P.Field'
  , fromParsecFields
  , genericFromParsecFields
  , prettyFieldLines
  , prettySectionArgs
  ) where

import Distribution.Compat.Prelude
import Distribution.Pretty (showToken)
import Prelude ()
import Control.Monad ((<=<))

import Distribution.Parsec.Position
import Distribution.Fields.Field (FieldName, Name)
import Distribution.Utils.Generic (fromUTF8BS, safeHead)

import qualified Data.List as List
import qualified Data.Text as T

import Distribution.Trivia
import qualified Distribution.Types.Modify as Mod
import Distribution.Types.Modify (AttachPosition, AnnotateWith)
import Distribution.Pretty.ExactDoc (ExactDoc)
import qualified Distribution.Pretty.ExactDoc as EPP
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import qualified Distribution.Fields.Parser as P

import qualified Data.ByteString as BS
import qualified Text.PrettyPrint as PP

-- | This type is used to discern when a comment block should go
--   before or after a cabal-like file field, otherwise it would
--   be hardcoded to a single position. It is often used in
--   conjunction with @PrettyField@.
data CommentPosition = CommentBefore [String] | CommentAfter [String] | NoComment

type PrettyField = PrettyFieldWith Mod.HasNoAnn

deriving instance Eq (PrettyFieldWith Mod.HasAnn)

-- NOTE(leana8959): some pretty field considerations
--  - do section args need to be a _list_ of PP.Doc
--  - do we need pretty empty with exact doc
--
-- TODO(leana8959): we need to reproduce the field line
--  - each Doc in field (FieldLine) should come with its positioning
data PrettyFieldWith (mod :: Mod.HasAnnotation)
  = PrettyField (AttachPosition mod FieldName) (AttachPosition mod PP.Doc)
  | PrettySection (AttachPosition mod FieldName) [PP.Doc] [PrettyFieldWith mod]
  | PrettyEmpty

prettyFieldPosition :: PrettyFieldWith Mod.HasAnn -> Maybe Position
prettyFieldPosition (PrettyField (pos, _) _) = Just pos
prettyFieldPosition _ = Nothing

prettySectionPosition :: PrettyFieldWith Mod.HasAnn -> Maybe Position
prettySectionPosition (PrettySection (pos, _) _ _) = Just pos
prettySectionPosition _ = Nothing

deriving instance Show (PrettyFieldWith Mod.HasAnn)

-- | Prettyprint a list of fields.
--
-- Note: the first argument should return 'String's without newlines
-- and properly prefixes (with @--@) to count as comments.
-- This unsafety is left in place so one could generate empty lines
-- between comment lines.
showFields :: (ann -> CommentPosition) -> [PrettyField] -> String
showFields rann = showFields' rann (const id) 4

exactShowFields :: [PrettyFieldWith Mod.HasAnn] -> String
exactShowFields =
      T.unpack
    . EPP.renderText
    . prettyFieldsToExactDoc
  where
    ctx0 = (Nothing, Nothing)

prettyFieldsToExactDoc :: [PrettyFieldWith Mod.HasAnn] -> ExactDoc
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
    maxNameLength !acc (PrettyEmpty : rest) = maxNameLength acc rest

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
renderField (Opts rann indent post) fw (PrettyField name doc) =
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
renderField opts@(Opts rann indent post) _ (PrettySection name args fields) =
  Block Margin Margin $
      [PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args]
      ++ map indent (renderFields opts fields)
  where
renderField _ _ PrettyEmpty = Block NoMargin NoMargin mempty

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
  ( Maybe (PrettyFieldWith Mod.HasAnn)
  , Maybe (Position, PP.Doc)
  )

placeAt :: Position -> ExactDoc -> ExactDoc
placeAt (Position r c) = EPP.place r c

-- | Post condition: Fields are sorted in ascending order
exactRenderPrettyFields
  :: PrettyFieldPositionContext
  -> [PrettyFieldWith Mod.HasAnn]
  -> (PrettyFieldPositionContext, [ExactDoc])
exactRenderPrettyFields ctx0 = fmap reverse . foldl go state0 . sortPrettyFields
  where
    state0 :: (PrettyFieldPositionContext, [ExactDoc])
    state0 = (ctx0, [])

    go (ctx, processed) field =
      let (ctx', field') = exactRenderPrettyField ctx field
       in (ctx', field' : processed)

exactRenderPrettyField
  :: PrettyFieldPositionContext
  -> PrettyFieldWith Mod.HasAnn
  -> (PrettyFieldPositionContext, ExactDoc)
exactRenderPrettyField ctx0@(lastField, lastFieldLine) field = case field of
  -- Absorb empty
  PrettyEmpty -> (ctx0, mempty)
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

        lastPosition :: Maybe Position
        lastPosition = fst <$> lastFieldLine <|> (prettyFieldPosition =<< lastField)

        isFirst = lastField == Nothing && lastFieldLine == Nothing

        docOut :: ExactDoc
        docOut = placeAt fieldNamePosition $
            EPP.text (T.pack $ fromUTF8BS fieldName <> ":") <> fieldLinesFinal
     in (ctx', docOut)
  PrettySection (pos, fieldName) sectionArgs fields ->
    let ctx' :: PrettyFieldPositionContext
        fields' :: [ExactDoc]
        (ctx', fields') = exactRenderPrettyFields (Just field, lastFieldLine) fields

        sectionNamePosition :: Maybe Position
        sectionNamePosition = prettySectionPosition field

        lastPosition :: Maybe Position
        lastPosition = fst <$> lastFieldLine <|> (prettyFieldPosition =<< lastField)

        fieldsFirstPosition :: Maybe Position
        fieldsFirstPosition = prettyFieldPosition <=< safeHead $ fields

        guessedIndentation :: Int
        guessedIndentation = fromMaybe 4 $ subtract 1 . positionCol <$> fieldsFirstPosition

        -- TODO(leana8959): section args are currently not exactly positioned
        fieldsFinal :: ExactDoc
        fieldsFinal =
          maybe id placeAt fieldsFirstPosition $
            mconcat fields'

        isFirst = lastField == Nothing && lastFieldLine == Nothing

        docOut :: ExactDoc
        docOut =
          maybe id placeAt sectionNamePosition $
            EPP.text (T.pack $ fromUTF8BS fieldName)
              <> mconcat (map docToExactDoc sectionArgs)
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
exactRenderPrettyFieldLines (lastField, lastFieldLine) fieldLine@(_, doc) =
  let lastPosition :: Maybe Position
      lastPosition = liftA2 max (fst <$> lastFieldLine) (prettyFieldPosition =<< lastField)

      fieldLinePosition :: Position
      fieldLinePosition = fst fieldLine

      ctx' :: PrettyFieldPositionContext
      ctx' = (lastField, Just fieldLine)

      fieldLine' :: ExactDoc
      fieldLine' = placeAt fieldLinePosition $ docToExactDoc doc
   in (ctx', fieldLine')

sortPrettyFields :: [PrettyFieldWith Mod.HasAnn] -> [PrettyFieldWith Mod.HasAnn]
sortPrettyFields = List.sortOn $ fromMaybe zeroPos . prettyFieldPosition

-- sortPrettyFieldLines :: [(Position, Doc)] -> [(Position, Doc)]
-- sortPrettyFieldLines = List.sortOn (fromMaybe zeroPos . fst)

docToExactDoc :: PP.Doc -> ExactDoc
docToExactDoc = EPP.multilineText . T.lines . T.pack . PP.renderStyle PP.style
