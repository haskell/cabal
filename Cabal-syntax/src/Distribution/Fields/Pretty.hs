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

import Distribution.Parsec.Position
import Distribution.Fields.Field (FieldName, Name)
import Distribution.Utils.Generic (fromUTF8BS)

import qualified Distribution.Types.Modify as Mod
import Distribution.Types.Modify (AttachPosition, AnnotateWith)
import qualified Distribution.Fields.Parser as P

import qualified Data.ByteString as BS
import qualified Text.PrettyPrint as PP

-- | This type is used to discern when a comment block should go
--   before or after a cabal-like file field, otherwise it would
--   be hardcoded to a single position. It is often used in
--   conjunction with @PrettyField@.
data CommentPosition = CommentBefore [String] | CommentAfter [String] | NoComment

type PrettyField = PrettyFieldWith Mod.HasNoAnn

-- NOTE(leana8959): some pretty field considerations
--  - do section args need to be a _list_ of PP.Doc
--  - do we need pretty empty with exact doc
--
-- TODO(leana8959): we need to reproduce the field line
--  - each Doc in field (FieldLine) should come with its positioning
data PrettyFieldWith (mod :: Mod.HasAnnotation)
  = PrettyField (AttachPosition mod FieldName) PP.Doc
  | PrettySection FieldName [PP.Doc] [PrettyFieldWith mod]
  | PrettyEmpty

deriving instance Show (PrettyFieldWith Mod.HasAnn)

-- | Prettyprint a list of fields.
--
-- Note: the first argument should return 'String's without newlines
-- and properly prefixes (with @--@) to count as comments.
-- This unsafety is left in place so one could generate empty lines
-- between comment lines.
showFields :: (ann -> CommentPosition) -> [PrettyField] -> String
showFields rann = showFields' rann (const id) 4

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
