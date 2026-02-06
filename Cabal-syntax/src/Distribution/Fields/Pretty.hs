{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , PrettyField (..)
  , PrettyFieldLine (..)
  , showFields
  , showFields'
  , showFieldsWithTrivia

    -- * Transformation from 'P.Field'
  , fromParsecFields
  , genericFromParsecFields
  , prettyFieldLines
  , prettySectionArgs
  ) where

import Control.Monad
import Distribution.Compat.Prelude
import Distribution.Pretty (showToken)
import Distribution.Parsec.Position
import Prelude ()

import Distribution.Types.Annotation

import Distribution.Fields.Field (FieldName)
import Distribution.Utils.Generic (fromUTF8BS)

import qualified Distribution.Fields.Parser as P

import qualified Data.List as List (tail)
import qualified Data.ByteString as BS
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

data PrettyField ann
  = PrettyField ann FieldName [PrettyFieldLine ann]
  | PrettySection ann FieldName [PP.Doc] [PrettyField ann]
  | PrettyEmpty
  deriving (Functor, Foldable, Traversable, Show {- NOTE(leana8959): for debugging -})

prettyFieldAnn :: PrettyField ann -> Maybe ann
prettyFieldAnn (PrettyField ann _ _) = Just ann
prettyFieldAnn (PrettySection ann _ _ _) = Just ann
prettyFieldAnn _ = Nothing

-- | Prettyprint a list of fields.
--
-- Note: the first argument should return 'String's without newlines
-- and properly prefixes (with @--@) to count as comments.
-- This unsafety is left in place so one could generate empty lines
-- between comment lines.
showFields :: (ann -> CommentPosition) -> [PrettyField ann] -> String
showFields rann = showFields' rann (const Nothing) (const $ const id)

showFieldsWithTrivia :: [PrettyField Trivia] -> String
showFieldsWithTrivia = showFields' (const NoComment) atPosition postProcess
  where
    postProcess :: Maybe Position -> Trivia -> PP.Doc -> PP.Doc
    postProcess prevPos trivia doc =
      let mDiff = liftA2 offset (atPosition trivia) prevPos
          offset x@(Position rx cx) y@(Position ry cy) = (rx - ry, if rx /= ry then cx else cx - cy)
      in  case mDiff of
            Just (rDiff, cDiff) ->
              (\x -> trace ("patching with " <> show (rDiff - 2, cDiff - 1, doc) ) x)
              $ foldr (.) id
                ( 
                replicate (rDiff -  1) (PP.text "" PP.$$) ++
                replicate (cDiff - 1) (PP.text " " <>)
                )
                doc
            Nothing -> case atPosition trivia of
              Just (Position _ col) ->
                -- No previous position to calculate line jump, but still compute column offset
                (\x -> trace ("patching with " <> show (col - 1, doc)) x)
                $ foldr (.) id
                  ( replicate (col - 1) (PP.text " " <>)
                  )
                  doc

-- | 'showFields' with user specified indentation.
showFields'
  :: (ann -> CommentPosition)
  -- ^ Convert an annotation to lined to precede the field or section.
  -> (ann -> Maybe Position)
  -- ^ Extract the position information from the annotation
  -> (Maybe Position -> ann -> PP.Doc -> PP.Doc)
  -- ^ Post-process non-annotation produced lines.
  -> [PrettyField ann]
  -- ^ Fields/sections to show.
  -> String
showFields' rann getPos post = concat . renderFields Nothing (Opts rann getPos post)
  where

data Opts ann = Opts
  { _optAnnotation :: ann -> CommentPosition
  , _optPosition :: ann -> Maybe Position
  , _optPostprocess :: Maybe Position -> ann -> PP.Doc -> PP.Doc
  }

renderFields :: forall ann. Maybe Position -> Opts ann -> [PrettyField ann] -> [String]
renderFields prevPos opts fields = flattenBlocks blocks
  where
    len = maxNameLength 0 fields

    posFromPrettyField :: PrettyField ann -> Maybe Position
    posFromPrettyField = _optPosition opts <=< prettyFieldAnn

    blocks =
      filter (not . null . _contentsBlock) -- empty blocks cause extra newlines #8236
        $ map
            ( \(prevPos, x) ->
                trace ("zipping with prevPos\n" <> show prevPos) $
                  renderField opts prevPos len x
            )
        $ zip (Nothing : map posFromPrettyField fields) fields

    maxNameLength !acc [] = acc
    maxNameLength !acc (PrettyField _ name _ : rest) = maxNameLength (max acc (BS.length name)) rest
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

renderField :: Opts ann -> Maybe Position -> Int -> PrettyField ann -> Block
renderField opts@(Opts rann getPos postWithPrev) prevPos fw (PrettyField ann name fieldLines) =
  Block before after content
  where
    fieldLines' :: [String]
    fieldLines' =
        map
          ( \(prevLinePos, fl) ->
              renderPrettyFieldLine opts prevLinePos fw fl
          )
        $ zip
          (prevPos : map (getPos . prettyFieldLineAnn) fieldLines)
          fieldLines

    post = postWithPrev prevPos
    content = case comments of
      CommentBefore cs -> cs ++ lines'
      CommentAfter cs -> lines' ++ cs
      NoComment -> lines'
    comments = rann ann
    before = case comments of
      CommentBefore [] -> NoMargin
      CommentAfter [] -> NoMargin
      NoComment -> NoMargin
      _ -> Margin

    -- TODO(leana8959): use the pretty library to render the field names
    (lines', after) = case fieldLines' of
      [] -> ([name' ++ ":"], NoMargin)
      -- [singleLine]
      --   | length singleLine < 60 ->
      --       ([name' ++ ": " ++ replicate (fw - length name') ' ' ++ concat fieldLines'], NoMargin)

            -- TODO(leana8959): fix indentation with exact positioning

      _ -> ((name' ++ ":") : "\n" : {- map indent -} fieldLines', Margin)

    name' = fromUTF8BS name

renderField opts@(Opts rann getPos postWithPrev) prevPos _ (PrettySection ann name args fields) =
  Block Margin Margin $
    attachComments
      [PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args]
      -- TODO(leana8959): fix indentation with exact positioning
      ++ {- map indent -} (renderFields (getPos ann) opts fields)
  where
    post = postWithPrev prevPos
    attachComments content = case rann ann of
      CommentBefore cs -> cs ++ content
      CommentAfter cs -> content ++ cs
      NoComment -> content
renderField _ _ _ PrettyEmpty = Block NoMargin NoMargin mempty

-- |
-- Invariant: a PrettyFieldLine is never more than one line
renderPrettyFieldLine :: Opts ann -> Maybe Position -> Int -> PrettyFieldLine ann -> String
renderPrettyFieldLine (Opts rann _ postWithPrevPos) prevPos fw (PrettyFieldLine ann doc) =
  let postProcess = postWithPrevPos prevPos
      narrowStyle :: PP.Style
      narrowStyle = PP.style{PP.lineLength = PP.lineLength PP.style - fw}

  in
      (\x -> trace ("rendered PFL " <> show x) x)
      $ PP.renderStyle narrowStyle
      $ postProcess ann
      $ doc

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
  where go (P.FieldLine ann bs) =
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
