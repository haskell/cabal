{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
-- | Cabal-like file AST types: 'Field', 'Section' etc,
--
-- This (intermediate) data type is used for pretty-printing.
--
-- @since 3.0.0.0
--
module Distribution.Fields.Pretty (
    -- * Fields
    CommentPosition (..),
    PrettyField (..),
    showFields,
    showFields',
    -- * Transformation from 'P.Field'
    fromParsecFields,
    genericFromParsecFields,
    prettyFieldLines,
    prettySectionArgs,
    exactShow
    ) where

import Control.Monad (mapM_)
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State

import Distribution.Compat.Prelude
import Distribution.Parsec.Position (Position (..))
import Distribution.Pretty         (showToken)
import Prelude ()

import Distribution.Fields.Field (FieldName)
import Distribution.Simple.Utils (fromUTF8BS)

import qualified Distribution.Fields.Parser as P

import qualified Data.ByteString  as BS
import qualified Text.PrettyPrint as PP

-- | This type is used to discern when a comment block should go 
--   before or after a cabal-like file field, otherwise it would
--   be hardcoded to a single position. It is often used in
--   conjunction with @PrettyField@.
data CommentPosition = CommentBefore [String] | CommentAfter [String] | NoComment

data PrettyField ann
    = PrettyField ann FieldName PP.Doc
    | PrettySection ann FieldName [PP.Doc] [PrettyField ann]
    | PrettyEmpty
  deriving (Functor, Foldable, Traversable)

-- | Prettyprint a list of fields.
--
-- Note: the first argument should return 'String's without newlines
-- and properly prefixes (with @--@) to count as comments.
-- This unsafety is left in place so one could generate empty lines
-- between comment lines.
--
showFields :: (ann -> CommentPosition) -> [PrettyField ann] -> String
showFields rann = showFields' rann (const id) 4

-- | 'showFields' with user specified indentation.
showFields'
  :: (ann -> CommentPosition)
     -- ^ Convert an annotation to lined to preceed the field or section.
  -> (ann -> [String] -> [String])
     -- ^ Post-process non-annotation produced lines.
  -> Int
     -- ^ Indentation level.
  -> [PrettyField ann]
     -- ^ Fields/sections to show.
  -> String
showFields' rann post n = unlines . renderFields (Opts rann indent post)
  where
    -- few hardcoded, "unrolled"  variants.
    indent | n == 4    = indent4
           | n == 2    = indent2
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

renderFields :: Opts ann -> [PrettyField ann] -> [String]
renderFields opts fields = flattenBlocks $ map (renderField opts len) fields
  where
    len = maxNameLength 0 fields

    maxNameLength !acc []                            = acc
    maxNameLength !acc (PrettyField _ name _ : rest) = maxNameLength (max acc (BS.length name)) rest
    maxNameLength !acc (PrettySection {}   : rest)   = maxNameLength acc rest
    maxNameLength !acc (PrettyEmpty : rest) = maxNameLength acc rest

-- | Block of lines,
-- Boolean parameter tells whether block should be surrounded by empty lines
data Block = Block Margin Margin [String]

data Margin = Margin | NoMargin
  deriving Eq

-- | Collapse margins, any margin = margin
instance Semigroup Margin where
    NoMargin <> NoMargin = NoMargin
    _        <> _        = Margin

flattenBlocks :: [Block] -> [String]
flattenBlocks = go0 where
    go0 [] = []
    go0 (Block _before after strs : blocks) = strs ++ go after blocks

    go _surr' [] = []
    go  surr' (Block before after strs : blocks) = ins $ strs ++ go after blocks where
        ins | surr' <> before == Margin = ("" :)
            | otherwise                 = id

renderField :: Opts ann -> Int -> PrettyField ann -> Block
renderField (Opts rann indent post) fw (PrettyField ann name doc) =
    Block before after content
  where
    content = case comments of
      CommentBefore cs -> cs ++ post ann lines'
      CommentAfter  cs -> post ann lines' ++ cs
      NoComment        -> post ann lines'
    comments = rann ann
    before = case comments of
      CommentBefore [] -> NoMargin
      CommentAfter  [] -> NoMargin
      NoComment        -> NoMargin
      _                -> Margin

    (lines', after) = case lines narrow of
        []           -> ([ name' ++ ":" ], NoMargin)
        [singleLine] | length singleLine < 60
                     -> ([ name' ++ ": " ++ replicate (fw - length name') ' ' ++ narrow ], NoMargin)
        _            -> ((name' ++ ":") : map indent (lines (PP.render doc)), Margin)

    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style { PP.lineLength = PP.lineLength PP.style - fw }

renderField opts@(Opts rann indent post) _ (PrettySection ann name args fields) = Block Margin Margin $
    
    attachComments
      (post ann [ PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args ])
    ++
    map indent (renderFields opts fields)
  where
    attachComments content = case rann ann of
      CommentBefore cs -> cs ++ content
      CommentAfter  cs -> content ++ cs
      NoComment        -> content

renderField _ _ PrettyEmpty = Block NoMargin NoMargin mempty

-------------------------------------------------------------------------------
-- Transform from Parsec.Field
-------------------------------------------------------------------------------

genericFromParsecFields
    :: Applicative f
    => (FieldName -> [P.FieldLine ann] -> f PP.Doc)     -- ^ transform field contents
    -> (FieldName -> [P.SectionArg ann] -> f [PP.Doc])  -- ^ transform section arguments
    -> [P.Field ann]
    -> f [PrettyField ann]
genericFromParsecFields f g = goMany where
    goMany = traverse go

    go (P.Field (P.Name ann name) fls)          = PrettyField ann name <$> f name fls
    go (P.Comment _ (P.CommentLine _ _))        = pure PrettyEmpty
    go (P.Section (P.Name ann name) secargs fs) = PrettySection ann name <$> g name secargs <*> goMany fs

-- | Used in 'fromParsecFields'.
prettyFieldLines :: FieldName -> [P.FieldLine ann] -> PP.Doc
prettyFieldLines _ fls = PP.vcat
    [ PP.text $ fromUTF8BS bs
    | P.FieldLine _ bs <- fls
    ]

-- | Used in 'fromParsecFields'.
prettySectionArgs :: FieldName -> [P.SectionArg ann] -> [PP.Doc]
prettySectionArgs _ = map $ \case
    P.SecArgName _ bs  -> showToken $ fromUTF8BS bs
    P.SecArgStr _ bs   -> showToken $ fromUTF8BS bs
    P.SecArgOther _ bs -> PP.text $ fromUTF8BS bs

-- | Simple variant of 'genericFromParsecField'
fromParsecFields :: [P.Field ann] -> [PrettyField ann]
fromParsecFields = runIdentity . genericFromParsecFields
    (Identity .: prettyFieldLines)
    (Identity .: prettySectionArgs)
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)

------------------------------------------------------------------------------
-- Exact print ---------------------------------------------------------------
------------------------------------------------------------------------------

data ExactPrint = ExactPrint
  { currentPosition :: Position,
    doc :: [String]
  }
  deriving (Eq, Show)

type ExactPrinter a = State ExactPrint a

initialExactPrint :: ExactPrint
initialExactPrint = ExactPrint (Position 1 1) []

exactShow :: [P.Field Position] -> String
exactShow fields =
  let r = State.execState (mapM_ exactShowField fields) initialExactPrint
   in foldr (++) "" (reverse $ doc r)

reachPos :: Position -> ExactPrinter ()
reachPos (Position row col) = do
  reachRow row
  reachCol col

reachCol :: Int -> ExactPrinter ()
reachCol n = do
  ExactPrint {currentPosition = Position row col, doc = _doc} <- State.get
  if col == n
    then return ()
    else
      State.put $
        ExactPrint
          { currentPosition = Position row n,
            doc = replicate (n - col) ' ' : _doc
          }

reachRow :: Int -> ExactPrinter ()
reachRow n = do
  ExactPrint {currentPosition = Position row _, doc = _doc} <- State.get
  if row == n
    then return ()
    else
      State.put $
        ExactPrint
          { currentPosition = Position n 1,
            doc = replicate (n - row) '\n' : _doc
          }

write :: String -> ExactPrinter ()
write s = do
  ExactPrint {currentPosition = Position row col, doc = _doc} <- State.get
  State.put $
    ExactPrint
      { currentPosition =
          Position row (col + length s),
        doc = s : _doc
      }

exactShowField :: P.Field Position -> ExactPrinter ()
exactShowField (P.Field (P.Name p bs) fields) = do
  reachPos p
  write (fromUTF8BS bs)
  write ":"
  mapM_ exactShowFieldLine fields
exactShowField (P.Comment p comment) = do
  reachPos p
  exactShowComment comment
exactShowField (P.Section (P.Name p bs) sections fields) = do
  reachPos p
  write (fromUTF8BS bs)
  mapM_ exactShowSection sections
  mapM_ exactShowField fields

exactShowFieldLine :: P.FieldLine Position -> ExactPrinter ()
exactShowFieldLine (P.FieldLine p bs) = do
  reachPos p
  write (fromUTF8BS bs)
exactShowFieldLine (P.CommentLineInField p comment) = do
  reachPos p
  exactShowComment comment

exactShowComment :: P.CommentLine Position -> ExactPrinter ()
exactShowComment (P.CommentLine p bs) = do
  reachPos p
  write (fromUTF8BS bs)

exactShowSection :: P.SectionArg Position -> ExactPrinter ()
exactShowSection (P.SecArgName p bs) = do
  reachPos p
  write (fromUTF8BS bs)
exactShowSection (P.SecArgStr p bs) = do
  reachPos p
  write (fromUTF8BS bs)
exactShowSection (P.SecArgOther p bs) = do
  reachPos p
  write (fromUTF8BS bs)
