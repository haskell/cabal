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
    PrettyField (..),
    showFields,
    showFields',
    -- * Transformation from 'P.Field'
    fromParsecFields,
    genericFromParsecFields,
    prettyFieldLines,
    prettySectionArgs,
    ) where

import Distribution.Compat.Prelude
import Distribution.Pretty         (showToken)
import Prelude ()

import Distribution.Fields.Field (FieldName)
import Distribution.Simple.Utils (fromUTF8BS)

import qualified Distribution.Fields.Parser as P

import qualified Data.ByteString  as BS
import qualified Text.PrettyPrint as PP

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
showFields :: (ann -> [String]) -> [PrettyField ann] -> String
showFields rann = showFields' rann (const id) 4

-- | 'showFields' with user specified indentation.
showFields'
  :: (ann -> [String])
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
  { _optAnnotation :: ann -> [String]
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
    Block before after $ comments ++ post ann lines'
  where
    comments = rann ann
    before = if null comments then NoMargin else Margin

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
    rann ann
    ++
    post ann [ PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args ]
    ++
    map indent (renderFields opts fields)

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
