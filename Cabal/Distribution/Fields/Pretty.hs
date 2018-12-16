{-# LANGUAGE BangPatterns #-}
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

import Data.Functor.Identity       (Identity (..))
import Distribution.Compat.Prelude
import Distribution.Pretty         (showToken)
import Prelude ()

import Distribution.Fields.Field (FieldName)
import Distribution.Simple.Utils (fromUTF8BS)

import qualified Distribution.Fields.Parser as P

import qualified Data.ByteString  as BS
import qualified Text.PrettyPrint as PP

data PrettyField
    = PrettyField FieldName PP.Doc
    | PrettySection FieldName [PP.Doc] [PrettyField]

-- | Prettyprint a list of fields.
showFields :: [PrettyField] -> String
showFields = showFields' 4

-- | 'showFields' with user specified indentation.
showFields' :: Int -> [PrettyField] -> String
showFields' n = unlines . renderFields indent where
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

renderFields :: (String -> String) -> [PrettyField] -> [String]
renderFields indent fields = flattenBlocks $ map (renderField indent len) fields
  where
    len = maxNameLength 0 fields

    maxNameLength !acc []                          = acc
    maxNameLength !acc (PrettyField name _ : rest) = maxNameLength (max acc (BS.length name)) rest
    maxNameLength !acc (PrettySection {}   : rest) = maxNameLength acc rest

-- | Block of lines,
-- Boolean parameter tells whether block should be surrounded by empty lines
data Block = Block Bool [String]

flattenBlocks :: [Block] -> [String]
flattenBlocks = go0 where
    go0 [] = []
    go0 (Block surr strs : blocks) = strs ++ go surr blocks

    go _surr' [] = []
    go  surr' (Block surr strs : blocks) = ins $ strs ++ go surr blocks where
        ins | surr' || surr = ("" :)
            | otherwise     = id

renderField :: (String -> String) -> Int -> PrettyField -> Block
renderField indent fw (PrettyField name doc) = Block False $ case lines narrow of
    []           -> [ name' ++ ":" ]
    [singleLine] | length singleLine < 60
                 -> [ name' ++ ": " ++ replicate (fw - length name') ' ' ++ narrow ]
    _            -> (name' ++ ":") : map indent (lines (PP.render doc))
  where
    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style { PP.lineLength = PP.lineLength PP.style - fw }

renderField indent _ (PrettySection name args fields) = Block True $
    [ PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args ]
    ++
    (map indent $ renderFields indent fields)

-------------------------------------------------------------------------------
-- Transform from Parsec.Field
-------------------------------------------------------------------------------

genericFromParsecFields
    :: Applicative f
    => (FieldName -> [P.FieldLine ann] -> f PP.Doc)     -- ^ transform field contents
    -> (FieldName -> [P.SectionArg ann] -> f [PP.Doc])  -- ^ transform section arguments
    -> [P.Field ann]
    -> f [PrettyField]
genericFromParsecFields f g = goMany where
    goMany = traverse go

    go (P.Field (P.Name _ann name) fls)          = PrettyField name <$> f name fls
    go (P.Section (P.Name _ann name) secargs fs) = PrettySection name <$> g name secargs <*> goMany fs

-- | Used in 'fromParsecFields'.
prettyFieldLines :: FieldName -> [P.FieldLine ann] -> PP.Doc
prettyFieldLines _ fls = PP.vcat
    [ PP.text $ fromUTF8BS bs
    | P.FieldLine _ bs <- fls
    ]

-- | Used in 'fromParsecFields'.
prettySectionArgs :: FieldName -> [P.SectionArg ann] -> [PP.Doc]
prettySectionArgs _ = map $ \sa -> case sa of
    P.SecArgName _ bs  -> showToken $ fromUTF8BS bs
    P.SecArgStr _ bs   -> showToken $ fromUTF8BS bs
    P.SecArgOther _ bs -> PP.text $ fromUTF8BS bs

-- | Simple variant of 'genericFromParsecField'
fromParsecFields :: [P.Field ann] -> [PrettyField]
fromParsecFields = runIdentity . genericFromParsecFields
    (Identity .: prettyFieldLines)
    (Identity .: prettySectionArgs)
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)
