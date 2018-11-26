{-# LANGUAGE BangPatterns #-}
-- | Cabal-like file AST types: 'Field', 'Section' etc,
--
-- This (intermediate) data type is used for pretty-printing.
--
-- @since 3.0.0.0
--
module Distribution.Pretty.Field (
    -- * Fields
    Field (..),
    showFields,
    -- * Transformation from Parsec.Field
    genericFromParsecFields,
    fromParsecFields,
    ) where

import Data.Functor.Identity       (Identity (..))
import Distribution.Compat.Prelude
import Distribution.Pretty         (showToken)
import Prelude ()

import Distribution.Parsec.Field (FieldName)
import Distribution.Simple.Utils (fromUTF8BS)

import qualified Distribution.Parsec.Field as P

import qualified Data.ByteString  as BS
import qualified Text.PrettyPrint as PP

data Field
    = Field FieldName PP.Doc
    | Section FieldName [PP.Doc] [Field]

-- | Prettyprint a list of fields.
showFields :: [Field] -> String
showFields = unlines . renderFields

renderFields :: [Field] -> [String]
renderFields fields = flattenBlocks $ map (renderField len) fields
  where
    len = maxNameLength 0 fields

    maxNameLength !acc []                    = acc
    maxNameLength !acc (Field name _ : rest) = maxNameLength (max acc (BS.length name)) rest
    maxNameLength !acc (Section {}   : rest) = maxNameLength acc rest

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

renderField :: Int -> Field -> Block
renderField fw (Field name doc) = Block False $ case lines narrow of
    []           -> [ name' ++ ":" ]
    [singleLine] | length singleLine < 60
                 -> [ name' ++ ": " ++ replicate (fw - length name') ' ' ++ narrow ]
    _            -> (name' ++ ":") : map indent (lines (PP.render doc))
  where
    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style { PP.lineLength = PP.lineLength PP.style - fw }

renderField _ (Section name args fields) = Block True $
    [ PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args ]
    ++
    (map indent $ renderFields fields)

-- Indent with 4 spaces. See 'Distribution.Pretty.indentWith'
indent :: String -> String
indent [] = []
indent xs = ' ' : ' ' : ' ' : ' ' : xs

-------------------------------------------------------------------------------
-- Transform from Parsec.Field
-------------------------------------------------------------------------------

genericFromParsecFields
    :: Applicative f
    => (FieldName -> [P.FieldLine ann] -> f PP.Doc)     -- ^ transform field contents
    -> (FieldName -> [P.SectionArg ann] -> f [PP.Doc])  -- ^ transform section arguments
    -> [P.Field ann]
    -> f [Field]
genericFromParsecFields f g = goMany where
    goMany = traverse go

    go (P.Field (P.Name _ann name) fls)          = Field name <$> f name fls
    go (P.Section (P.Name _ann name) secargs fs) = Section name <$> g name secargs <*> goMany fs

-- | Simple variant of 'genericFromParsecField'
fromParsecFields :: [P.Field ann] -> [Field]
fromParsecFields =
    runIdentity . genericFromParsecFields (Identity .: trFls) (Identity .: trSecArgs)
  where
    trFls :: FieldName -> [P.FieldLine ann] -> PP.Doc
    trFls _ fls = PP.vcat
        [ PP.text $ fromUTF8BS bs
        | P.FieldLine _ bs <- fls
        ]

    trSecArgs :: FieldName -> [P.SectionArg ann] -> [PP.Doc]
    trSecArgs _ = map $ \sa -> case sa of
        P.SecArgName _ bs  -> showToken $ fromUTF8BS bs
        P.SecArgStr _ bs   -> showToken $ fromUTF8BS bs
        P.SecArgOther _ bs -> PP.text $ fromUTF8BS bs

    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)
