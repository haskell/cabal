{-# LANGUAGE BangPatterns, GADTSyntax, ExistentialQuantification #-}
module ParseFields where

import Parser hiding (Parser)
import qualified Text.Parsec
import Control.Monad.Identity

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Map (Map)

-- | Define a set of field descriptions (a 'FieldsDescr'). This can then be used
-- with 'readFields' or 'parseCabalStyleFile'.
--
fieldsDescr :: a -> [FieldDescr a] -> FieldsDescr a
fieldsDescr empty =
    foldl' accum (FieldsDescr empty Nothing Map.empty Nothing Map.empty)
  where
    accum (FieldsDescr e fs fo ss so) (FieldDescr fname set parser) =
      FieldsDescr e (Map.insert fname (set, parser) fs) fo ss so

    accum (FieldsDescr e fs fo ss so) (FieldDescrOther set parser) =
      FieldsDescr e fs (Just (set, parser)) ss so


-- | Describe an individual named field.
--
fieldDescr ::  String -> (a -> b -> a) -> FParser s b  -> FieldDescr a
fieldDescr = FieldDescr

-- | Describe what to do with fields other than those named with 'fieldDescr'.
--
fieldDescrOther :: (a -> b -> a) -> (String -> FParser s b) -> FieldDescr a
fieldDescrOther = FieldDescrOther

sectionDescr :: String -> (a -> b -> c) -> FParser s a  -> FieldsDescr b -> FieldDescr c
sectionDescr = SectionDescr

sectionDescrOther :: (a -> b -> c) -> (String -> FParser s a) -> FieldsDescr b -> FieldDescr c
sectionDescrOther = SectionDescrOther

data FieldsDescr a where
  FieldsDescr :: a -> (a -> b -> a) -> Map FieldName (FieldDescr b) -> FieldsDescr a

data FieldDescr a where
  FieldDescr        :: String -> (a -> b -> a) -> FParser [FieldLine] a -> FieldDescr a
  FieldDescrOther   :: (a -> b -> a) -> (String -> FParser s b) -> FieldDescr a
  SectionDescr      :: String -> (a -> b -> c)  -> FParser s a  -> FieldsDescr b -> FieldDescr c
  SectionDescrOther :: (a -> b -> c) -> (String -> FParser s a) -> FieldsDescr b -> FieldDescr c


data Ex = Ex { exField1 :: Int, exField2 :: String }

example :: FieldsDescr Ex
example =
  fieldsDescr (Ex 0 "")
    [ fieldDescr "field1" (\e x -> e { exField1 = x })
                 (\_ -> ParseOk [] 42)
    , fieldDescr "field2" (\e x -> e { exField2 = x })
                 (\_ -> ParseOk [] "foo!")
    , fieldDescrOther const
                 (\f _ -> ParseOk ["Unexpected field " ++ f] ())
    ]

type ParseError   = ()
type ParseWarning = ()
data ParseResult a = ParseFailed [ParseError] | ParseOk [ParseWarning] a

--type Parsec a = Text.Parsec.Stream s m Char => Text.Parsec.ParsecT s u Identity a
--type Parsec s a = Text.Parsec.ParsecT s () Identity a
type FParser s a = s -> ParseResult a

-- | Parse a file in Cabal-style syntax.
--
-- You supply a description of the fields and sections that are allowed.
--
parseCabalStyleFile :: FieldsDescr a -> ByteString -> ParseResult a
parseCabalStyleFile descrs input =
  outlineParseCabalStyleFile input >>= parseFields descr

-- | Do an \"outline\" parse of a file in Cabal-style syntax.
--
-- It just parses the structure, but not the contents of the individual fields.
-- This can be useful if you need to treat the file in a generic way and do not
-- know what is in every field, for example if you want to edit just a few
-- specific fields and write the file out again.
--
outlineParseCabalStyleFile :: ByteString -> ParseResult [Field]
outlineParseCabalStyleFile = undefined

type FieldName = ByteString

data FieldsDescr a where
  FieldsDescr :: a -> (a -> b -> a) -> Map FieldName (FieldDescr b) -> FieldsDescr a

data FieldDescr a where
  FieldDescr      :: FParser [FieldLine] a -> FieldDescr a
  SubsectionDescr :: (c -> b -> a) -> FParser [SectionArg] c -> FieldsDescr b -> FieldDescr a


parseFields :: FieldsDescr a -> [Field] -> ParseResult a
parseFields (FieldsDescr empty add fielddescrmap) = undefined {- go [] empty
  where
    go []   !acc [] = Right acc
    go errs !acc [] = Left  (reverse errs)

    go errs !acc (Field (Name pos name) content : fields) =
      case M.lookup name fielddescrmap of
        Just (FieldDescr parseField)
          | Right x <- parseField content
          -> go errs (acc `add` x) fields

          | otherwise
          -> go (err:errs) acc fields
               where
                 err = () -- parse error in field name at pos: content
        _ -> go (err:errs) acc fields
               where
                 err = () -- unexpected field name at pos

    go errs !acc (Section (Name _ name) args fields' : fields) =
      case M.lookup name fielddescrmap of        
        Just (SubsectionDescr subsec parseArgs fielddescrs)
          | Right a <- parseArgs args
          -> case parseFields fielddescrs fields' of
               Right x    -> go errs (acc `add` subsec a x) fields
               Left errs' -> go (errs'++errs) acc fields

          | otherwise
          -> go (err:errs) acc fields
               where
                 err = () -- parse error in field name at pos: content

        _ -> go (err:errs) acc fields
               where
                 err = () -- unexpected section name at pos
-}
