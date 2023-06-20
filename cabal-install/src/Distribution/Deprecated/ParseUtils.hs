{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- This module is meant to be local-only to Distribution...
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      :  Distribution.Deprecated.ParseUtils
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for parsing 'PackageDescription' and 'InstalledPackageInfo'.
--
-- The @.cabal@ file format is not trivial, especially with the introduction
-- of configurations and the section syntax that goes with that. This module
-- has a bunch of parsing functions that is used by the @.cabal@ parser and a
-- couple others. It has the parsing framework code and also little parsers for
-- many of the formats we get in various @.cabal@ file fields, like module
-- names, comma separated lists etc.
module Distribution.Deprecated.ParseUtils
  ( LineNo
  , PError (..)
  , PWarning (..)
  , locatedErrorMsg
  , syntaxError
  , warning
  , runP
  , runE
  , ParseResult (..)
  , parseFail
  , showPWarning
  , Field (..)
  , lineNo
  , FieldDescr (..)
  , readFields
  , parseHaskellString
  , parseTokenQ
  , parseSpaceList
  , parseOptCommaList
  , showFilePath
  , showToken
  , showFreeText
  , field
  , simpleField
  , listField
  , listFieldWithSep
  , spaceListField
  , newLineListField
  , liftField
  , readPToMaybe
  , fieldParsec
  , simpleFieldParsec
  , listFieldParsec
  , commaListFieldParsec
  , commaNewLineListFieldParsec
  , UnrecFieldParser
  ) where

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Deprecated.ReadP as ReadP hiding (get)

import Distribution.Pretty
import Distribution.ReadE
import Distribution.Utils.Generic

import System.FilePath (normalise)
import Text.PrettyPrint (Doc, comma, fsep, punctuate, sep)
import qualified Text.Read as Read

import qualified Control.Monad.Fail as Fail
import Distribution.Parsec (ParsecParser, parsecLeadingCommaList, parsecLeadingOptCommaList)

import qualified Data.ByteString as BS
import qualified Distribution.Fields as Fields
import qualified Distribution.Fields.Field as Fields
import qualified Distribution.Fields.LexerMonad as Fields
import qualified Distribution.Parsec as Parsec
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Pos as PP

-- -----------------------------------------------------------------------------

type LineNo = Int

data PError
  = AmbiguousParse String LineNo
  | NoParse String LineNo
  | TabsError LineNo
  | FromString String (Maybe LineNo)
  deriving (Eq, Show)

data PWarning
  = PWarning String
  | UTFWarning LineNo String
  deriving (Eq, Show)

showPWarning :: FilePath -> PWarning -> String
showPWarning fpath (PWarning msg) =
  normalise fpath ++ ": " ++ msg
showPWarning fpath (UTFWarning line fname) =
  normalise fpath
    ++ ":"
    ++ show line
    ++ ": Invalid UTF-8 text in the '"
    ++ fname
    ++ "' field."

data ParseResult a = ParseFailed PError | ParseOk [PWarning] a
  deriving (Show)

instance Functor ParseResult where
  fmap _ (ParseFailed err) = ParseFailed err
  fmap f (ParseOk ws x) = ParseOk ws $ f x

instance Applicative ParseResult where
  pure = ParseOk []
  (<*>) = ap

{- FOURMOLU_DISABLE -}
instance Monad ParseResult where
  return = pure
  ParseFailed err >>= _ = ParseFailed err
  ParseOk ws x >>= f = case f x of
    ParseFailed err -> ParseFailed err
    ParseOk ws' x' -> ParseOk (ws' ++ ws) x'
#if !(MIN_VERSION_base(4,9,0))
  fail = parseResultFail
#elif !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif
{- FOURMOLU_ENABLE -}

instance Foldable ParseResult where
  foldMap _ (ParseFailed _) = mempty
  foldMap f (ParseOk _ x) = f x

instance Traversable ParseResult where
  traverse _ (ParseFailed err) = pure (ParseFailed err)
  traverse f (ParseOk ws x) = ParseOk ws <$> f x

instance Fail.MonadFail ParseResult where
  fail = parseResultFail

parseResultFail :: String -> ParseResult a
parseResultFail s = parseFail (FromString s Nothing)

parseFail :: PError -> ParseResult a
parseFail = ParseFailed

runP :: LineNo -> String -> ReadP a a -> String -> ParseResult a
runP line fieldname p s =
  case [x | (x, "") <- results] of
    [a] -> ParseOk (utf8Warnings line fieldname s) a
    -- TODO: what is this double parse thing all about?
    --      Can't we just do the all isSpace test the first time?
    [] -> case [x | (x, ys) <- results, all isSpace ys] of
      [a] -> ParseOk (utf8Warnings line fieldname s) a
      [] -> ParseFailed (NoParse fieldname line)
      _ -> ParseFailed (AmbiguousParse fieldname line)
    _ -> ParseFailed (AmbiguousParse fieldname line)
  where
    results = readP_to_S p s

runE :: LineNo -> String -> ReadE a -> String -> ParseResult a
runE line fieldname p s =
  case runReadE p s of
    Right a -> ParseOk (utf8Warnings line fieldname s) a
    Left e ->
      syntaxError line $
        "Parse of field '" ++ fieldname ++ "' failed (" ++ e ++ "): " ++ s

utf8Warnings :: LineNo -> String -> String -> [PWarning]
utf8Warnings line fieldname s =
  take
    1
    [ UTFWarning n fieldname
    | (n, l) <- zip [line ..] (lines s)
    , '\xfffd' `elem` l
    ]

locatedErrorMsg :: PError -> (Maybe LineNo, String)
locatedErrorMsg (AmbiguousParse f n) =
  ( Just n
  , "Ambiguous parse in field '" ++ f ++ "'."
  )
locatedErrorMsg (NoParse f n) =
  ( Just n
  , "Parse of field '" ++ f ++ "' failed."
  )
locatedErrorMsg (TabsError n) = (Just n, "Tab used as indentation.")
locatedErrorMsg (FromString s n) = (n, s)

syntaxError :: LineNo -> String -> ParseResult a
syntaxError n s = ParseFailed $ FromString s (Just n)

warning :: String -> ParseResult ()
warning s = ParseOk [PWarning s] ()

-- | Field descriptor.  The parameter @a@ parameterizes over where the field's
--   value is stored in.
data FieldDescr a = FieldDescr
  { fieldName :: String
  , fieldGet :: a -> Doc
  , fieldSet :: LineNo -> String -> a -> ParseResult a
  -- ^ @fieldSet n str x@ Parses the field value from the given input
  -- string @str@ and stores the result in @x@ if the parse was
  -- successful.  Otherwise, reports an error on line number @n@.
  }

field :: String -> (a -> Doc) -> ReadP a a -> FieldDescr a
field name showF readF =
  FieldDescr name showF (\line val _st -> runP line name readF val)

fieldParsec :: String -> (a -> Doc) -> ParsecParser a -> FieldDescr a
fieldParsec name showF readF =
  FieldDescr name showF $ \line val _st -> case explicitEitherParsec readF val of
    Left err -> ParseFailed (FromString err (Just line))
    Right x -> ParseOk [] x

-- Lift a field descriptor storing into an 'a' to a field descriptor storing
-- into a 'b'.
liftField :: (b -> a) -> (a -> b -> b) -> FieldDescr a -> FieldDescr b
liftField get set (FieldDescr name showF parseF) =
  FieldDescr
    name
    (showF . get)
    ( \line str b -> do
        a <- parseF line str (get b)
        return (set a b)
    )

-- Parser combinator for simple fields.  Takes a field name, a pretty printer,
-- a parser function, an accessor, and a setter, returns a FieldDescr over the
-- compoid structure.
simpleField
  :: String
  -> (a -> Doc)
  -> ReadP a a
  -> (b -> a)
  -> (a -> b -> b)
  -> FieldDescr b
simpleField name showF readF get set =
  liftField get set $ field name showF readF

simpleFieldParsec
  :: String
  -> (a -> Doc)
  -> ParsecParser a
  -> (b -> a)
  -> (a -> b -> b)
  -> FieldDescr b
simpleFieldParsec name showF readF get set =
  liftField get set $ fieldParsec name showF readF

commaListFieldWithSepParsec
  :: Separator
  -> String
  -> (a -> Doc)
  -> ParsecParser a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
commaListFieldWithSepParsec separator name showF readF get set =
  liftField get set' $
    fieldParsec name showF' (parsecLeadingCommaList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF' = separator . punctuate comma . map showF

commaListFieldParsec
  :: String
  -> (a -> Doc)
  -> ParsecParser a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
commaListFieldParsec = commaListFieldWithSepParsec fsep

commaNewLineListFieldParsec
  :: String
  -> (a -> Doc)
  -> ParsecParser a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
commaNewLineListFieldParsec = commaListFieldWithSepParsec sep

spaceListField
  :: String
  -> (a -> Doc)
  -> ReadP [a] a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
spaceListField name showF readF get set =
  liftField get set' $
    field name showF' (parseSpaceList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF' = fsep . map showF

-- this is a different definition from listField, like
-- commaNewLineListField it pretty prints on multiple lines
newLineListField
  :: String
  -> (a -> Doc)
  -> ReadP [a] a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
newLineListField = listFieldWithSep sep

listFieldWithSep
  :: Separator
  -> String
  -> (a -> Doc)
  -> ReadP [a] a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
listFieldWithSep separator name showF readF get set =
  liftField get set' $
    field name showF' (parseOptCommaList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF' = separator . map showF

listFieldWithSepParsec
  :: Separator
  -> String
  -> (a -> Doc)
  -> ParsecParser a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
listFieldWithSepParsec separator name showF readF get set =
  liftField get set' $
    fieldParsec name showF' (parsecLeadingOptCommaList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF' = separator . map showF

listField
  :: String
  -> (a -> Doc)
  -> ReadP [a] a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
listField = listFieldWithSep fsep

listFieldParsec
  :: String
  -> (a -> Doc)
  -> ParsecParser a
  -> (b -> [a])
  -> ([a] -> b -> b)
  -> FieldDescr b
listFieldParsec = listFieldWithSepParsec fsep

-- | The type of a function which, given a name-value pair of an
--   unrecognized field, and the current structure being built,
--   decides whether to incorporate the unrecognized field
--   (by returning  Just x, where x is a possibly modified version
--   of the structure being built), or not (by returning Nothing).
type UnrecFieldParser a = (String, String) -> a -> Maybe a

------------------------------------------------------------------------------

-- The data type for our three syntactic categories
data Field
  = -- | A regular @<property>: <value>@ field
    F LineNo String String
  | -- | A section with a name and possible parameter.  The syntactic
    -- structure is:
    --
    -- @
    --   <sectionname> <arg> {
    --     <field>*
    --   }
    -- @
    Section LineNo String String [Field]
  deriving
    ( Show
    , Eq -- for testing
    )

lineNo :: Field -> LineNo
lineNo (F n _ _) = n
lineNo (Section n _ _ _) = n

readFields :: BS.ByteString -> ParseResult [Field]
readFields input = case Fields.readFields' input of
  Right (fs, ws) ->
    ParseOk
      [PWarning msg | Fields.PWarning _ _ msg <- Fields.toPWarnings ws]
      (legacyFields fs)
  Left perr ->
    ParseFailed $
      NoParse
        ( PE.showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of file"
            (PE.errorMessages perr)
        )
        (PP.sourceLine pos)
    where
      pos = PE.errorPos perr

legacyFields :: [Fields.Field Parsec.Position] -> [Field]
legacyFields = map legacyField

legacyField :: Fields.Field Parsec.Position -> Field
legacyField (Fields.Field (Fields.Name pos name) fls) =
  F (posToLineNo pos) (fromUTF8BS name) (Fields.fieldLinesToString fls)
legacyField (Fields.Section (Fields.Name pos name) args fs) =
  Section (posToLineNo pos) (fromUTF8BS name) (Fields.sectionArgsToString args) (legacyFields fs)

posToLineNo :: Parsec.Position -> LineNo
posToLineNo (Parsec.Position row _) = row

------------------------------------------------------------------------------

-- urgh, we can't define optQuotes :: ReadP r a -> ReadP r a
-- because the "compat" version of ReadP isn't quite powerful enough.  In
-- particular, the type of <++ is ReadP r r -> ReadP r a -> ReadP r a
-- Hence the trick above to make 'lic' polymorphic.

-- Different than the naive version. it turns out Read instance for String accepts
-- the ['a', 'b'] syntax, which we do not want. In particular it messes
-- up any token starting with [].
parseHaskellString :: ReadP r String
parseHaskellString =
  readS_to_P $
    Read.readPrec_to_S (do Read.String s <- Read.lexP; return s) 0

parseTokenQ :: ReadP r String
parseTokenQ = parseHaskellString <++ munch1 (\x -> not (isSpace x) && x /= ',')

parseSpaceList
  :: ReadP r a
  -- ^ The parser for the stuff between commas
  -> ReadP r [a]
parseSpaceList p = sepBy p skipSpaces

-- This version avoid parse ambiguity for list element parsers
-- that have multiple valid parses of prefixes.
parseOptCommaList :: ReadP r a -> ReadP r [a]
parseOptCommaList p = sepBy p localSep
  where
    -- The separator must not be empty or it introduces ambiguity
    localSep =
      (skipSpaces >> char ',' >> skipSpaces)
        +++ (satisfy isSpace >> skipSpaces)

readPToMaybe :: ReadP a a -> String -> Maybe a
readPToMaybe p str =
  listToMaybe
    [ r | (r, s) <- readP_to_S p str, all isSpace s
    ]
