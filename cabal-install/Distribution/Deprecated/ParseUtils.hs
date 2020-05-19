{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
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

-- This module is meant to be local-only to Distribution...

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE Rank2Types #-}
module Distribution.Deprecated.ParseUtils (
        LineNo, PError(..), PWarning(..), locatedErrorMsg, syntaxError, warning,
        runP, runE, ParseResult(..), parseFail, showPWarning,
        Field(..), lineNo,
        FieldDescr(..), readFields, readFieldsFlat,
        parseHaskellString, parseFilePathQ, parseTokenQ,
        parseOptCommaList,
        showFilePath, showToken, showFreeText,
        field, simpleField, listField, listFieldWithSep, spaceListField,
        newLineListField,
        liftField,
        readPToMaybe,

        fieldParsec, simpleFieldParsec,
        listFieldParsec,
        commaListFieldParsec,
        commaNewLineListFieldParsec,

        UnrecFieldParser,
  ) where

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Deprecated.ReadP as ReadP hiding (get)

import Distribution.Pretty
import Distribution.ReadE
import Distribution.Utils.Generic

import Data.Tree as Tree (Tree (..), flatten)
import System.FilePath  (normalise)
import Text.PrettyPrint (Doc, punctuate, comma, fsep, sep)
import qualified Text.Read as Read

import qualified Control.Monad.Fail as Fail
import Distribution.Parsec (ParsecParser, parsecLeadingCommaList, parsecLeadingOptCommaList)

-- -----------------------------------------------------------------------------

type LineNo    = Int

data PError = AmbiguousParse String LineNo
            | NoParse String LineNo
            | TabsError LineNo
            | FromString String (Maybe LineNo)
        deriving (Eq, Show)

data PWarning = PWarning String
              | UTFWarning LineNo String
        deriving (Eq, Show)

showPWarning :: FilePath -> PWarning -> String
showPWarning fpath (PWarning msg) =
  normalise fpath ++ ": " ++ msg
showPWarning fpath (UTFWarning line fname) =
  normalise fpath ++ ":" ++ show line
        ++ ": Invalid UTF-8 text in the '" ++ fname ++ "' field."

data ParseResult a = ParseFailed PError | ParseOk [PWarning] a
        deriving Show

instance Functor ParseResult where
        fmap _ (ParseFailed err) = ParseFailed err
        fmap f (ParseOk ws x) = ParseOk ws $ f x

instance Applicative ParseResult where
        pure = ParseOk []
        (<*>) = ap


instance Monad ParseResult where
        return = pure
        ParseFailed err >>= _ = ParseFailed err
        ParseOk ws x >>= f = case f x of
                               ParseFailed err -> ParseFailed err
                               ParseOk ws' x' -> ParseOk (ws'++ws) x'

#if !(MIN_VERSION_base(4,9,0))
        fail = parseResultFail
#elif !(MIN_VERSION_base(4,13,0))
        fail = Fail.fail
#endif

instance Fail.MonadFail ParseResult where
        fail = parseResultFail

parseResultFail :: String -> ParseResult a
parseResultFail s = parseFail (FromString s Nothing)

parseFail :: PError -> ParseResult a
parseFail = ParseFailed

runP :: LineNo -> String -> ReadP a a -> String -> ParseResult a
runP line fieldname p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk (utf8Warnings line fieldname s) a
    --TODO: what is this double parse thing all about?
    --      Can't we just do the all isSpace test the first time?
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk (utf8Warnings line fieldname s) a
             []  -> ParseFailed (NoParse fieldname line)
             _   -> ParseFailed (AmbiguousParse fieldname line)
    _   -> ParseFailed (AmbiguousParse fieldname line)
  where results = readP_to_S p s

runE :: LineNo -> String -> ReadE a -> String -> ParseResult a
runE line fieldname p s =
    case runReadE p s of
      Right a -> ParseOk (utf8Warnings line fieldname s) a
      Left  e -> syntaxError line $
        "Parse of field '" ++ fieldname ++ "' failed (" ++ e ++ "): " ++ s

utf8Warnings :: LineNo -> String -> String -> [PWarning]
utf8Warnings line fieldname s =
  take 1 [ UTFWarning n fieldname
         | (n,l) <- zip [line..] (lines s)
         , '\xfffd' `elem` l ]

locatedErrorMsg :: PError -> (Maybe LineNo, String)
locatedErrorMsg (AmbiguousParse f n) = (Just n,
                                        "Ambiguous parse in field '"++f++"'.")
locatedErrorMsg (NoParse f n)        = (Just n,
                                        "Parse of field '"++f++"' failed.")
locatedErrorMsg (TabsError n)        = (Just n, "Tab used as indentation.")
locatedErrorMsg (FromString s n)     = (n, s)

syntaxError :: LineNo -> String -> ParseResult a
syntaxError n s = ParseFailed $ FromString s (Just n)

tabsError :: LineNo -> ParseResult a
tabsError ln = ParseFailed $ TabsError ln

warning :: String -> ParseResult ()
warning s = ParseOk [PWarning s] ()

-- | Field descriptor.  The parameter @a@ parameterizes over where the field's
--   value is stored in.
data FieldDescr a
  = FieldDescr
      { fieldName     :: String
      , fieldGet      :: a -> Doc
      , fieldSet      :: LineNo -> String -> a -> ParseResult a
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
    Right x  -> ParseOk [] x

-- Lift a field descriptor storing into an 'a' to a field descriptor storing
-- into a 'b'.
liftField :: (b -> a) -> (a -> b -> b) -> FieldDescr a -> FieldDescr b
liftField get set (FieldDescr name showF parseF)
 = FieldDescr name (showF . get)
        (\line str b -> do
            a <- parseF line str (get b)
            return (set a b))

-- Parser combinator for simple fields.  Takes a field name, a pretty printer,
-- a parser function, an accessor, and a setter, returns a FieldDescr over the
-- compoid structure.
simpleField :: String -> (a -> Doc) -> ReadP a a
            -> (b -> a) -> (a -> b -> b) -> FieldDescr b
simpleField name showF readF get set
  = liftField get set $ field name showF readF

simpleFieldParsec :: String -> (a -> Doc) -> ParsecParser a
            -> (b -> a) -> (a -> b -> b) -> FieldDescr b
simpleFieldParsec name showF readF get set
  = liftField get set $ fieldParsec name showF readF

commaListFieldWithSepParsec :: Separator -> String -> (a -> Doc) -> ParsecParser a
                      -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaListFieldWithSepParsec separator name showF readF get set =
   liftField get set' $
     fieldParsec name showF' (parsecLeadingCommaList readF)
   where
     set' xs b = set (get b ++ xs) b
     showF'    = separator . punctuate comma . map showF

commaListFieldParsec :: String -> (a -> Doc) -> ParsecParser a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaListFieldParsec = commaListFieldWithSepParsec fsep

commaNewLineListFieldParsec
    :: String -> (a -> Doc) ->  ParsecParser a
    -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaNewLineListFieldParsec = commaListFieldWithSepParsec sep

spaceListField :: String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
spaceListField name showF readF get set =
  liftField get set' $
    field name showF' (parseSpaceList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF'    = fsep . map showF

-- this is a different definition from listField, like
-- commaNewLineListField it pretty prints on multiple lines
newLineListField :: String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
newLineListField = listFieldWithSep sep

listFieldWithSep :: Separator -> String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listFieldWithSep separator name showF readF get set =
  liftField get set' $
    field name showF' (parseOptCommaList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF'    = separator . map showF

listFieldWithSepParsec :: Separator -> String -> (a -> Doc) -> ParsecParser a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listFieldWithSepParsec separator name showF readF get set =
  liftField get set' $
    fieldParsec name showF' (parsecLeadingOptCommaList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF'    = separator . map showF

listField :: String -> (a -> Doc) -> ReadP [a] a
          -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listField = listFieldWithSep fsep

listFieldParsec
    :: String -> (a -> Doc) -> ParsecParser a
    -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listFieldParsec = listFieldWithSepParsec fsep

-- | The type of a function which, given a name-value pair of an
--   unrecognized field, and the current structure being built,
--   decides whether to incorporate the unrecognized field
--   (by returning  Just x, where x is a possibly modified version
--   of the structure being built), or not (by returning Nothing).
type UnrecFieldParser a = (String,String) -> a -> Maybe a

------------------------------------------------------------------------------

-- The data type for our three syntactic categories
data Field
    = F LineNo String String
      -- ^ A regular @<property>: <value>@ field
    | Section LineNo String String [Field]
      -- ^ A section with a name and possible parameter.  The syntactic
      -- structure is:
      --
      -- @
      --   <sectionname> <arg> {
      --     <field>*
      --   }
      -- @
    | IfBlock LineNo String [Field] [Field]
      -- ^ A conditional block with an optional else branch:
      --
      -- @
      --  if <condition> {
      --    <field>*
      --  } else {
      --    <field>*
      --  }
      -- @
      deriving (Show
               ,Eq)   -- for testing

lineNo :: Field -> LineNo
lineNo (F n _ _) = n
lineNo (Section n _ _ _) = n
lineNo (IfBlock n _ _ _) = n

readFields :: String -> ParseResult [Field]
readFields input = ifelse
               =<< traverse (mkField 0)
               =<< mkTree tokens

  where ls = (lines . normaliseLineEndings) input
        tokens = (concatMap tokeniseLine . trimLines) ls

readFieldsFlat :: String -> ParseResult [Field]
readFieldsFlat input = traverse (mkField 0)
                   =<< mkTree tokens
  where ls = (lines . normaliseLineEndings) input
        tokens = (concatMap tokeniseLineFlat . trimLines) ls

-- attach line number and determine indentation
trimLines :: [String] -> [(LineNo, Indent, HasTabs, String)]
trimLines ls = [ (lineno, indent, hastabs, trimTrailing l')
               | (lineno, l) <- zip [1..] ls
               , let (sps, l') = span isSpace l
                     indent    = length sps
                     hastabs   = '\t' `elem` sps
               , validLine l' ]
  where validLine ('-':'-':_) = False      -- Comment
        validLine []          = False      -- blank line
        validLine _           = True

-- | We parse generically based on indent level and braces '{' '}'. To do that
-- we split into lines and then '{' '}' tokens and other spans within a line.
data Token =
       -- | The 'Line' token is for bits that /start/ a line, eg:
       --
       -- > "\n  blah blah { blah"
       --
       -- tokenises to:
       --
       -- > [Line n 2 False "blah blah", OpenBracket, Span n "blah"]
       --
       -- so lines are the only ones that can have nested layout, since they
       -- have a known indentation level.
       --
       -- eg: we can't have this:
       --
       -- > if ... {
       -- > } else
       -- >     other
       --
       -- because other cannot nest under else, since else doesn't start a line
       -- so cannot have nested layout. It'd have to be:
       --
       -- > if ... {
       -- > }
       -- >   else
       -- >     other
       --
       -- but that's not so common, people would normally use layout or
       -- brackets not both in a single @if else@ construct.
       --
       -- > if ... { foo : bar }
       -- > else
       -- >    other
       --
       -- this is OK
       Line LineNo Indent HasTabs String
     | Span LineNo                String  -- ^ span in a line, following brackets
     | OpenBracket LineNo | CloseBracket LineNo

type Indent = Int
type HasTabs = Bool

-- | Tokenise a single line, splitting on '{' '}' and the spans in between.
-- Also trims leading & trailing space on those spans within the line.
tokeniseLine :: (LineNo, Indent, HasTabs, String) -> [Token]
tokeniseLine (n0, i, t, l) = case split n0 l of
                            (Span _ l':ss) -> Line n0 i t l' :ss
                            cs              -> cs
  where split _ "" = []
        split n s  = case span (\c -> c /='}' && c /= '{') s of
          ("", '{' : s') ->             OpenBracket  n : split n s'
          (w , '{' : s') -> mkspan n w (OpenBracket  n : split n s')
          ("", '}' : s') ->             CloseBracket n : split n s'
          (w , '}' : s') -> mkspan n w (CloseBracket n : split n s')
          (w ,        _) -> mkspan n w []

        mkspan n s ss | null s'   =             ss
                      | otherwise = Span n s' : ss
          where s' = trimTrailing (trimLeading s)

tokeniseLineFlat :: (LineNo, Indent, HasTabs, String) -> [Token]
tokeniseLineFlat (n0, i, t, l)
  | null l'   = []
  | otherwise = [Line n0 i t l']
  where
    l' = trimTrailing (trimLeading l)

trimLeading, trimTrailing :: String -> String
trimLeading  = dropWhile isSpace
trimTrailing = dropWhileEndLE isSpace


type SyntaxTree = Tree (LineNo, HasTabs, String)

-- | Parse the stream of tokens into a tree of them, based on indent \/ layout
mkTree :: [Token] -> ParseResult [SyntaxTree]
mkTree toks =
  layout 0 [] toks >>= \(trees, trailing) -> case trailing of
    []               -> return trees
    OpenBracket  n:_ -> syntaxError n "mismatched brackets, unexpected {"
    CloseBracket n:_ -> syntaxError n "mismatched brackets, unexpected }"
    -- the following two should never happen:
    Span n     l  :_ -> syntaxError n $ "unexpected span: " ++ show l
    Line n _ _ l  :_ -> syntaxError n $ "unexpected line: " ++ show l


-- | Parse the stream of tokens into a tree of them, based on indent
-- This parse state expect to be in a layout context, though possibly
-- nested within a braces context so we may still encounter closing braces.
layout :: Indent       -- ^ indent level of the parent\/previous line
       -> [SyntaxTree] -- ^ accumulating param, trees in this level
       -> [Token]      -- ^ remaining tokens
       -> ParseResult ([SyntaxTree], [Token])
                       -- ^ collected trees on this level and trailing tokens
layout _ a []                               = return (reverse a, [])
layout i a (s@(Line _ i' _ _):ss) | i' < i  = return (reverse a, s:ss)
layout i a (Line n _ t l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    layout i (Node (n,t,l) sub:a) ss'

layout i a (Span n     l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    layout i (Node (n,False,l) sub:a) ss'

-- look ahead to see if following lines are more indented, giving a sub-tree
layout i a (Line n i' t l:ss) = do
    lookahead <- layout (i'+1) [] ss
    case lookahead of
        ([], _)   -> layout i (Node (n,t,l) [] :a) ss
        (ts, ss') -> layout i (Node (n,t,l) ts :a) ss'

layout _ _ (   OpenBracket  n :_)  = syntaxError n "unexpected '{'"
layout _ a (s@(CloseBracket _):ss) = return (reverse a, s:ss)
layout _ _ (   Span n l       : _) = syntaxError n $ "unexpected span: "
                                                  ++ show l

-- | Parse the stream of tokens into a tree of them, based on explicit braces
-- This parse state expects to find a closing bracket.
braces :: LineNo       -- ^ line of the '{', used for error messages
       -> [SyntaxTree] -- ^ accumulating param, trees in this level
       -> [Token]      -- ^ remaining tokens
       -> ParseResult ([SyntaxTree],[Token])
                       -- ^ collected trees on this level and trailing tokens
braces m a (Line n _ t l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    braces m (Node (n,t,l) sub:a) ss'

braces m a (Span n     l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    braces m (Node (n,False,l) sub:a) ss'

braces m a (Line n i t l:ss) = do
    lookahead <- layout (i+1) [] ss
    case lookahead of
        ([], _)   -> braces m (Node (n,t,l) [] :a) ss
        (ts, ss') -> braces m (Node (n,t,l) ts :a) ss'

braces m a (Span n       l:ss) = braces m (Node (n,False,l) []:a) ss
braces _ a (CloseBracket _:ss) = return (reverse a, ss)
braces n _ []                  = syntaxError n $ "opening brace '{'"
                              ++ "has no matching closing brace '}'"
braces _ _ (OpenBracket  n:_)  = syntaxError n "unexpected '{'"

-- | Convert the parse tree into the Field AST
-- Also check for dodgy uses of tabs in indentation.
mkField :: Int -> SyntaxTree -> ParseResult Field
mkField d (Node (n,t,_) _) | d >= 1 && t = tabsError n
mkField d (Node (n,_,l) ts) = case span (\c -> isAlphaNum c || c == '-') l of
  ([], _)       -> syntaxError n $ "unrecognised field or section: " ++ show l
  (name, rest)  -> case trimLeading rest of
    (':':rest') -> do let followingLines = concatMap Tree.flatten ts
                          tabs = not (null [()| (_,True,_) <- followingLines ])
                      if tabs && d >= 1
                        then tabsError n
                        else return $ F n (map toLower name)
                                          (fieldValue rest' followingLines)
    rest'       -> do ts' <- traverse (mkField (d+1)) ts
                      return (Section n (map toLower name) rest' ts')
 where    fieldValue firstLine followingLines =
            let firstLine' = trimLeading firstLine
                followingLines' = map (\(_,_,s) -> stripDot s) followingLines
                allLines | null firstLine' =              followingLines'
                         | otherwise       = firstLine' : followingLines'
             in intercalate "\n" allLines
          stripDot "." = ""
          stripDot s   = s

-- | Convert if/then/else 'Section's to 'IfBlock's
ifelse :: [Field] -> ParseResult [Field]
ifelse [] = return []
ifelse (Section n "if"   cond thenpart
       :Section _ "else" as   elsepart:fs)
       | null cond     = syntaxError n "'if' with missing condition"
       | null thenpart = syntaxError n "'then' branch of 'if' is empty"
       | not (null as) = syntaxError n "'else' takes no arguments"
       | null elsepart = syntaxError n "'else' branch of 'if' is empty"
       | otherwise     = do tp  <- ifelse thenpart
                            ep  <- ifelse elsepart
                            fs' <- ifelse fs
                            return (IfBlock n cond tp ep:fs')
ifelse (Section n "if"   cond thenpart:fs)
       | null cond     = syntaxError n "'if' with missing condition"
       | null thenpart = syntaxError n "'then' branch of 'if' is empty"
       | otherwise     = do tp  <- ifelse thenpart
                            fs' <- ifelse fs
                            return (IfBlock n cond tp []:fs')
ifelse (Section n "else" _ _:_) = syntaxError n
                                  "stray 'else' with no preceding 'if'"
ifelse (Section n s a fs':fs) = do fs''  <- ifelse fs'
                                   fs''' <- ifelse fs
                                   return (Section n s a fs'' : fs''')
ifelse (f:fs) = do fs' <- ifelse fs
                   return (f : fs')

------------------------------------------------------------------------------

parseFilePathQ :: ReadP r FilePath
parseFilePathQ = parseTokenQ
  -- removed until normalise is no longer broken, was:
  --   liftM normalise parseTokenQ

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

parseSpaceList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseSpaceList p = sepBy p skipSpaces

-- This version avoid parse ambiguity for list element parsers
-- that have multiple valid parses of prefixes.
parseOptCommaList :: ReadP r a -> ReadP r [a]
parseOptCommaList p = sepBy p localSep
  where
    -- The separator must not be empty or it introduces ambiguity
    localSep = (skipSpaces >> char ',' >> skipSpaces)
      +++ (satisfy isSpace >> skipSpaces)

readPToMaybe :: ReadP a a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- readP_to_S p str
                                     , all isSpace s ]
