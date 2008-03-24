-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ParseUtils
-- Copyright   :  (c) The University of Glasgow 2004
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Utilities for parsing PackageDescription and InstalledPackageInfo.


{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the University nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

-- This module is meant to be local-only to Distribution...

-- #hide
module Distribution.ParseUtils (
        LineNo, PError(..), PWarning, locatedErrorMsg, syntaxError, warning,
	runP, runE, ParseResult(..), catchParseError, parseFail,
	Field(..), fName, lineNo, 
	FieldDescr(..), ppField, ppFields, readFields, 
	parseFilePathQ, parseTokenQ,
	parseModuleNameQ, parseBuildTool, parsePkgconfigDependency,
        parseOptVersion, parsePackageNameQ, parseVersionRangeQ,
	parseTestedWithQ, parseLicenseQ, parseExtensionQ, 
	parseSepList, parseCommaList, parseOptCommaList,
	showFilePath, showToken, showTestedWith, showFreeText,
	field, simpleField, listField, commaListField, optsField, liftField,
	parseReadS, parseQuoted,

        UnrecFieldParser, warnUnrec, ignoreUnrec,
  ) where

import Distribution.Compiler (CompilerFlavor)
import Distribution.License
import Distribution.Version
import Distribution.Package	( parsePackageName, Dependency(..) )
import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.ReadE
import Distribution.Text
         ( Text(..) )
import Distribution.Simple.Utils (intercalate)
import Language.Haskell.Extension (Extension)

import Text.PrettyPrint.HughesPJ hiding (braces)
import Data.Char (isSpace, isUpper, toLower, isAlphaNum, isSymbol, isDigit)
import Data.Maybe	(fromMaybe)
import Data.Tree as Tree (Tree(..), flatten)

-- -----------------------------------------------------------------------------

type LineNo = Int

data PError = AmbigousParse String LineNo
            | NoParse String LineNo
            | TabsError LineNo
            | UTFError LineNo
            | FromString String (Maybe LineNo)
        deriving Show

type PWarning = String

data ParseResult a = ParseFailed PError | ParseOk [PWarning] a
        deriving Show

instance Monad ParseResult where
	return x = ParseOk [] x
	ParseFailed err >>= _ = ParseFailed err
	ParseOk ws x >>= f = case f x of
	                       ParseFailed err -> ParseFailed err
			       ParseOk ws' x' -> ParseOk (ws'++ws) x'
	fail s = ParseFailed (FromString s Nothing)

catchParseError :: ParseResult a -> (PError -> ParseResult a)
                -> ParseResult a
p@(ParseOk _ _) `catchParseError` _ = p
ParseFailed e `catchParseError` k   = k e

parseFail :: PError -> ParseResult a
parseFail = ParseFailed

runP :: LineNo -> String -> ReadP a a -> String -> ParseResult a
runP line fieldname p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk [] a
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk [] a
             []  -> ParseFailed (NoParse fieldname line)
             _   -> ParseFailed (AmbigousParse fieldname line)
    _   -> ParseFailed (AmbigousParse fieldname line)
  where results = readP_to_S p s

runE :: LineNo -> String -> ReadE a -> String -> ParseResult a
runE line fieldname p s =
    case runReadE p s of
      Right a -> ParseOk [] a
      Left  e -> syntaxError line ("Parse of field '"++fieldname++"' failed ("++e++"): " )

locatedErrorMsg :: PError -> (Maybe LineNo, String)
locatedErrorMsg (AmbigousParse f n) = (Just n, "Ambiguous parse in field '"++f++"'")
locatedErrorMsg (NoParse f n)       = (Just n, "Parse of field '"++f++"' failed: ")
locatedErrorMsg (TabsError n)       = (Just n, "Tab used as indentation.")
locatedErrorMsg (UTFError  n)       = (Just n, "Invalid UTF-8 text.")
locatedErrorMsg (FromString s n)    = (n, s)

syntaxError :: LineNo -> String -> ParseResult a
syntaxError n s = ParseFailed $ FromString s (Just n)

tabsError :: LineNo -> ParseResult a
tabsError ln = ParseFailed $ TabsError ln

utf8Error :: LineNo -> ParseResult a
utf8Error ln = ParseFailed $ UTFError ln

warning :: String -> ParseResult ()
warning s = ParseOk [s] ()

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

field :: String -> (a -> Doc) -> (ReadP a a) -> FieldDescr a
field name showF readF = 
  FieldDescr name showF (\line val _st -> runP line name readF val)

-- Lift a field descriptor storing into an 'a' to a field descriptor storing
-- into a 'b'.
liftField :: (b -> a) -> (a -> b -> b) -> FieldDescr a -> FieldDescr b
liftField get set (FieldDescr name showF parseF)
 = FieldDescr name (\b -> showF (get b))
	(\line str b -> do
	    a <- parseF line str (get b)
	    return (set a b))

-- Parser combinator for simple fields.  Takes a field name, a pretty printer,
-- a parser function, an accessor, and a setter, returns a FieldDescr over the
-- compoid structure.
simpleField :: String -> (a -> Doc) -> (ReadP a a)
            -> (b -> a) -> (a -> b -> b) -> FieldDescr b
simpleField name showF readF get set
  = liftField get set $ field name showF readF

commaListField :: String -> (a -> Doc) -> (ReadP [a] a)
		 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaListField name showF readF get set = 
  liftField get set $ 
    field name (fsep . punctuate comma . map showF) (parseCommaList readF)

listField :: String -> (a -> Doc) -> (ReadP [a] a)
		 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listField name showF readF get set = 
  liftField get set $ 
    field name (fsep . map showF) (parseOptCommaList readF)

optsField :: String -> CompilerFlavor -> (b -> [(CompilerFlavor,[String])]) -> ([(CompilerFlavor,[String])] -> b -> b) -> FieldDescr b
optsField name flavor get set = 
   liftField (fromMaybe [] . lookup flavor . get) 
	     (\opts b -> set (update flavor opts (get b)) b) $
	field name (hsep . map text)
		   (sepBy parseTokenQ' (munch1 isSpace))
  where
        update f opts [] = [(f,opts)]
	update f opts ((f',opts'):rest)
           | f == f'   = (f, opts ++ opts') : rest
           | otherwise = (f',opts') : update f opts rest

ppFields :: a -> [FieldDescr a] -> Doc
ppFields _ [] = empty
ppFields pkg' ((FieldDescr name getter _):flds) =
     ppField name (getter pkg') $$ ppFields pkg' flds

ppField :: String -> Doc -> Doc
ppField name fielddoc = text name <> colon <+> fielddoc


-- | The type of a function which, given a name-value pair of an
--   unrecognized field, and the current structure being built,
--   decides whether to incorporate the unrecognized field
--   (by returning  Just x, where x is a possibly modified version
--   of the structure being built), or not (by returning Nothing).
type UnrecFieldParser a = (String,String) -> a -> Maybe a

-- | A default unrecognized field parser which simply returns Nothing,
--   i.e. ignores all unrecognized fields, so warnings will be generated.
warnUnrec :: UnrecFieldParser a
warnUnrec _ _ = Nothing

-- | A default unrecognized field parser which silently (i.e. no
--   warnings will be generated) ignores unrecognized fields, by
--   returning the structure being built unmodified.
ignoreUnrec :: UnrecFieldParser a
ignoreUnrec _ x = Just x

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

fName :: Field -> String
fName (F _ n _) = n
fName (Section _ n _ _) = n
fName _ = error "fname: not a field or section"

readFields :: String -> ParseResult [Field]
readFields input =
  case [ n | (n,l) <- zip [1..] ls
           , '\xfffd' `elem` l ] of

    (n:_) -> utf8Error n

    []    -> ifelse
         =<< mapM (mkField 0)
         =<< mkTree tokens

  where ls = (lines . normaliseLineEndings) input
        tokens = (concatMap tokeniseLine . trimLines) ls

-- attach line number and determine indentation
trimLines :: [String] -> [(LineNo, Indent, HasTabs, String)]
trimLines ls = [ (lineno, indent, hastabs, (trimTrailing l'))
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
       -- this is ok
       Line LineNo Indent HasTabs String
     | Span LineNo                String  -- ^ span in a line, following brackets
     | OpenBracket LineNo | CloseBracket LineNo

type Indent = Int
type HasTabs = Bool

-- | Tokenise a single line, splitting on '{' '}' and the spans inbetween.
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

trimLeading, trimTrailing :: String -> String
trimLeading  = dropWhile isSpace
trimTrailing = reverse . dropWhile isSpace . reverse


-- | Fix different systems silly line ending conventions
normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old osx
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

type SyntaxTree = Tree (LineNo, HasTabs, String)

-- | Parse the stream of tokens into a tree of them, based on indent \/ layout
mkTree :: [Token] -> ParseResult [SyntaxTree]
mkTree toks =
  layout 0 [] toks >>= \(trees, trailing) -> case trailing of
    []               -> return trees
    OpenBracket  n:_ -> syntaxError n "mismatched backets, unexpected {"
    CloseBracket n:_ -> syntaxError n "mismatched backets, unexpected }"
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

layout _ _ (   OpenBracket  n :_)  = syntaxError n $ "unexpected '{'"
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
    rest'       -> do ts' <- mapM (mkField (d+1)) ts
                      return (Section n (map toLower name) rest' ts')
    where fieldValue firstLine followingLines =
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
ifelse (Section n "else" _ _:_) = syntaxError n "stray 'else' with no preceding 'if'"
ifelse (Section n s a fs':fs) = do fs''  <- ifelse fs'
                                   fs''' <- ifelse fs
                                   return (Section n s a fs'' : fs''')
ifelse (f:fs) = do fs' <- ifelse fs 
                   return (f : fs')

------------------------------------------------------------------------------

-- |parse a module name
parseModuleNameQ :: ReadP r String
parseModuleNameQ = parseQuoted modu <++ modu
 where modu = do 
	  c <- satisfy isUpper
	  cs <- munch (\x -> isAlphaNum x || x `elem` "_'.")
	  return (c:cs)

parseFilePathQ :: ReadP r FilePath
parseFilePathQ = parseTokenQ 
  -- removed until normalise is no longer broken, was:
  --   liftM normalise parseTokenQ

parseReadS :: Read a => ReadP r a
parseReadS = readS_to_P reads

parseBuildTool :: ReadP r Dependency
parseBuildTool = do name <- parseBuildToolNameQ
                    skipSpaces
                    ver <- parseVersionRangeQ <++ return AnyVersion
                    skipSpaces
                    return $ Dependency name ver

parseBuildToolNameQ :: ReadP r String
parseBuildToolNameQ = parseQuoted parseBuildToolName <++ parseBuildToolName

-- like parsePackageName but accepts symbols in components
parseBuildToolName :: ReadP r String
parseBuildToolName = do ns <- sepBy1 component (ReadP.char '-')
                        return (intercalate "-" ns)
  where component = do
          cs <- munch1 (\c -> isAlphaNum c
                           || isSymbol   c
                           || c == '_'
                           && c /= '-')
          if all isDigit cs then pfail else return cs

-- pkg-config allows versions and other letters in package names, 
-- eg "gtk+-2.0" is a valid pkg-config package _name_.
-- It then has a package version number like 2.10.13
parsePkgconfigDependency :: ReadP r Dependency
parsePkgconfigDependency = do name <- munch1 (\c -> isAlphaNum c || c `elem` "+-._")
                              skipSpaces
                              ver <- parseVersionRangeQ <++ return AnyVersion
                              skipSpaces
                              return $ Dependency name ver

parsePackageNameQ :: ReadP r String
parsePackageNameQ = parseQuoted parsePackageName <++ parsePackageName 

parseVersionRangeQ :: ReadP r VersionRange
parseVersionRangeQ = parseQuoted parse <++ parse

parseOptVersion :: ReadP r Version
parseOptVersion = parseQuoted ver <++ ver
  where ver = parse <++ return noVersion
	noVersion = Version{ versionBranch=[], versionTags=[] }

parseTestedWithQ :: ReadP r (CompilerFlavor,VersionRange)
parseTestedWithQ = parseQuoted tw <++ tw
  where tw = do compiler <- parseReadS
		skipSpaces
		version <- parse <++ return AnyVersion
		skipSpaces
		return (compiler,version)

parseLicenseQ :: ReadP r License
parseLicenseQ = parseQuoted parse <++ parse

-- urgh, we can't define optQuotes :: ReadP r a -> ReadP r a
-- because the "compat" version of ReadP isn't quite powerful enough.  In
-- particular, the type of <++ is ReadP r r -> ReadP r a -> ReadP r a
-- Hence the trick above to make 'lic' polymorphic.

parseExtensionQ :: ReadP r Extension
parseExtensionQ = parseQuoted parse <++ parse

parseTokenQ :: ReadP r String
parseTokenQ = parseReadS <++ munch1 (\x -> not (isSpace x) && x /= ',')

parseTokenQ' :: ReadP r String
parseTokenQ' = parseReadS <++ munch1 (\x -> not (isSpace x))

parseSepList :: ReadP r b
	     -> ReadP r a -- ^The parser for the stuff between commas
             -> ReadP r [a]
parseSepList sepr p = sepBy p separator
    where separator = skipSpaces >> sepr >> skipSpaces

parseCommaList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseCommaList = parseSepList (ReadP.char ',')

parseOptCommaList :: ReadP r a -- ^The parser for the stuff between commas
                  -> ReadP r [a]
parseOptCommaList = parseSepList (optional (ReadP.char ','))

parseQuoted :: ReadP r a -> ReadP r a
parseQuoted p = between (ReadP.char '"') (ReadP.char '"') p

-- --------------------------------------------
-- ** Pretty printing

showFilePath :: FilePath -> Doc
showFilePath = showToken

showToken :: String -> Doc
showToken str
 | not (any dodgy str) &&
   not (null str)       = text str
 | otherwise            = text (show str)
  where dodgy c = isSpace c || c == ','

showTestedWith :: (CompilerFlavor,VersionRange) -> Doc
showTestedWith (compiler, version) = text (show compiler) <+> disp version

-- | Pretty-print free-format text, ensuring that it is vertically aligned,
-- and with blank lines replaced by dots for correct re-parsing.
showFreeText :: String -> Doc
showFreeText s = vcat [text (if null l then "." else l) | l <- lines s]
