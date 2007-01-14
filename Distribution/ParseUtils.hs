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
	runP, ParseResult(..),
	Field,
	FieldDescr(..), readFields,
	parseFilePathQ, parseTokenQ,
	parseModuleNameQ, parseDependency, parseOptVersion,
	parsePackageNameQ, parseVersionRangeQ,
	parseTestedWithQ, parseLicenseQ, parseExtensionQ, 
	parseSepList, parseCommaList, parseOptCommaList,
	showFilePath, showToken, showTestedWith, showDependency, showFreeText,
	field, simpleField, listField, commaListField, optsField, liftField,
	parseReadS, parseReadSQ, parseQuoted,
  ) where

import Distribution.Compiler (CompilerFlavor)
import Distribution.License
import Distribution.Version
import Distribution.Package	( parsePackageName )
import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.Compat.FilePath (platformPath)
import Language.Haskell.Extension (Extension)

import Text.PrettyPrint.HughesPJ
import Control.Monad (liftM)
import Data.Char
import Data.Maybe	( fromMaybe)

-- -----------------------------------------------------------------------------

type LineNo = Int

data PError = AmbigousParse String LineNo
            | NoParse String LineNo
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

runP :: LineNo -> String -> ReadP a a -> String -> ParseResult a
runP lineNo fieldname p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk [] a
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk [] a
             []  -> ParseFailed (NoParse fieldname lineNo)
             _   -> ParseFailed (AmbigousParse fieldname lineNo)
    _   -> ParseFailed (AmbigousParse fieldname lineNo)
  where results = readP_to_S p s

locatedErrorMsg :: PError -> (Maybe LineNo, String)
locatedErrorMsg (AmbigousParse f n) = (Just n, "Ambigous parse in field '"++f++"'")
locatedErrorMsg (NoParse f n)       = (Just n, "Parse of field '"++f++"' failed: ")
locatedErrorMsg (FromString s n)    = (n, s)

syntaxError :: LineNo -> String -> ParseResult a
syntaxError n s = ParseFailed $ FromString s (Just n)

warning :: String -> ParseResult ()
warning s = ParseOk [s] ()

data FieldDescr a 
  = FieldDescr 
      { fieldName     :: String
      , fieldGet      :: a -> Doc
      , fieldSet      :: LineNo -> String -> a -> ParseResult a
      }

field :: String -> (a -> Doc) -> (ReadP a a) -> FieldDescr a
field name showF readF = 
  FieldDescr name showF (\lineNo val _st -> runP lineNo name readF val)

liftField :: (b -> a) -> (a -> b -> b) -> FieldDescr a -> FieldDescr b
liftField get set (FieldDescr name showF parseF)
 = FieldDescr name (\b -> showF (get b))
	(\lineNo str b -> do
	    a <- parseF lineNo str (get b)
	    return (set a b))

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

trimTrailingSpaces :: String -> String
trimTrailingSpaces = reverse . dropWhile isSpace . reverse

type Field  = (LineNo,String,String)

-- |Split a file into "Field: value" groups
readFields :: String -> ParseResult [Field]
readFields = mkStanza . merge . filter validLine . zip [1..] . map trimTrailingSpaces . lines
  where validLine (_,s) = case dropWhile isSpace s of
                            '-':'-':_ -> False      -- Comment
                            []        -> False      -- blank line
                            _         -> True

merge :: [(a, [Char])] -> [(a, [Char])]
merge ((n,x):(_,c:s):ys) 
  | c == ' ' || c == '\t' = case dropWhile isSpace s of
                               ('.':s') -> merge ((n,x++"\n"++s'):ys)
                               s'       -> merge ((n,x++"\n"++s'):ys)
merge ((n,x):ys) = (n,x) : merge ys
merge []         = []

mkStanza :: [(Int,String)] -> ParseResult [Field]
mkStanza []          = return []
mkStanza ((n,'#':xs):ys) | not (isSpace (head xs)) = do
  ss <- mkStanza ys
  return ((n, '#':dir, dropWhile isSpace val) : ss)
  where (dir,val) = break isSpace xs
mkStanza ((n,xs):ys) =
  case break (==':') xs of
    (fld0, ':':val) -> do
       let fld = map toLower fld0
       ss <- mkStanza ys
       return ((n, fld, dropWhile isSpace val):ss)
    (_, _)       -> syntaxError n "Invalid syntax (no colon after field name)"

-- |parse a module name
parseModuleNameQ :: ReadP r String
parseModuleNameQ = parseQuoted modu <++ modu
 where modu = do 
	  c <- satisfy isUpper
	  cs <- munch (\x -> isAlphaNum x || x `elem` "_'.")
	  return (c:cs)

parseFilePathQ :: ReadP r FilePath
parseFilePathQ = liftM platformPath parseTokenQ

parseReadS :: Read a => ReadP r a
parseReadS = readS_to_P reads

parseDependency :: ReadP r Dependency
parseDependency = do name <- parsePackageNameQ
                     skipSpaces
                     ver <- parseVersionRangeQ <++ return AnyVersion
                     skipSpaces
                     return $ Dependency name ver

parsePackageNameQ :: ReadP r String
parsePackageNameQ = parseQuoted parsePackageName <++ parsePackageName 

parseVersionRangeQ :: ReadP r VersionRange
parseVersionRangeQ = parseQuoted parseVersionRange <++ parseVersionRange

parseOptVersion :: ReadP r Version
parseOptVersion = parseQuoted ver <++ ver
  where ver = parseVersion <++ return noVersion
	noVersion = Version{ versionBranch=[], versionTags=[] }

parseTestedWithQ :: ReadP r (CompilerFlavor,VersionRange)
parseTestedWithQ = parseQuoted tw <++ tw
  where tw = do compiler <- parseReadS
		skipSpaces
		version <- parseVersionRange <++ return AnyVersion
		skipSpaces
		return (compiler,version)

parseLicenseQ :: ReadP r License
parseLicenseQ = parseQuoted parseReadS <++ parseReadS

-- urgh, we can't define optQuotes :: ReadP r a -> ReadP r a
-- because the "compat" version of ReadP isn't quite powerful enough.  In
-- particular, the type of <++ is ReadP r r -> ReadP r a -> ReadP r a
-- Hence the trick above to make 'lic' polymorphic.

parseExtensionQ :: ReadP r Extension
parseExtensionQ = parseQuoted parseReadS <++ parseReadS

-- | Parse something optionally wrapped in quotes.
parseReadSQ :: Read a => ReadP r a
parseReadSQ = parseQuoted parseReadS <++ parseReadS

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
showTestedWith (compiler,version) = text (show compiler ++ " " ++ showVersionRange version)

showDependency :: Dependency -> Doc
showDependency (Dependency name ver) = text name <+> text (showVersionRange ver)

-- | Pretty-print free-format text, ensuring that it is vertically aligned,
-- and with blank lines replaced by dots for correct re-parsing.
showFreeText :: String -> Doc
showFreeText s = vcat [text (if null l then "." else l) | l <- lines s]
