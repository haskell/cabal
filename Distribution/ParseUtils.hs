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
        LineNo, PError(..), PWarning,
        locatedErrorMsg, showError, syntaxError, warning,
	runP, ParseResult(..),
	StanzaField(..), splitStanzas, Stanza, singleStanza,
	parseFilePathQ, parseTokenQ,
	parseModuleNameQ, parseDependency, parseOptVersion,
	parsePackageNameQ, parseVersionRangeQ,
	parseTestedWithQ, parseLicenseQ, parseExtensionQ, parseCommaList, parseOptCommaList,
	showFilePath, showToken, showTestedWith, showDependency, showFreeText,
	simpleField, listField, commaListField, optsField, 
	parseReadS, parseQuoted,
  ) where

import Text.PrettyPrint.HughesPJ
import Distribution.Compiler (CompilerFlavor)
import Distribution.License
import Distribution.Version
import Distribution.Package	( parsePackageName )
import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.Compat.FilePath (platformPath)
import Control.Monad (liftM)
import Data.Char
import Language.Haskell.Extension (Extension)

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
runP lineNo field p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk [] a
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk [] a
             []  -> ParseFailed (NoParse field lineNo)
             _   -> ParseFailed (AmbigousParse field lineNo)
    _   -> ParseFailed (AmbigousParse field lineNo)
  where results = readP_to_S p s

-- TODO: deprecated
showError :: PError -> String
showError e =
  case locatedErrorMsg e of
    (Just n,  s) -> "Line "++show n++": " ++ s
    (Nothing, s) -> s

locatedErrorMsg :: PError -> (Maybe LineNo, String)
locatedErrorMsg (AmbigousParse f n) = (Just n, "Ambigous parse in field '"++f++"'")
locatedErrorMsg (NoParse f n)       = (Just n, "Parse of field '"++f++"' failed: ")
locatedErrorMsg (FromString s n)    = (n, s)

syntaxError :: LineNo -> String -> ParseResult a
syntaxError n s = ParseFailed $ FromString s (Just n)

warning :: String -> ParseResult ()
warning s = ParseOk [s] ()

data StanzaField a 
  = StanzaField 
      { fieldName     :: String
      , fieldGet      :: a -> Doc
      , fieldSet      :: LineNo -> String -> a -> ParseResult a
      }

simpleField :: String -> (a -> Doc) -> (ReadP a a) -> (b -> a) -> (a -> b -> b) -> StanzaField b
simpleField name showF readF get set = StanzaField name
   (\st -> showF (get st))
   (\lineNo val st -> do
       x <- runP lineNo name readF val
       return (set x st))

commaListField :: String -> (a -> Doc) -> (ReadP [a] a) -> (b -> [a]) -> ([a] -> b -> b) -> StanzaField b
commaListField name showF readF get set = StanzaField name
   (\st -> fsep (punctuate comma (map showF (get st))))
   (\lineNo val st -> do
       xs <- runP lineNo name (parseCommaList readF) val
       return (set xs st))

listField :: String -> (a -> Doc) -> (ReadP [a] a) -> (b -> [a]) -> ([a] -> b -> b) -> StanzaField b
listField name showF readF get set = StanzaField name
   (\st -> fsep (map showF (get st)))
   (\lineNo val st -> do
       xs <- runP lineNo name (parseOptCommaList readF) val
       return (set xs st))

optsField :: String -> CompilerFlavor -> (b -> [(CompilerFlavor,[String])]) -> ([(CompilerFlavor,[String])] -> b -> b) -> StanzaField b
optsField name flavor get set = StanzaField name
   (\st -> case lookup flavor (get st) of
        Just args -> hsep (map text args)
        Nothing   -> empty)
   (\_ val st -> 
       let
         old_val  = get st
         old_args = case lookup flavor old_val of
                       Just args -> args
                       Nothing   -> []
         val'     = filter (\(f,_) -> f/=flavor) old_val
       in return (set ((flavor,words val++old_args) : val') st))

type Stanza = [(LineNo,String,String)]

-- |Split a string into blank line-separated stanzas of
-- "Field: value" groups
splitStanzas :: String -> ParseResult [Stanza]
splitStanzas = mapM mkStanza . map merge . groupStanzas . filter validLine . zip [1..] . lines
  where validLine (_,s) = case dropWhile isSpace s of
                            '-':'-':_ -> False      -- Comment
                            _         -> True
        groupStanzas :: [(Int,String)] -> [[(Int,String)]]
        groupStanzas [] = []
        groupStanzas xs = let (ys,zs) = break allSpaces xs
                           in ys : groupStanzas (dropWhile allSpaces zs)

allSpaces :: (a, String) -> Bool
allSpaces (_,xs) = all isSpace xs

-- |Split a file into "Field: value" groups, but blank lines have no
-- significance, unlike 'splitStanzas'.  A field value may span over blank
-- lines.
singleStanza :: String -> ParseResult Stanza
singleStanza = mkStanza . merge . filter validLine . zip [1..] . lines
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

mkStanza :: [(Int,String)] -> ParseResult Stanza
mkStanza []          = return []
mkStanza ((n,xs):ys) =
  case break (==':') xs of
    (fld', ':':val) -> do
       let fld'' = map toLower fld'
       fld <- case () of
                _ | fld'' == "hs-source-dir"
                           -> do warning "The field \"hs-source-dir\" is deprecated, please use hs-source-dirs."
                                 return "hs-source-dirs"
                  | fld'' == "other-files"
                           -> do warning "The field \"other-files\" is deprecated, please use extra-source-files."
                                 return "extra-source-files"
                  | otherwise -> return fld''
       ss <- mkStanza ys
       checkDuplField fld ss
       return ((n, fld, dropWhile isSpace val):ss)
    (_, _)       -> syntaxError n "Invalid syntax (no colon after field name)"
  where
    checkDuplField _ [] = return ()
    checkDuplField fld ((n',fld',_):xs')
      | fld' == fld = syntaxError (max n n') $ "The field "++fld++" was already defined on line " ++ show (min n n')
      | otherwise   = checkDuplField fld xs'

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

parseTokenQ :: ReadP r String
parseTokenQ = parseReadS <++ munch1 (\x -> not (isSpace x) && x /= ',')

parseCommaList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseCommaList p = sepBy p separator
    where separator = skipSpaces >> ReadP.char ',' >> skipSpaces

parseOptCommaList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseOptCommaList p = sepBy p separator
    where separator = skipSpaces >> optional (ReadP.char ',') >> skipSpaces

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
