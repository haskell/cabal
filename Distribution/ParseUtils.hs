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
	LineNo, PError(..), showError, myError, runP,
	ParseResult(..),
	StanzaField(..), splitStanzas, Stanza, singleStanza,
	parseFilePathQ, parseLibNameQ,
	parseModuleNameQ, parseDependency, parseOptVersion,
	parsePackageNameQ, parseVersionRangeQ,
	parseTestedWithQ, parseLicenseQ, parseExtensionQ, parseCommaList,
	showFilePath, showTestedWith, showDependency, showFreeText,
	simpleField, listField, optsField, 
	parseReadS, parseQuoted,
  ) where

import Text.PrettyPrint.HughesPJ
import Distribution.License
import Distribution.Version
import Distribution.Extension
import Distribution.Package	( parsePackageName )
import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.Setup(CompilerFlavor(..))

import Data.Char

-- -----------------------------------------------------------------------------

type LineNo = Int

data PError = AmbigousParse String LineNo
            | NoParse String LineNo
            | FromString String (Maybe LineNo)
        deriving Show

data ParseResult a = ParseFailed PError | ParseOk a
        deriving Show

instance Monad ParseResult where
	return x = ParseOk x
	ParseFailed err >>= _ = ParseFailed err
	ParseOk x >>= f = f x
	fail s = ParseFailed (FromString s Nothing)

runP :: LineNo -> String -> ReadP a a -> String -> ParseResult a
runP lineNo field p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk a
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk a
             []  -> ParseFailed (NoParse field lineNo)
             _   -> ParseFailed (AmbigousParse field lineNo)
    _   -> ParseFailed (AmbigousParse field lineNo)
  where results = readP_to_S p s

showError :: PError -> String
showError (AmbigousParse f n)     = "Line "++show n++": Ambigous parse in field '"++f++"'"
showError (NoParse f n)           = "Line "++show n++": Parse of field '"++f++"' failed"
showError (FromString s (Just n)) = "Line "++show n++": " ++ s
showError (FromString s Nothing)  = s

myError :: LineNo -> String -> ParseResult a
myError n s = ParseFailed $ FromString s (Just n)

data StanzaField a 
  = StanzaField 
      { fieldName     :: String
      , fieldShow     :: a -> Doc
      , fieldGet      :: a -> Doc
      , fieldSet      :: LineNo -> String -> a -> ParseResult a
      }

simpleField :: String -> (a -> Doc) -> (ReadP a a) -> (b -> a) -> (a -> b -> b) -> StanzaField b
simpleField name showF readF get set = StanzaField name
   (\st -> text name <> colon <+> showF (get st))
   (showF . get)
   (\lineNo val st -> do
       x <- runP lineNo name readF val
       return (set x st))

listField :: String -> (a -> Doc) -> (ReadP [a] a) -> (b -> [a]) -> ([a] -> b -> b) -> StanzaField b
listField name showF readF get set = StanzaField name
   (\st -> case get st of
        [] -> empty
        lst ->
           text name <> colon <+> fsep (punctuate comma (map showF lst)))
   (\st -> case get st of
        [] -> empty
        lst ->
           vcat (map (\value -> comma <+> showF value) lst))
   (\lineNo val st -> do
       xs <- runP lineNo name (parseCommaList readF) val
       return (set xs st))

optsField :: String -> CompilerFlavor -> (b -> [(CompilerFlavor,[String])]) -> ([(CompilerFlavor,[String])] -> b -> b) -> StanzaField b
optsField name flavor get set = StanzaField name
   (\st -> case lookup flavor (get st) of
        Just args -> text name <> colon <+> hsep (map text args)
        Nothing   -> empty)
   (\st -> case lookup flavor (get st) of
        Just args -> sep (map text args)
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
       let fld = map toLower fld'
       ss <- mkStanza ys
       checkDuplField fld ss
       return ((n, fld, dropWhile isSpace val):ss)
    (_, _)       -> fail $ "Line "++show n++": Invalid syntax (no colon after field name)"
  where
    checkDuplField fld [] = return ()
    checkDuplField fld (x'@(n',fld',val'):xs')
      | fld' == fld = fail ("The field "++fld++" is defined on both line "++show n++" and "++show n')
      | otherwise   = checkDuplField fld xs'

-- |parse a module name
parseModuleNameQ :: ReadP r String
parseModuleNameQ = parseQuoted mod <++ mod
 where mod = do 
	  c <- satisfy isUpper
	  cs <- munch (\x -> isAlphaNum x || x `elem` "_'.")
	  return (c:cs)

parseFilePathQ :: ReadP r FilePath
parseFilePathQ = parseReadS <++ (munch1 (\x -> isAlphaNum x || x `elem` "-+/_."))

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

parseLibNameQ :: ReadP r String
parseLibNameQ = parseReadS <++ munch1 (\x -> not (isSpace x) && x /= ',')

parseCommaList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseCommaList p = sepBy p separator
    where separator = skipSpaces >> ReadP.char ',' >> skipSpaces

parseQuoted :: ReadP r a -> ReadP r a
parseQuoted p = between (ReadP.char '"') (ReadP.char '"') p

-- --------------------------------------------
-- ** Pretty printing

showFilePath :: FilePath -> Doc
showFilePath fpath
	| all (\x -> isAlphaNum x || x `elem` "-+/_.") fpath = text (replaceSlash fpath)
	| otherwise = doubleQuotes (text (replaceSlash fpath))
        where
        replaceSlash s = case break (== '\\') s of
                         (a, (h:t)) -> a ++ (h:h:(replaceSlash t))
                         (a, []) -> a

showTestedWith :: (CompilerFlavor,VersionRange) -> Doc
showTestedWith (compiler,version) = text (show compiler ++ " " ++ showVersionRange version)

showDependency :: Dependency -> Doc
showDependency (Dependency name ver) = text name <+> text (showVersionRange ver)

-- | Pretty-print free-format text, ensuring that it is vertically aligned,
-- and with blank lines replaced by dots for correct re-parsing.
showFreeText :: String -> Doc
showFreeText s = vcat [text (if null l then "." else l) | l <- lines s]
