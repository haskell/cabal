{-# OPTIONS -cpp #-}
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
	Field(..), fName, lineNo,
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
import System.FilePath (normalise)
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
runP line fieldname p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk [] a
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk [] a
             []  -> ParseFailed (NoParse fieldname line)
             _   -> ParseFailed (AmbigousParse fieldname line)
    _   -> ParseFailed (AmbigousParse fieldname line)
  where results = readP_to_S p s

locatedErrorMsg :: PError -> (Maybe LineNo, String)
locatedErrorMsg (AmbigousParse f n) = (Just n, "Ambiguous parse in field '"++f++"'")
locatedErrorMsg (NoParse f n)       = (Just n, "Parse of field '"++f++"' failed: ")
locatedErrorMsg (FromString s n)    = (n, s)

syntaxError :: LineNo -> String -> ParseResult a
syntaxError n s = ParseFailed $ FromString s (Just n)

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

------------------------------------------------------------------------------

trimTrailingSpaces :: String -> String
trimTrailingSpaces = reverse . dropWhile isSpace . reverse


dropSpaces :: String -> String
dropSpaces = dropWhile isSpace

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
      -- $
      deriving (Show
               ,Eq)   -- for testing

lineNo :: Field -> LineNo
lineNo (F n _ _) = n
lineNo (Section n _ _ _) = n
lineNo (IfBlock n _ _ _) = n

fName :: Field -> String
fName (F _ n _) = n
fName (Section _ n _ _) = n
fName _ = undefined

--   sectionname ::= "library" | "executable"
sectionNames :: [String]
sectionNames = ["library", "executable", "flag"]

-- |Split a file into "Field: value" groups
readFields :: String -> ParseResult [Field]
readFields = mkStanza 
           -- . merge 
           . filter validLine 
           . zip [1..] 
           . map trimTrailingSpaces . lines
  where validLine (_,s) = case dropSpaces s of
                            '-':'-':_ -> False      -- Comment
                            []        -> False      -- blank line
                            _         -> True

mkStanza :: [(Int,String)] -> ParseResult [Field]
mkStanza lines0 = parseLines lines0 []
  where 
    parseLines [] fs = return (reverse fs)
    parseLines ls fs = do (f, ls') <- getField ls
                          parseLines ls' $ maybe fs (:fs) f

-- parses:
--
--   field ::= '#' directive value '\n'
--           | [<indent>] fieldname ':' space* fieldvalue
--           | "if" space cond
--           | sectionname section
--
getField :: [(Int,String)] -> ParseResult (Maybe Field,[(Int,String)])
getField [] = return (Nothing, [])
getField ((_,[]):ls) = return (Nothing,ls)
getField ((n,'#':xs):ls) | not (isSpace (head xs)) = do
  return (Just $ F n ('#':dir) (dropSpaces val), ls)
  where (dir,val) = break isSpace xs
getField ((lineno,line0):lines0) =
    let (spaces,line) = span isSpace line0
        indent = length spaces in 
    case break (`elem` " :{") line of
      (fld0, ':':val0) -> do  -- regular field
        let fld = map toLower fld0
            (val, lines') = getFieldValue indent (dropWhile isSpace val0) lines0
        return (Just $ F lineno fld val, lines')
      (blkName, ' ':rest) 
        | map toLower blkName == "if"             -> getIf (lineno,rest) lines0
        | map toLower blkName `elem` sectionNames -> 
            getSection (map toLower blkName) (lineno,rest) lines0
        | otherwise -> syntaxError lineno $
            "Missing colon after field label or invalid section name"
      (blkName, '{':rest) 
        | map toLower blkName `elem` sectionNames -> 
            getSection (map toLower blkName) (lineno,'{':rest) lines0
      ("","") -> return (Nothing,lines0)
      (_,_) -> syntaxError lineno $
        "Unrecognized field format: '" ++ line ++ "'"
          


-- parses:
--
--   cond ::= (any - '}')* block [ space* "else" block ]
--
getIf :: (Int,String) -> [(Int,String)] -> ParseResult (Maybe Field,[(Int,String)])
getIf (n,rest) ls = do
    (cond, ifBlock, lines') <- 
        case break (=='{') (dropSpaces rest) of
          (cond, '{':cs) -> 
            do (b,ls') <- getBlock (n,'{':cs) ls
               return (cond, b, ls')
          (_, _) -> -- condition spans more than one line
            syntaxError n "Multi-line conditions currently not supported."
    (elseBlock, lines'') <- tryElseBlock lines'
    return (Just $ IfBlock n cond ifBlock elseBlock, lines'') 
  where 
    tryElseBlock [] = return ([], [])
    tryElseBlock ((m,l):ls') = 
        if all isSpace l then return ([],ls') 
        else case (splitAt 4 . dropSpaces) l of
          (kw, rst) -> 
              if kw == "else" then 
                  getBlock (m,dropSpaces rst) ls'
              else syntaxError m "Only 'else' may appear after an if-Block"
          
-- parses:
-- 
--   block ::= space* '{' space* '\n'
--             field*
--             space* '}' space* '\n'
--   
getBlock :: (Int,String) -> [(Int,String)] -> ParseResult ([Field],[(Int,String)])
getBlock (lnum,rest) lines0 = do
    lines' <- checkBlockStart (lnum,dropSpaces rest) lines0
    munchTillEndOfBlock lines' []
  where
    checkBlockStart (n,'{':cs) ls = 
        if all isSpace cs then return ls
        else syntaxError n "Invalid characters after '{'"
    checkBlockStart (_,[]) ((n,l):ls) = 
        checkBlockStart (n,dropSpaces l) ls
    checkBlockStart (n,_) _ = syntaxError n "'{' expected"
                         
    munchTillEndOfBlock [] _ = syntaxError (-1) "missing '}' at end of file"
    munchTillEndOfBlock lines1@((n,l):ls) fs =
        case break (=='}') l of
          (spaces, '}':rst) ->   
              if all isSpace spaces
              then return ( reverse fs
                          , (n, rst):ls) 
              else syntaxError n "'}' must be first character on the line"
          _ -> do (f,ls') <- getField lines1
                  munchTillEndOfBlock ls' $ maybe fs (:fs) f

-- parses:
--
--   section ::= space* [ blocklabel ] space* block
--   
getSection :: String -> (Int,String) -> [(Int,String)] 
           -> ParseResult (Maybe Field,[(Int,String)])
getSection sectName (n,l) lines0 = 
    case break (=='{') (dropSpaces l) of
      (sectLabel, '{':rest) -> 
          do (b,lines') <- getBlock (n,'{':rest) lines0 
             return (Just $ Section n sectName (trimTrailingSpaces sectLabel) b, lines')
      (_,_) -> error "getSection got a line without a '{'.  Consider this a bug."

-- Get the field value of a field at given indentation      
getFieldValue :: Int -> String -> [(Int,String)] 
              -> (String,[(Int,String)])
getFieldValue indent val lines0 = 
    ( val' ++ rest 
    , lines')
  where
    val' = dropWhile isSpace val
    rest = (if val' == "" then safeTail else id) $
             -- don't include initial newline if it would be the first
             -- character
             concatMap (getContinuation . snd) valrest
    safeTail (_:xs) = xs
    safeTail [] = []
                
    (valrest,lines') = span (isContinuation indent . snd) lines0
    -- the continuation of a field value is everything that is indented
    -- relative to the field's label
    isContinuation ind line = 
        length (takeWhile isSpace line) > ind && not (all isSpace line)
    getContinuation line = '\n':stripDot (dropWhile isSpace line)
    stripDot "." = ""
    stripDot s   = s

------------------------------------------------------------------------------

-- |parse a module name
parseModuleNameQ :: ReadP r String
parseModuleNameQ = parseQuoted modu <++ modu
 where modu = do 
	  c <- satisfy isUpper
	  cs <- munch (\x -> isAlphaNum x || x `elem` "_'.")
	  return (c:cs)

parseFilePathQ :: ReadP r FilePath
parseFilePathQ = liftM normalise parseTokenQ

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

------------------------------------------------------------------------------
-- TESTING

#ifdef DEBUG
test_readFields = case 
                    readFields testFile 
                  of
                    ParseOk _ x -> x == expectedResult
                    _ -> False
  where 
    testFile = unlines $
          [ "Cabal-version: 3"
          , ""
          , "Description: This is a test file   "
          , "  with a description longer than two lines.  "
          , "if os(windows) {"
          , "  License:  You may not use this software"
          , "    ."
          , "    If you do use this software you will be seeked and destroyed."
          , "}"
          , "if os(linux) {"
          , "  Main-is:  foo1  "
          , "}"
          , ""
          , "if os(vista) {"
          , "  executable RootKit {"
          , "    Main-is: DRMManager.hs"
          , "  }"
          , "} else {"
          , "  executable VistaRemoteAccess {"
          , "    Main-is: VCtrl"
          , "}}"
          , ""
          , "executable Foo-bar {"
          , "  Main-is: Foo.hs"
          , "}"
          ]
    expectedResult = 
          [ F 1 "cabal-version" "3"
          , F 3 "description" 
                  "This is a test file\nwith a description longer than two lines."
          , IfBlock 5 "os(windows) " 
              [ F 6 "license" 
                      "You may not use this software\n\nIf you do use this software you will be seeked and destroyed."
              ]
              []
          , IfBlock 10 "os(linux) " 
              [ F 11 "main-is" "foo1" ] 
              [ ]
          , IfBlock 14 "os(vista) " 
              [ Section 15 "executable" "RootKit " 
                [ F 16 "main-is" "DRMManager.hs"]
              ] 
              [ Section 19 "executable" "VistaRemoteAccess "
                 [F 20 "main-is" "VCtrl"]
              ]
          , Section 23 "executable" "Foo-bar " 
              [F 24 "main-is" "Foo.hs"]
          ]

test_readFieldsCompat' = case test_readFieldsCompat of
                           ParseOk _ fs -> mapM_ (putStrLn . show) fs
                           x -> putStrLn $ "Failed: " ++ show x
test_readFieldsCompat = readFields testPkgDesc
  where 
    testPkgDesc = unlines [
        "-- Required",
        "Name: Cabal",
        "Version: 0.1.1.1.1-rain",
        "License: LGPL",
        "License-File: foo",
        "Copyright: Free Text String",
        "Cabal-version: >1.1.1",
        "-- Optional - may be in source?",
        "Author: Happy Haskell Hacker",
        "Homepage: http://www.haskell.org/foo",
        "Package-url: http://www.haskell.org/foo",
        "Synopsis: a nice package!",
        "Description: a really nice package!",
        "Category: tools",
        "buildable: True",
        "CC-OPTIONS: -g -o",
        "LD-OPTIONS: -BStatic -dn",
        "Frameworks: foo",
        "Tested-with: GHC",
        "Stability: Free Text String",
        "Build-Depends: haskell-src, HUnit>=1.0.0-rain",
        "Other-Modules: Distribution.Package, Distribution.Version,",
        "                Distribution.Simple.GHCPackageConfig",
        "Other-files: file1, file2",
        "Extra-Tmp-Files:    file1, file2",
        "C-Sources: not/even/rain.c, such/small/hands",
        "HS-Source-Dirs: src, src2",
        "Exposed-Modules: Distribution.Void, Foo.Bar",
        "Extensions: OverlappingInstances, TypeSynonymInstances",
        "Extra-Libraries: libfoo, bar, bang",
        "Extra-Lib-Dirs: \"/usr/local/libs\"",
        "Include-Dirs: your/slightest, look/will",
        "Includes: /easily/unclose, /me, \"funky, path\\\\name\"",
        "Install-Includes: /easily/unclose, /me, \"funky, path\\\\name\"",
        "GHC-Options: -fTH -fglasgow-exts",
        "Hugs-Options: +TH",
        "Nhc-Options: ",
        "Jhc-Options: ",
        "",
        "-- Next is an executable",
        "Executable: somescript",
        "Main-is: SomeFile.hs",
        "Other-Modules: Foo1, Util, Main",
        "HS-Source-Dir: scripts",
        "Extensions: OverlappingInstances",
        "GHC-Options: ",
        "Hugs-Options: ",
        "Nhc-Options: ",
        "Jhc-Options: "
        ]
{-
test' = do h <- openFile "../Cabal.cabal" ReadMode
           s <- hGetContents h
           let r = readFields s
           case r of
             ParseOk _ fs -> mapM_ (putStrLn . show) fs
             x -> putStrLn $ "Failed: " ++ show x
           putStrLn "==================="
           mapM_ (putStrLn . show) $
                 merge . zip [1..] . lines $ s
           hClose h
-}
-- 
#endif
