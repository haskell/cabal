{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Package
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--
-- Explanation: Package description and parsing

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

    * Neither the name of Isaac Jones nor the names of other
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

module Distribution.Package (
	PackageIdentifier(..), 
	showPackageId,
	PackageDescription(..),
	emptyPackageDescription,
        readPackageDescription,
        writePackageDescription,
        hasLibs,
        BuildInfo(..),
        emptyBuildInfo,
        Executable(..),
        emptyExecutable,
        allModules,
#ifdef DEBUG
        hunitTests,
        test
#endif
  ) where

import Control.Monad(foldM, liftM)
import Data.Char
import Data.List(concatMap)
import Data.Maybe(fromMaybe)
import Text.PrettyPrint.HughesPJ

import Distribution.Version(Version(..), VersionRange(..),
                            showVersion, parseVersion, 
                            showVersionRange, parseVersionRange)
import Distribution.Misc(License(..), Dependency(..), Extension(..))
import Distribution.Setup(CompilerFlavor(..))

import Compat.H98
import Compat.ReadP hiding (get)

#ifdef DEBUG
import HUnit (Test(..), (~:), (~=?), assertEqual, assertBool, Assertion, runTestTT)
#endif

data PackageIdentifier
    = PackageIdentifier {pkgName::String, pkgVersion::Version}
      deriving (Read, Show, Eq)

showPackageId :: PackageIdentifier -> String
showPackageId (PackageIdentifier n (Version [] _)) = n -- if no version, don't show version.
showPackageId pkgid = 
  pkgName pkgid ++ '-': showVersion (pkgVersion pkgid)


-- | This data type is the internal representation of the file @pkg.descr@.
-- It contains two kinds of information about the package: information
-- which is needed for all packages, such as the package name and version, and 
-- information which is needed for the simple build system only, such as 
-- the compiler options and library name.
-- 
data PackageDescription
    =  PackageDescription {
	-- the following are required by all packages:
	package        :: PackageIdentifier,
        license        :: License,
        copyright      :: String,
        maintainer     :: String,
        stability      :: String,
        library        :: Maybe BuildInfo,
        executables    :: [Executable]
    }
    deriving (Show, Read, Eq)

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {package      = PackageIdentifier "" (Version [] []),
                      license      = AllRightsReserved,
                      copyright    = "",
                      maintainer   = "",
                      stability    = "",
                      library      = Nothing,
                      executables  = []
                     }

-- |Get all the module names from this package
allModules :: PackageDescription -> [String]
allModules PackageDescription{executables=execs, library=lib}
    = (concatMap (\e -> modules $ buildInfo e) execs)
         ++ (maybe [] modules lib)

-- |Set the name for this package. Convenience function.
setPkgName :: String -> PackageDescription -> PackageDescription
setPkgName n desc@PackageDescription{package=pkgIdent}
    = desc{package=pkgIdent{pkgName=n}}

-- |Set the version for this package. Convenience function.
setPkgVersion :: Version -> PackageDescription -> PackageDescription
setPkgVersion v desc@PackageDescription{package=pkgIdent}
    = desc{package=pkgIdent{pkgVersion=v}}

-- |does this package have any libraries?
hasLibs :: PackageDescription -> Bool
hasLibs p = case library p of
            Just l  -> if null (cSources l) && null (modules l)
                       then False else True
            Nothing -> False

            
data BuildInfo = BuildInfo {
        buildDepends    :: [Dependency],
        modules         :: [String],
	exposedModules  :: [String],
        cSources        :: [FilePath],
        hsSourceDir     :: FilePath,
        extensions      :: [Extension],
        extraLibs       :: [String],
        includeDirs     :: [FilePath],
        includes        :: [FilePath],
        options         :: [(CompilerFlavor,[String])]
    }
    deriving (Show,Read,Eq)

currentDir :: FilePath
currentDir = "."-- FIX: FileUtils.currentDir

emptyBuildInfo :: BuildInfo
emptyBuildInfo = BuildInfo {
                      buildDepends   = [],
                      modules        = [],
		      exposedModules = [], -- Only used for libs
		      cSources       = [],
		      hsSourceDir    = currentDir,
                      extensions     = [],
                      extraLibs      = [],
                      includeDirs    = [],
                      includes       = [],
                      options        = []
                     }
                     
-- |Add options for a specific compiler. Convenience function.
setOptions :: CompilerFlavor -> [String] -> BuildInfo -> BuildInfo
setOptions c xs desc@BuildInfo{options=opts}
    = desc{options=(c,xs):opts}


data Executable = Executable {
        exeName    :: String,
        modulePath :: FilePath,
        buildInfo  :: BuildInfo
    }
    deriving (Show, Read, Eq)

emptyExecutable :: Executable
emptyExecutable = Executable {
                      exeName = "",
                      modulePath = "",
                      buildInfo = emptyBuildInfo
                     }

-- ------------------------------------------------------------
-- * Parsing & Pretty printing
-- ------------------------------------------------------------

type LineNo = Int

data PError = AmbigousParse String LineNo
            | NoParse String LineNo
            | FromString String (Maybe LineNo)
        deriving Show

instance Error PError where
        strMsg s = FromString s Nothing

showError :: PError -> String
showError (AmbigousParse f n)     = "Line "++show n++": Ambigous parse in field '"++f++"'"
showError (NoParse f n)           = "Line "++show n++": Parse of field '"++f++"' failed"
showError (FromString s (Just n)) = "Line "++show n++": " ++ s
showError (FromString s Nothing)  = s

myError :: LineNo -> String -> Either PError a
myError n s = Left $ FromString s (Just n)

data StanzaField a 
  = StanzaField 
      { fieldName     :: String
      , fieldGet      :: a -> Doc
      , fieldSet      :: LineNo -> String -> a -> Either PError a
      }

basicStanzaFields :: [StanzaField PackageDescription]
basicStanzaFields =
 [ simpleField "name"
                           text                   parsePackageName
                           (pkgName . package)    (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField "version"
                           (text . showVersion)   parseVersion 
                           (pkgVersion . package) (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , licenseField "license" False
                           license                (\l pkg -> pkg{license=l})
 , licenseField "license-file" True
                           license                (\l pkg -> pkg{license=l})
 , simpleField "copyright"
                           text                   (munch (const True))
                           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
                           text                   (munch (const True))
                           maintainer             (\val pkg -> pkg{maintainer=val})
 , simpleField "stability"
                           text                   (munch (const True))
                           stability              (\val pkg -> pkg{stability=val})
 ]

executableStanzaFields :: [StanzaField Executable]
executableStanzaFields =
 [ simpleField "executable"
                           text               (munch (const True))
                           exeName            (\xs    exe -> exe{exeName=xs})
 , simpleField "main-is"
                           showFilePath       parseFilePath
                           modulePath         (\xs    exe -> exe{modulePath=xs})
 ]

binfoFields :: [StanzaField BuildInfo]
binfoFields =
 [ listField   "build-depends"   
                           showDependency     parseDependency
                           buildDepends       (\xs    binfo -> binfo{buildDepends=xs})
 , listField   "modules"         
                           text               parseModuleName
                           modules            (\xs    binfo -> binfo{modules=xs})
 , listField   "exposed-modules"
                           text               parseModuleName
                           exposedModules     (\xs    binfo -> binfo{exposedModules=xs})
 , listField   "c-sources"
                           showFilePath       parseFilePath
                           cSources           (\paths binfo -> binfo{cSources=paths})
 , listField   "extensions"
                           (text . show)      parseExtension
                           extensions         (\exts  binfo -> binfo{extensions=exts})
 , listField   "extra-libs"
                           text               parseLibName
                           extraLibs          (\xs    binfo -> binfo{extraLibs=xs})
 , listField   "includes"
                           showFilePath       parseFilePath
                           includes           (\paths binfo -> binfo{includes=paths})
 , listField   "include-dirs"
                           showFilePath       parseFilePath
                           includes           (\paths binfo -> binfo{includeDirs=paths})
 , simpleField "hs-source-dir"
                           showFilePath       parseFilePath
                           hsSourceDir        (\path  binfo -> binfo{hsSourceDir=path})
 , optsField   "options-ghc"  GHC
                           options            (\path  binfo -> binfo{options=path})
 , optsField   "options-hugs" Hugs
                           options            (\path  binfo -> binfo{options=path})
 , optsField   "options-nhc"  NHC
                           options            (\path  binfo -> binfo{options=path})
 ]

simpleField :: String -> (a -> Doc) -> (ReadP a a) -> (b -> a) -> (a -> b -> b) -> StanzaField b
simpleField name showF readF get set = StanzaField name
   (\st -> text name <> colon <+> showF (get st))
   (\lineNo val st -> do
       x <- runP lineNo name readF val
       return (set x st))

listField :: String -> (a -> Doc) -> (ReadP [a] a) -> (b -> [a]) -> ([a] -> b -> b) -> StanzaField b
listField name showF readF get set = StanzaField name
   (\st -> case get st of
        [] -> empty
        (value:values) ->
           text name <> vcat (               colon <+> showF value:
                              map (\value' -> comma <+> showF value') values))
   (\lineNo val st -> do
       xs <- runP lineNo name (parseCommaList readF) val
       return (set xs st))

licenseField :: String -> Bool -> (b -> License) -> (License -> b -> b) -> StanzaField b
licenseField name flag get set = StanzaField name
   (\st -> case get st of
             OtherLicense path | flag      -> text name <> colon <+> showFilePath path
                               | otherwise -> empty
             license'          | not flag  -> text name <> colon <+> text (show license')
                               | otherwise -> empty)
   (\lineNo val st ->
       if flag 
         then do 
            path <- runP lineNo name parseFilePath val
            return (set (OtherLicense path) st)
         else do
            x <- runP lineNo name parseLicense val
            return (set x st))

optsField :: String -> CompilerFlavor -> (b -> [(CompilerFlavor,[String])]) -> ([(CompilerFlavor,[String])] -> b -> b) -> StanzaField b
optsField name flavor get set = StanzaField name
   (\st -> case lookup flavor (get st) of
        Just args -> text name <> colon <+> hsep (map text args)
        Nothing   -> empty)
   (\_ val st -> 
       let
         old_val  = get st
         old_args = case lookup flavor old_val of
                       Just args -> args
                       Nothing   -> []
         val'     = filter (\(f,_) -> f/=flavor) old_val
       in return (set ((flavor,words val++old_args) : val') st))

-- --------------------------------------------
-- ** Parsing

-- |Parse the given package file.
readPackageDescription :: FilePath -> IO PackageDescription
readPackageDescription fpath = do 
  str <- readFile fpath
  case parseDescription str of
    Left  e -> error (showError e) -- FIXME
    Right PackageDescription{library=Nothing, executables=[]} -> error "no library listed, and no executable stanza."
    Right x -> return x

parseDescription :: String -> Either PError PackageDescription
parseDescription inp = do let (st:sts) = splitStanzas inp
                          pkg <- foldM (parseBasicStanza basicStanzaFields) emptyPackageDescription st
                          exes <- mapM parseExecutableStanza sts
                          return pkg{executables=exes}
  where -- The basic stanza, with library building info
        parseBasicStanza ((StanzaField name _ set):fields) pkg (lineNo, f, val)
          | name == f = set lineNo val pkg
          | otherwise = parseBasicStanza fields pkg (lineNo, f, val)
        parseBasicStanza [] pkg (lineNo, f, val) = do
          let lib = fromMaybe emptyBuildInfo (library pkg)
	  lib' <- parseBInfoField binfoFields lib (lineNo, f, val)
          return pkg{library=Just lib'}

        parseExecutableStanza st@((_, "executable",eName):_) =
          case lookupField "main-is" st of
	    Just (_,_) -> foldM (parseExecutableField executableStanzaFields) emptyExecutable st
	    Nothing           -> fail $ "No 'Main-Is' field found for " ++ eName ++ " stanza"
        parseExecutableStanza ((lineNo, f,_):_) = 
          myError lineNo $ "'Executable' stanza starting with field '" ++ f ++ "'"
        parseExecutableStanza _ = error "This shouldn't happen!"

        parseExecutableField ((StanzaField name _ set):fields) exe (lineNo, f, val)
	  | name == f = set lineNo val exe
	  | otherwise = parseExecutableField fields exe (lineNo, f, val)
	parseExecutableField [] exe (lineNo, f, val) = do
	  binfo <- parseBInfoField binfoFields (buildInfo exe) (lineNo, f, val)
          return exe{buildInfo=binfo}

        parseBInfoField ((StanzaField name _ set):fields) binfo (lineNo, f, val)
	  | name == f = set lineNo val binfo
	  | otherwise = parseBInfoField fields binfo (lineNo, f, val)
	parseBInfoField [] _ (lineNo, f, _) =
	  myError lineNo $ "Unknown field '" ++ f ++ "'"
        -- ...
        lookupField :: String -> Stanza -> Maybe (LineNo,String)
        lookupField _ [] = Nothing
        lookupField x ((n,f,v):st)
          | x == f      = Just (n,v)
          | otherwise   = lookupField x st


runP :: LineNo -> String -> ReadP a a -> String -> Either PError a
runP lineNo field p s =
  case [ x | (x,"") <- results ] of
    [a] -> Right a
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> Right a
             []  -> Left (NoParse field lineNo)
             _   -> Left (AmbigousParse field lineNo)
    _   -> Left (AmbigousParse field lineNo)
  where results = readP_to_S p s

type Stanza = [(LineNo,String,String)]

-- |Split a string into blank line-separated stanzas of
-- "Field: value" groups
splitStanzas :: String -> [Stanza]
splitStanzas = map merge . groupStanzas . filter validLine . zip [1..] . lines
  where validLine (_,s) = case dropWhile isSpace s of
                            '-':'-':_ -> False      -- Comment
                            _         -> True
        allSpaces (_,xs) = all isSpace xs
        groupStanzas :: [(Int,String)] -> [[(Int,String)]]
        groupStanzas [] = []
        groupStanzas xs = let (ys,zs) = break allSpaces xs
                           in ys : groupStanzas (dropWhile allSpaces zs)
        merge ((n,x):(_,' ':s):ys) = case dropWhile isSpace s of
                                       ('.':s') -> merge ((n,x++"\n"++s'):ys)
                                       s'       -> merge ((n,x++"\n"++s'):ys)
        merge ((n,x):ys) = brk n x : merge ys
        merge []         = []
        brk n xs = case break (==':') xs of
                     (fld, ':':val) -> (n, map toLower fld, dropWhile isSpace val)
                     (_, _)       -> error $ "Line "++show n++": Invalid syntax (no colon after field name)"

-- |parse a module name
parseModuleName :: ReadP r String
parseModuleName = do c <- satisfy isUpper
                     cs <- munch (\x -> isAlphaNum x || x `elem` "_'.")
                     return (c:cs)

parseFilePath :: ReadP r FilePath
parseFilePath = parseReadS <++ (munch1 (\x -> isAlphaNum x || x `elem` "-+/_."))

showFilePath :: FilePath -> Doc
showFilePath fpath
	| all (\x -> isAlphaNum x || x `elem` "-+/_.") fpath = text fpath
	| otherwise = doubleQuotes (text fpath)


parseReadS :: Read a => ReadP r a
parseReadS = readS_to_P reads

parsePackageName :: ReadP r String
parsePackageName = do n <- satisfy isAlpha
                      name <- munch1 (\x -> isAlphaNum x || x `elem` "-")
                      return (n:name)

parseDependency :: ReadP r Dependency
parseDependency = do name <- parsePackageName
                     skipSpaces
                     ver <- parseVersionRange <++ return AnyVersion
                     skipSpaces
                     return $ Dependency name ver

parseLicense :: ReadP r License
parseLicense = parseReadS

parseExtension :: ReadP r Extension
parseExtension = parseReadS

parseLibName :: ReadP r String
parseLibName = munch1 (\x -> not (isSpace x) && x /= ',')

parseCommaList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseCommaList p = sepBy1 p separator
    where separator = skipSpaces >> Compat.ReadP.char ',' >> skipSpaces



-- --------------------------------------------
-- ** Pretty printing

writePackageDescription :: FilePath -> PackageDescription -> IO ()
writePackageDescription fpath pkg = writeFile fpath (showPackageDescription pkg)

showPackageDescription :: PackageDescription -> String
showPackageDescription pkg = render $
  ppFields pkg basicStanzaFields $$
  (case library pkg of
     Nothing  -> empty
     Just lib -> ppFields lib binfoFields) $$
  vcat (map ppExecutable (executables pkg))
  where
    ppExecutable exe =
      space $$
      ppFields exe executableStanzaFields $$
      ppFields (buildInfo exe) binfoFields

    ppFields _ [] = empty
    ppFields pkg' ((StanzaField _ get _):flds) =
           get pkg' $$ ppFields pkg' flds
        
showDependency :: Dependency -> Doc
showDependency (Dependency name ver) = text name <+> text (showVersionRange ver)

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
testPkgDesc = unlines [
        "-- Required",
        "Name: Cabal",
        "Version: 0.1.1.1.1-rain",
        "License: LGPL",
        "Copyright: Free Text String",
        "-- Optional - may be in source?",
        "Stability: Free Text String",
        "Build-Depends: haskell-src, HUnit>=1.0.0-rain",
        "Modules: Distribution.Package, Distribution.Version,",
        "         Distribution.Simple.GHCPackageConfig",
        "C-Sources: not/even/rain.c, such/small/hands",
        "HS-Source-Dir: src",
        "Exposed-Modules: Distribution.Void, Foo.Bar",
        "Extensions: OverlappingInstances, TypeSynonymInstances",
        "Extra-Libs: libfoo, bar, bang",
        "Include-Dirs: your/slightest, look/will",
        "Includes: /easily/unclose, /me, \"funky, path\\\\name\"",
        "Options-ghc: -fTH -fglasgow-exts",
        "Options-hugs: +TH",
        "",
        "-- Next is an executable",
        "Executable: somescript",
        "Main-is: SomeFile.hs",
        "Modules: Foo1, Util, Main",
        "HS-Source-Dir: scripts",
        "Extensions: OverlappingInstances"
        ]

testPkgDescAnswer = 
 PackageDescription {package = PackageIdentifier {pkgName = "Cabal",
                                                 pkgVersion = Version {versionBranch = [0,1,1,1,1],
                                                 versionTags = ["rain"]}},
                    license = LGPL,
                    copyright = "Free Text String",
                    maintainer = "",
                    stability = "Free Text String",

                    library = Just $ BuildInfo {
                        buildDepends = [Dependency "haskell-src" AnyVersion,
                                        Dependency "HUnit"
                                         (UnionVersionRanges (ThisVersion (Version [1,0,0] ["rain"]))
                                          (LaterVersion (Version [1,0,0] ["rain"])))],

                        modules = ["Distribution.Package","Distribution.Version",
                                      "Distribution.Simple.GHCPackageConfig"],

                        cSources = ["not/even/rain.c", "such/small/hands"],
                        hsSourceDir = "src",
                        exposedModules = ["Distribution.Void", "Foo.Bar"],
                        extensions = [OverlappingInstances, TypeSynonymInstances],
                        extraLibs = ["libfoo", "bar", "bang"],
                        includeDirs = ["your/slightest", "look/will"],
                        includes = ["/easily/unclose", "/me", "funky, path\\name"],
                        -- Note reversed order:
                        options = [(Hugs,["+TH"]), (GHC,["-fTH","-fglasgow-exts"])]
                    },
                    executables = [Executable "somescript" "SomeFile.hs" (
                      emptyBuildInfo{
                        modules = ["Foo1","Util","Main"],
                        hsSourceDir = "scripts",
                        extensions = [OverlappingInstances]
                      })]
}

hunitTests :: [Test]
hunitTests = [
              TestLabel "license parsers" $ TestCase $
                 sequence_ [assertRight ("license " ++ show lVal) lVal
                                        (runP 1 "license" parseLicense (show lVal))
                           | lVal <- [GPL,LGPL,BSD3,BSD4]],

              TestLabel "Required fields" $ TestCase $
                 do assertRight "some fields"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0] ["asdf"]))}
                       (parseDescription "Name: foo\nVersion: 0.0-asdf")

                    assertRight "more fields foo"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0]["asdf"])),
                                               license=GPL}
                       (parseDescription "Name: foo\nVersion:0.0-asdf\nLicense: GPL")

                    assertRight "required fields for foo"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0]["asdf"])),
                                        license=GPL, copyright="2004 isaac jones"}
                       (parseDescription "Name: foo\nVersion:0.0-asdf\nCopyright: 2004 isaac jones\nLicense: GPL"),
                                          
             TestCase $ assertRight "no library" Nothing
                        (library `liftM` parseDescription "Name: foo\nVersion: 1\nLicense: GPL\nMaintainer: someone\n\nExecutable: script\nMain-is: SomeFile.hs\n"),

             TestLabel "Package description" $ TestCase $ 
                assertRight "entire package description" testPkgDescAnswer
                                                         (parseDescription testPkgDesc)

             ]


assertRight :: (Eq val) => String -> val -> (Either a val) -> Assertion
assertRight mes expected actual
    =  assertBool mes
           (case actual of
             (Right v) -> v == expected
             _         -> False)

isError (Left _) = True
isError _        = False

test = runTestTT (TestList hunitTests)
#endif
