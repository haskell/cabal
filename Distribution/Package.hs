{-# OPTIONS -cpp -DDEBUG #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Package
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

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
        parsePackageDesc,
        parseField,
#ifdef DEBUG        
        hunitTests,
        main
#endif
  ) where

import Control.Monad(when, liftM)
import Data.Char(isSpace)

import Distribution.Version(Version(..), VersionRange(..),
                            showVersion, parseVersion, parseVersionRange)
import Distribution.Misc(License(..), Dependency(..), Extension)
import Distribution.Setup(CompilerFlavor)

import System.IO(openFile, IOMode(..), hGetContents)

import Text.ParserCombinators.Parsec

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

	-- the following are required by the simple build infrastructure only:
        buildDepends   :: [ Dependency ],
        allModules     :: [ String ],
        mainModules    :: [ String ],
        cSources       :: [ FilePath ],
	hsSourceDir    :: FilePath,
	exposedModules :: [ String ],
        extensions     :: [ Extension ],
        extraLibs      :: [ String ],
        includeDirs    :: [ FilePath ],
        includes       :: [ FilePath ],
        options        :: [ (CompilerFlavor, [String]) ]
    }
    deriving (Show, Read, Eq)

-- |Set the name for this package. Convenience function.
setPkgName :: String -> PackageDescription -> PackageDescription
setPkgName n desc@PackageDescription{package=pkgIdent}
    = desc{package=pkgIdent{pkgName=n}}

-- |Set the version for this package. Convenience function.
setPkgVersion :: Version -> PackageDescription -> PackageDescription
setPkgVersion v desc@PackageDescription{package=pkgIdent}
    = desc{package=pkgIdent{pkgVersion=v}}

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {package      = PackageIdentifier "" (Version [] []),
                      license      = AllRightsReserved,
                      copyright    = "",
                      maintainer   = "",
                      stability    = "",
                      buildDepends = [],
                      allModules   = [],
                      mainModules   = [],
		      cSources     = [],
		      hsSourceDir  = ".", -- FIX: FileUtils.currentDir
		      exposedModules = [],
                      extensions   = [],
                      extraLibs    = [],
                      includeDirs  = [],
                      includes     = [],
                      options      = []
                     }


-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

notImp :: String -> a
notImp s = error $ s ++ " not yet implemented"

-- |Parse the given package file.  FIX: don't use read / show.
parsePackageDesc :: FilePath -> IO PackageDescription
parsePackageDesc p
    = openFile p ReadMode >>= hGetContents >>= return . read

-- |Wrapper function for 'parseDesc'
doParseDesc :: String -> Either ParseError PackageDescription
doParseDesc = runParser parseReqFields emptyPackageDescription ""

-- |High-level parser for package descriptions
parseDesc :: GenParser Char PackageDescription PackageDescription
parseDesc = (many1 (parseReqFields >> parseDescHelp)) >> getState
    where
    parseDescHelp
           -- misc
        =  try (parseField "Stability" False word
               >>= updateState . (\l pkgD -> pkgD{stability=l}))
           <|> try (parseField "Extra-Libs" True (parseCommaList word)
               >>= updateState . (\l pkgD -> pkgD{extraLibs=l}))
           <|> try (parseField "Build-Depends" True (parseCommaList parseDependency)
               >>= updateState . (\l pkgD -> pkgD{buildDepends=l}))
           -- File-path-related
           <|> try (parseField "C-Sources" True parseFilePath
               >>= updateState . (\l pkgD -> pkgD{cSources=l}))
           <|> try (parseField "Include-Dirs" True parseFilePath
               >>= updateState . (\l pkgD -> pkgD{includeDirs=l}))
           <|> try (parseField "Includes" True parseFilePath
               >>= updateState . (\l pkgD -> pkgD{includes=l}))
           <|> try (parseField "HS-Source-Dir" True parseFilePath
               >>= updateState . (\l pkgD -> pkgD{hsSourceDir=head l}))
           -- module related
           <|> try (parseField "Main-Modules" True (parseCommaList moduleName)
               >>= updateState . (\l pkgD -> pkgD{mainModules=l}))
           <|> try (parseField "Exposed-Modules" True (parseCommaList moduleName)
               >>= updateState . (\l pkgD -> pkgD{exposedModules=l}))
           <|> try (parseField "Modules" True (parseCommaList moduleName)
               >>= updateState . (\l pkgD -> pkgD{allModules=l}))

-- Parsing remains for:
--
-- Options: ghc: -fTH, hugs: +TH
-- Extensions: {some known extensions}

-- |Wrapper function for 'parseReqFields'
doParseReqFields :: String -> Either ParseError PackageDescription
doParseReqFields = runParser parseReqFields emptyPackageDescription ""

-- |Parse the required fields. We'll basically run through these a
-- bunch of times updating the state as we go along, as is noted.

parseReqFields :: GenParser Char PackageDescription PackageDescription
parseReqFields = many1 parseReqFieldsHelp >> getState
    where
    parseReqFieldsHelp :: GenParser Char PackageDescription ()
    parseReqFieldsHelp
        =  try (parseField "Name" False word
               >>= (updateState . setPkgName))
           <|> try (parseField "Version" True parseVersion
               >>= (updateState . setPkgVersion))
           <|> try (parseField "Copyright" True parseFreeText
               >>= (updateState . (\l pkgD -> pkgD{copyright=l})))
           <|> try (parseField "License" True parseLicense
               >>= (updateState . (\c pkgD -> pkgD{license=c})))

-- |A parser for any of the given parsers.  This actually seems to
-- behave differently from "choice".

anyOf :: [GenParser tok st a] -> GenParser tok st a
anyOf [a] = a
anyOf (h:t) = foldl ((<|>) . try) (try h) t

-- |parse a module name
moduleName = many (alphaNum <|> oneOf "_'.")

-- |FIX: must learn to escape whitespace
parseFilePath :: GenParser Char st [FilePath]
parseFilePath
    = parseCommaList (many1 (do try word
                                <|> toStr digit
                                <|> toStr (oneOf "!@#$%^&*()?></\\|]}[{")
                            ) >>= return . concat)

parseLicense :: GenParser Char st License
parseLicense = anyOf [string s>>return l | (s,l) <- licenses]

parseDependency :: GenParser Char st Dependency
parseDependency = do name <- word
                     skipMany parseWhite
                     ver  <- parseVersionRange
                     skipMany parseWhite
                     return $ Dependency name ver

-- |Mapping between the licenses and their names
licenses :: [(String, License)]
licenses= [("GPL", GPL),
           ("LGPL", LGPL),
           ("BSD3", BSD3),
           ("BSD4", BSD4),
           ("PublicDomain", PublicDomain),
           ("AllRightsReserved", AllRightsReserved)]

-- |FIX: Could be better.  The problem is making it free enough without
-- eating the next field.
parseFreeText = many1 (do try word
                          <|> toStr digit
                          <|> toStr parseWhite
                          <|> toStr (oneOf "!@#$%^&*()?></\\|]}[{")
                      )
                >>= return . concat
toStr c = c >>= \x -> return [x]

word :: GenParser Char st String
word = many1 letter <?> "word"

number  :: Parser Integer
number  = do{ ds <- many1 digit
            ; return (read ds)
            }
        <?> "number"

--parseWordCommaList :: GenParser Char st [t]
parseCommaList :: GenParser Char st a -- ^The parser for the stuff between commas
               -> GenParser Char st [a]
parseCommaList p
    = do words <- sepBy1 p separator
         return words
    where separator =  (skipMany parseWhite) >> char ',' >> (skipMany parseWhite)

parseWhite = try parseSpaceNotNewline
            <|> (try (char '\n' >> parseWhite))

parseSpaceNotNewline = (satisfy isSpaceNotNewline <?> "space, not newline")
    where isSpaceNotNewline :: Char -> Bool
          isSpaceNotNewline '\n' = False
          isSpaceNotNewline n    = isSpace n

parseField :: String              -- ^The field name to parse
           -> Bool                -- ^Require newline?
           -> GenParser Char st t -- ^The parser to use for this field
           -> GenParser Char st t
parseField s newline p
    = do when newline (char '\n'>>return ())
         string s
         skipMany parseWhite
         char ':'
         skipMany parseWhite
         p

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
testPkgDesc = "-- Required\nName: Cabal\nVersion: 0.1.1.1.1-foo-bar-bang\nLicense: LGPL\nCopyright: Free Text String\n-- Optional - may be in source?\nStability: Free Text String\nBuild-Depends: haskell-src, HUnit>=1.0.0-foo\nModules: Distribution.Package, Distribution.Version, Distribution.Simple.GHCPackageConfig\nMain-Modules: Distribution.Main\nC-Sources: foo/bar/bang.c, bong/boing.h\nHS-Source-Dir: src\nExposed-Modules: Distribution.Void, Foo.Bar\nExtensions: {some known extensions}\nExtra-Libs: libfoo, bar, bang\nInclude-Dirs: foo/bar, fang/fong\nIncludes: /foo/bar, jedi/night\nOptions: ghc: -fTH, hugs: +TH"

testPkgDescAnswer = 
 PackageDescription {package = PackageIdentifier {pkgName = "Cabal",
                                                 pkgVersion = Version {versionBranch = [0,1],
                                                 versionTags = []}},
                    license = LGPL,
                    copyright = "",
                    maintainer = "",
                    stability = "",
                    buildDepends = [Dependency "haskell-src" AnyVersion,
                                    Dependency "HUnit-1.0" AnyVersion],

                    allModules = ["Distribution.Package","Distribution.Version",
                                  "Distribution.Misc","Distribution.Setup",
                                  "Distribution.InstalledPackageInfo",
                                  "Distribution.Make","Distribution.Simple",
                                  "Distribution.Simple.Build",
                                  "Distribution.Simple.Install","Distribution.Simple.SrcDist",
                                  "Distribution.Simple.Configure","Distribution.Simple.Utils",
                                  "Distribution.Simple.Register",
                                  "Distribution.Simple.GHCPackageConfig",
                                  "Distribution.GetOpt"],

                    mainModules = [],
                    cSources = [],
                    hsSourceDir = ".",
                    exposedModules = [],
                    extensions = [],
                    extraLibs = [],
                    includeDirs = [],
                    includes = [],
                    options = []
}

hunitTests :: [Test]
hunitTests = [TestLabel "newline before word (parsewhite)" $ TestCase $
              do assertRight "newline before word 1"
                  "foo" (parse (skipMany parseWhite>>char '\n'>>word) "" "   \n  \nfoo")
                 assertRight "newline before word 2"
                  "foo" (parse (skipMany parseWhite>>char '\n'>>word) "" "   \n \t    \n  \nfoo"),

              TestLabel "skip spaces not newlines" $ TestCase $
              do assertRight "spaces with newlines"
                  "foo" (parse (skipMany parseWhite>>word) "" "   \n  foo")
                 assertRight "spaces with newlines"
                  "foo" (parse (skipMany parseWhite>>word) "" "   \n \t\n   foo")
                 assertRight "no preceding spaces"
                  "foo" (parse (skipMany parseWhite>>word) "" "foo")
                 assertBool "newline before data without in-between spaces"
                  (isError (parse (skipMany parseWhite>>word) "" "   \n  \nfoo")),

              TestLabel "basic fields" $ TestCase $
              do let p1 = parse (do w1 <- parseField "Foo" False parseVersion
                                    skipMany parseWhite
                                    w2 <- parseField "Bar" True word
                                    return (w1, w2)
                                ) ""
                     knownVal1 = (Version {versionBranch = [3,2], versionTags = ["date=one"]},"boo")
                 assertRight "basic spaces 1"
                   knownVal1 (p1 "Foo: 3.2-one\nBar: boo")
                 assertRight "basic spaces 2"
                   knownVal1 (p1 "Foo: 3.2-one \t   \nBar: boo")
                 assertRight "basic spaces 3"
                   knownVal1 (p1 "Foo : 3.2-one \t   \nBar:    boo  ")
                 assertRight "basic spaces 3"
                   knownVal1 (p1 "Foo:3.2-one \t   \nBar:    boo  ")
                 assertRight "basic spaces with newline"
                   knownVal1 (p1 "Foo:\n 3.2-one \t   \nBar:    boo  ")
                 assertRight "basic spaces with newline"
                   knownVal1 (p1 "Foo:\n 3.2-one \t \n  \nBar:    boo  "),

              TestCase (assertRight "BSD4" BSD4 (parse parseLicense "" "BSD4")),

              TestLabel "license parsers" $ 
                        TestCase (sequence_ [assertRight ("license " ++ lName) lVal
                                                    (parse parseLicense "" lName)
                                             | (lName, lVal) <- licenses]),
              TestLabel "misc fields" $ TestCase $
                 do let someModules = ["Somewhere.I.Have.Never.Traveled",
                                       "Gladly.Beyond.Any.Experience",
                                       "Your.Eyes.Have.Their.Silence"]
                    let someModulesText = "Somewhere.I.Have.Never.Traveled\t   , Gladly.Beyond.Any.Experience,  \tYour.Eyes.Have.Their.Silence"
                    assertRight "path field"
                       ["foo/bar/bang","/baz/boom/pow", "/", "foob"]
                       (parse (parseField "Includes" False parseFilePath) ""
                         "Includes: foo/bar/bang   , /baz/boom/pow, /, foob")
                    assertRight "dependencies"
                       [Dependency "not" (LaterVersion (Version [0]   [])),
                        Dependency "even" (ThisVersion (Version [3,3] ["date=the"])),
                        Dependency "rain"
                           (UnionVersionRanges (ThisVersion (Version [3,3] []))
                                               (LaterVersion (Version[3,3] [])))]
                       (parse (parseField "Build-Depends" False
                               (parseCommaList parseDependency)) ""
                               "Build-Depends: not>0, even   ==  3.3-the   ,   rain>=3.3")
                    -- Module-related fields
                    assertRight "main modules field"
                       someModules (parse (parseField "Main-Modules" False
                                           (parseCommaList moduleName)) "" 
                                           ("Main-Modules: " ++ someModulesText))
                    assertRight "exposed modules field"
                       someModules (parse (parseField "Exposed-Modules" False
                                           (parseCommaList moduleName)) "" 
                                           ("Exposed-Modules: " ++ someModulesText))
                    assertRight "modules field"
                       someModules (parse (parseField "Modules" False
                                           (parseCommaList moduleName)) "" 
                                           ("Modules: " ++ someModulesText))
                    assertRight "extra libs"
                       ["inYour", "libMostFrail", "gestures"]
                       (parse (parseField "Extra-Libs" False (parseCommaList word))
                               "" "Extra-Libs: inYour\t, libMostFrail,gestures"),

              TestLabel "Required fields" $ TestCase $
                 do assertRight "some fields"
                       emptyPackageDescription{package=(PackageIdentifier "rain"
                                                        (Version [0,0] ["date=asdf"]))}
                       (doParseReqFields "Name: rain\nVersion: 0.0-asdf")
                    assertRight "more fields rain"
                       emptyPackageDescription{package=(PackageIdentifier "rain"
                                                        (Version [0,0]["date=asdf"])),
                                               license=GPL}
                       (doParseReqFields "Name: rain\nVersion:0.0-asdf\nLicense: GPL")

                    assertRight "copyright field"
                       "(c) 2004 foo bar bang"
                       (parse (parseField "Copyright" False parseFreeText) ""
                                 "Copyright: (c) 2004 foo bar\n bang")

                    assertRight "required fields for foo"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0]["date=asdf"])),
                                        license=GPL, copyright="2004 isaac jones"}
                       (doParseReqFields "Name: foo\nVersion:0.0-asdf\nCopyright: 2004 isaac jones\nLicense: GPL"),
                                          

              TestLabel "Package description" $ TestCase $ 
                 assertRight "entire package description" testPkgDescAnswer
                                                          (doParseDesc testPkgDesc)
             ]


assertRight :: (Eq val) => String -> val -> (Either a val) -> Assertion
assertRight mes expected actual
    =  assertBool mes
           (case actual of
             (Right v) -> v == expected
             _         -> False)

isError (Left _) = True
isError _        = False

main = runTestTT (TestList hunitTests)
#endif
