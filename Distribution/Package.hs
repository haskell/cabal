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
#ifdef DEBUG        
        hunitTests,
        test
#endif
  ) where

import Control.Monad.State
import Control.Monad(when, foldM)
import Control.Monad.Error
import Data.Char(isSpace, toLower)
import Data.List(isPrefixOf)

import Distribution.Version(Version(..), VersionRange(..),
                            showVersion, parseVersion, parseVersionRange)
import Distribution.Misc(License(..), Dependency(..), Extension(..))
import Distribution.Setup(CompilerFlavor(..))

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
        mainModules    :: [ (String, String) ],
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

-- |Add options for a specific compiler. Convenience function.
setPkgOptions :: CompilerFlavor -> [String] -> PackageDescription -> PackageDescription
setPkgOptions c xs desc@PackageDescription{options=opts}
    = desc{options=(c,xs):opts}

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

data PError = Parsec ParseError | FromString String
        deriving Show

instance Error PError where
        strMsg = FromString

parseDescription :: String -> Either PError PackageDescription
parseDescription inp = foldM parseDescHelp emptyPackageDescription (splitLines inp)
  where -- Required fields
        parseDescHelp pkg (f@"name",      val) = return (setPkgName val pkg)
        parseDescHelp pkg (f@"version",   val) =
          do v <- runP f parseVersion val
             return (setPkgVersion v pkg)
        parseDescHelp pkg (f@"copyright", val) = return pkg{copyright=val}
        parseDescHelp pkg (f@"license",   val) =
          do l <- runP f parseLicense val
             return pkg{license=l}
        -- Misc.
        parseDescHelp pkg (f@"maintainer", val) = return pkg{maintainer=val}
        parseDescHelp pkg (f@"stability",  val) = return pkg{stability=val}
        parseDescHelp pkg (f@"extra-libs", val) =
          do xs <- runP f (parseCommaList word) val
             return pkg{extraLibs=xs}
        parseDescHelp pkg (f@"build-depends", val) =
          do xs <- runP f (parseCommaList parseDependency) val
             return pkg{buildDepends=xs}
        -- Paths and stuff
        parseDescHelp pkg (f@"c-sources", val) =
          do paths <- runP f (parseCommaList parseFilePath) val
             return pkg{cSources=paths}
        parseDescHelp pkg (f@"include-dirs", val) =
          do paths <- runP f (parseCommaList parseFilePath) val
             return pkg{includeDirs=paths}
        parseDescHelp pkg (f@"includes", val) =
          do paths <- runP f (parseCommaList parseFilePath) val
             return pkg{includes=paths}
        parseDescHelp pkg (f@"hs-source-dir", val) =
          do path <- runP f parseFilePath val
             return pkg{hsSourceDir=path}
        -- Module related
        parseDescHelp pkg (f@"main-modules", val) =
          do xs <- runP f (parseCommaList mainModule) val
             return pkg{mainModules=xs}
        parseDescHelp pkg (f@"exposed-modules", val) =
          do xs <- runP f (parseCommaList moduleName) val
             return pkg{exposedModules=xs}
        parseDescHelp pkg (f@"modules", val) =
          do xs <- runP f (parseCommaList moduleName) val
             return pkg{allModules=xs}
        parseDescHelp pkg (f@"extensions", val) =
          do exts <- runP f (parseCommaList parseExtension) val
             return pkg{extensions=exts}
        parseDescHelp pkg (f, val) | "options-" `isPrefixOf` f =
          let compilers = [("ghc",GHC),("nhc",NHC),("hugs",Hugs)] -- FIXME
           in case lookup (drop 8 f) compilers of
                Just c -> do xs <- runP f (parseCommaList parseOption) val
                             return (setPkgOptions c xs pkg)
                Nothing -> error $ "Unknown compiler (" ++ drop 8 f ++ ")"
        parseDescHelp pkg (field, val) = error $ "Unknown field :: " ++ field
        -- ...
        runP f p s = case parse p f s of
                       Left pe -> Left (Parsec pe)
                       Right a -> Right a

splitLines :: String -> [(String,String)]
splitLines = merge . filter validLine . lines
  where validLine s = case dropWhile isSpace s of
                        ""        -> False      -- Empty line
                        '-':'-':_ -> False      -- Comment
                        _         -> True
        merge (x:(' ':s):ys) = case dropWhile isSpace s of
                                 "." -> merge ((x++"\n"):ys)
                                 s'  -> merge ((x++"\n"++s'):ys)
        merge (x:ys) = brk x : merge ys
        merge []     = []
        brk xs = case break (==':') xs of
                   (fld, ':':val) -> (map toLower fld, dropWhile isSpace val)
                   (fld, "")      -> error "FIXME"

-- |parse a module name
moduleName = many (alphaNum <|> oneOf "_'.") <?> "moduleName"

mainModule = do filename <- word
                skipMany parseWhite
                char ':'
                skipMany parseWhite
                modname <- moduleName
                return (filename,modname)

-- |FIX: must learn to escape whitespace
parseFilePath :: GenParser Char st FilePath
parseFilePath = liftM concat (many1 (
                        do try word
                           <|> toStr digit
                           <|> toStr (oneOf "!@#$%^&*()?></\\|]}[{.")
                       ))
        <?> "parseFilePath"

parseDependency :: GenParser Char st Dependency
parseDependency = do name <- many1 (letter <|> digit <|> oneOf "-_")
                     skipMany parseWhite
                     ver <- parseVersionRange <|> return AnyVersion
                     skipMany parseWhite
                     return $ Dependency name ver
        <?> "parseDependency"

parseLicense :: GenParser Char st License
parseLicense = choice [ try (string s >> return l) | (s,l) <- licenses]
        <?> "parseLicense"

-- |Mapping between the licenses and their names
licenses :: [(String, License)]
licenses= [("GPL", GPL),
           ("LGPL", LGPL),
           ("BSD3", BSD3),
           ("BSD4", BSD4),
           ("PublicDomain", PublicDomain),
           ("AllRightsReserved", AllRightsReserved)]

parseExtension :: GenParser Char st Extension
parseExtension = choice [ try (string s >> return e) | (s,e) <- extensionsMap ]
        <?> "parseExtension"

-- |Mapping between extensions and their names
extensionsMap = [("OverlappingInstances", OverlappingInstances),
                 ("TypeSynonymInstances", TypeSynonymInstances),
                 ("TemplateHaskell", TemplateHaskell)]

parseOption = many1 (letter <|> digit <|> oneOf "-+/\\._") -- FIXME

toStr c = c >>= \x -> return [x]

word :: GenParser Char st String
word = many1 letter <?> "word"

parseCommaList :: GenParser Char st a -- ^The parser for the stuff between commas
               -> GenParser Char st [a]
parseCommaList p
    = do words <- sepBy1 p separator
         return words
    where separator = spaces >> char ',' >> spaces

parseWhite = try parseSpaceNotNewline
            <|> (try (char '\n' >> parseWhite))

parseSpaceNotNewline = (satisfy isSpaceNotNewline <?> "space, not newline")
    where isSpaceNotNewline :: Char -> Bool
          isSpaceNotNewline '\n' = False
          isSpaceNotNewline n    = isSpace n

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
        "Modules: Distribution.Package, Distribution.Version, Distribution.Simple.GHCPackageConfig",
        "Main-Modules: cabal: Distribution.Main",
        "C-Sources: not/even/rain.c, such/small/hands",
        "HS-Source-Dir: src",
        "Exposed-Modules: Distribution.Void, Foo.Bar",
        "Extensions: OverlappingInstances, TypeSynonymInstances",
        "Extra-Libs: libfoo, bar, bang",
        "Include-Dirs: your/slightest, look/will",
        "Includes: /easily/unclose, /me",
        "Options-ghc: -fTH",
        "Options-hugs: +TH"
        ]

testPkgDescAnswer = 
 PackageDescription {package = PackageIdentifier {pkgName = "Cabal",
                                                 pkgVersion = Version {versionBranch = [0,1,1,1,1],
                                                 versionTags = ["rain"]}},
                    license = LGPL,
                    copyright = "Free Text String",
                    maintainer = "",
                    stability = "Free Text String",
                    buildDepends = [Dependency "haskell-src" AnyVersion,
                                    Dependency "HUnit"
                                     (UnionVersionRanges (ThisVersion (Version [1,0,0] ["rain"]))
                                      (LaterVersion (Version [1,0,0] ["rain"])))],

                    allModules = ["Distribution.Package","Distribution.Version",
                                  "Distribution.Simple.GHCPackageConfig"],

                    mainModules = [("cabal","Distribution.Main")],
                    cSources = ["not/even/rain.c", "such/small/hands"],
                    hsSourceDir = "src",
                    exposedModules = ["Distribution.Void", "Foo.Bar"],
                    extensions = [OverlappingInstances, TypeSynonymInstances],
                    extraLibs = ["libfoo", "bar", "bang"],
                    includeDirs = ["your/slightest", "look/will"],
                    includes = ["/easily/unclose", "/me"],
                    options = [(Hugs,["+TH"]), (GHC,["-fTH"])] -- Note reversed order
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

--              TestLabel "basic fields" $ TestCase $
--              do let p1 = parse (do w1 <- parseField "Foo" False parseVersion
--                                    skipMany parseWhite
--                                    w2 <- parseField "Bar" True word
--                                    return (w1, w2)
--                                ) ""
--                     knownVal1 = (Version {versionBranch = [3,2], versionTags = ["one"]},"boo")
--                 assertRight "basic spaces 1"
--                   knownVal1 (p1 "Foo: 3.2-one\nBar: boo")
--                 assertRight "basic spaces 2"
--                   knownVal1 (p1 "Foo: 3.2-one \t   \nBar: boo")
--                 assertRight "basic spaces 3"
--                   knownVal1 (p1 "Foo : 3.2-one \t   \nBar:    boo  ")
--                 assertRight "basic spaces 3"
--                   knownVal1 (p1 "Foo:3.2-one \t   \nBar:    boo  ")
--                 assertRight "basic spaces with newline"
--                   knownVal1 (p1 "Foo:\n 3.2-one \t   \nBar:    boo  ")
--                 assertRight "basic spaces with newline"
--                   knownVal1 (p1 "Foo:\n 3.2-one \t \n  \nBar:    boo  "),

              TestCase (assertRight "BSD4" BSD4 (parse parseLicense "" "BSD4")),

              TestLabel "license parsers" $ 
                        TestCase (sequence_ [assertRight ("license " ++ lName) lVal
                                                    (parse parseLicense "" lName)
                                             | (lName, lVal) <- licenses]),

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
