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
        hunitTests
#endif
  ) where

import Control.Monad(when)
import Data.Char(isSpace)

import Distribution.Version(Version(..), showVersion, parseVersion)
import Distribution.Misc(License(..), Dependency, Extension)
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
    deriving (Show, Read)

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {package      = undefined,
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

-- |Parse the given package file.  FIX: don't use read / show.
parsePackageDesc :: FilePath -> IO PackageDescription
parsePackageDesc p
    = openFile p ReadMode >>= hGetContents >>= return . read

word :: Parser String
word = many1 letter <?> "word"

--parseWordCommaList :: GenParser Char st [t]
parseCommaList :: GenParser Char st a -- ^The parser for the stuff between commas
               -> GenParser Char st [a]
parseCommaList p
    = do words <- sepBy1 p separator
         newline
         return words
    where separator = skipMany1 (space <|> char ',')

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
                   knownVal1 (p1 "Foo:\n 3.2-one \t \n  \nBar:    boo  ")
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
