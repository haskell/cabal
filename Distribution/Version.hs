-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Version
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: Represents and parses versions like Nov-2003, 1.2-4, etc.

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

module Distribution.Version (
  -- * The Version type
  Version(..),

  -- * Package versions
  showVersion,
  parseVersion,

  -- ** Version ranges
  VersionRange(..), 
  orLaterVersion, orEarlierVersion,
  betweenVersionsInclusive,
  withinRange,
  showVersionRange,
  parseVersionRange,
  hunitTests
 ) where

import Data.List	( intersperse )

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

import HUnit

-- -----------------------------------------------------------------------------
-- The Version type

{- |
A 'Version' represents the version of a software entity.

An instance 'Eq' is provided, which implements exact equality modulo
reordering of the tags in the 'versionTags' field.  

The interpretation of ordering is dependent on the entity being
versioned, and perhaps the application.  For example, simple branch
ordering is probably sufficient for many uses (see the 'versionBranch'
field), but some versioning schemes may include pre-releases which
have tags @"pre1"@, @"pre2"@, and so on, and these would need to be
taken into account when determining ordering.  In some cases, date
ordering may be more appropriate, so the application would have to
look for @date@ tags in the 'versionTags' field and compare those.

Similarly, concrete representations of versions may differ, so we leave
parsing and printing up to the application.
-}
data Version = 
  Version { versionBranch :: [Int],
		-- ^ The numeric branch for this version.  This reflects the
		-- fact that most software versions are tree-structured; there
		-- is a main trunk which is tagged with versions at various
		-- points (1,2,3...), and the first branch off the trunk after
		-- version 3 is 3.1, the second branch off the trunk after
		-- version 3 is 3.2, and so on.  The tree can be branched
		-- arbitrarily, just by adding more digits.
		-- 
		-- We represent the branch as a list of 'Int', so
		-- version 3.2.1 becomes [3,2,1].  Lexicographic ordering
		-- (i.e. the default instance of 'Ord' for @[Int]@) gives
		-- the natural ordering of branches.

	   versionTags :: [String]  -- really a bag
		-- ^ A version can be tagged with an arbitrary list of strings.
		-- The interpretation of the list of tags is entirely dependent
		-- on the entity that this version applies to.
	}
  deriving (Read,Show)

instance Eq Version where
  v1 == v2  =  versionBranch v1 == versionBranch v2 
		&& all (`elem` (versionTags v2)) (versionTags v1)
		-- tags may be in any order

-- -----------------------------------------------------------------------------
-- Package Versions

-- Todo: maybe move this to Distribution.Package.Version?
-- (package-specific versioning scheme).

-- Our conventions:
--
--	* Versions are of the form  A.B.C-tag1-tag2
--
--	* Ordering is determined by lexicographic ordering of the
--	  numeric part of the version only.

showVersion :: Version -> String
showVersion (Version branch tags)
  = concat (intersperse "." (map show branch)) ++ 
    concat (map ('-':) tags)

-- -----------------------------------------------------------------------------
-- Version ranges

-- Todo: maybe move this to Distribution.Package.Version?
-- (package-specific versioning scheme).

data VersionRange
  = AnyVersion
  | ThisVersion		   Version -- = version
  | LaterVersion	   Version -- > version  (NB. not >=)
  | EarlierVersion	   Version -- < version
	-- ToDo: are these too general?
  | UnionVersionRanges      VersionRange VersionRange
  | IntersectVersionRanges  VersionRange VersionRange
  deriving (Show,Read,Eq)

orLaterVersion :: Version -> VersionRange
orLaterVersion   v = UnionVersionRanges (ThisVersion v) (LaterVersion v)

orEarlierVersion :: Version -> VersionRange
orEarlierVersion v = UnionVersionRanges (ThisVersion v) (EarlierVersion v)


betweenVersionsInclusive :: Version -> Version -> VersionRange
betweenVersionsInclusive v1 v2 =
  IntersectVersionRanges (orLaterVersion v1) (orEarlierVersion v2)

laterVersion :: Version -> Version -> Bool
v1 `laterVersion`   v2 = versionBranch v1 > versionBranch v2

earlierVersion :: Version -> Version -> Bool
v1 `earlierVersion` v2 = versionBranch v1 < versionBranch v2

-- |Does this version fall within the given range?
withinRange :: Version -> VersionRange -> Bool
withinRange _  AnyVersion                = True
withinRange v1 (ThisVersion v2) 	 = v1 == v2
withinRange v1 (LaterVersion v2)         = v1 `laterVersion` v2
withinRange v1 (EarlierVersion v2)       = v1 `earlierVersion` v2
withinRange v1 (UnionVersionRanges v2 v3) 
   = v1 `withinRange` v2 || v1 `withinRange` v3
withinRange v1 (IntersectVersionRanges v2 v3) 
   = v1 `withinRange` v2 && v1 `withinRange` v3

showVersionRange :: VersionRange -> String
showVersionRange AnyVersion = "-any"
showVersionRange (ThisVersion v) = '=' : showVersion v
showVersionRange (LaterVersion v) = '>' : showVersion v
showVersionRange (EarlierVersion v) = '<' : showVersion v
showVersionRange (UnionVersionRanges r1 r2) 
  = showVersionRange r1 ++ "||" ++ showVersionRange r2
showVersionRange (IntersectVersionRanges r1 r2) 
  = showVersionRange r1 ++ "&&" ++ showVersionRange r2

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

word :: Parser String
word = many1 letter <?> "word"

--  -----------------------------------------------------------
parseVersionRange :: Parser VersionRange
parseVersionRange = try (do reservedOp "<"
                            v <- parseVersion
                            return $ EarlierVersion v)
                    <|> (do reservedOp ">"
                            v <- parseVersion
                            return $ LaterVersion v)
                    <|> (do reservedOp ">="
                            v <- parseVersion
                            return $ orLaterVersion v)
                    <|> (do reservedOp "<="
                            v <- parseVersion
                            return $ orEarlierVersion v)
                    <|> (do reservedOp "=="
                            v <- parseVersion
                            return $ ThisVersion v)
                    <|> (do reservedOp "-"
                            reserved "any"
                            return $ AnyVersion)


--  -----------------------------------------------------------
-- |Parse any kind of version
parseVersion :: Parser Version
parseVersion
    = do branch <- branchParser
	 date <- dateParser
	 return (Version{versionBranch=branch, versionTags=date})

--  -----------------------------------------------------------
-- |Parse a version of the form 1.2.3
branchParser :: Parser [Int]
branchParser
    = do n <- number
	 bs <- branches
	 return (n : bs)

branches :: Parser [Int]
branches
    = option [] $ do
	  char '.'
	  n <- number
          bs <- branches
          return (n:bs)

dateParser :: Parser [String]
dateParser
     = (try $ do char '-'; d <- word; return ["date="++d])
       <|> (do notFollowedBy anyChar; return [])

number :: (Integral a, Read a) => Parser a
number  = do{ ds <- many1 digit
            ; return (read ds)
            }
        <?> "number"

-- -----------------------------------------------------------------------------
-- Parsing dates

{-
-- Here is some code for parsing dates.  We might need this at some point.

-- |Seperate the date with typically a '.' or a '-', /sep/
dateSeparatedBy :: Char -> GenParser Char () Version
dateSeparatedBy sep
    = try (do year  <- number -- 2003.01.15, 2003.1.15
              char sep
              month <- number
              char sep
              day   <- number
              return $ DateVersion year (toEnum $ month - 1) day)
      <|>  try (do year  <- number -- 2003-Jan-15
                   char sep
                   month <- shortMonthParser
                   char sep
                   day   <- number
                   return $ DateVersion year month day)

      <|>  try (do month <- shortMonthParser -- Nov-2002
                   char sep
                   year  <- number
                   return $ DateVersion year month 0)

      <|>  try (do year  <- number -- 2003-January-15
                   char sep
                   month <- word
                   char sep
                   day   <- number
                   return $ DateVersion year (read month) day)

dateVersionParser :: Parser String
dateVersionParser 
    = try (dateSeparatedBy '.')
      <|> (dateSeparatedBy '-')

shortMonthParser :: Parser Month
shortMonthParser = foldl1 (<|>) [do reserved a;return b | (a,b)
                                 <- [("Jan", January),   ("Feb", February),
                                     ("Mar", March),     ("Apr", April), 
                                     ("May", May),       ("Jun", June),
                                     ("Jul", July),      ("Aug", August),
                                     ("Sep", September), ("Oct", October),
                                     ("Nov", November),  ("Dec", December)]]
-}

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser 
         (emptyDef

         { P.reservedNames = ["Jan","Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "any"
                             ],
           P.identStart    = letter <|> char '_',
           P.identLetter    = alphaNum <|> oneOf "_'",
           P.reservedOpNames = ["<", ">", "<=", ">=", "==", "-"]
         })

whiteSpace :: CharParser () ()
whiteSpace = P.whiteSpace lexer

lexeme :: CharParser () a -> CharParser () a
lexeme = P.lexeme lexer

symbol :: String -> CharParser () String
symbol = P.symbol lexer

natural :: CharParser () Integer
natural = P.natural lexer

parens :: CharParser () a -> CharParser () a
parens  = P.parens lexer

semi :: CharParser () String
semi = P.semi lexer

identifier :: CharParser () String
identifier = P.identifier lexer

reserved :: String -> CharParser () ()
reserved = P.reserved lexer

reservedOp :: String -> CharParser () ()
reservedOp = P.reservedOp lexer

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

-- |Simple version parser wrapper
doVersionParse :: String -> Either String Version
doVersionParse input = let x = parse parseVersion "" input
                        in case x of
                           Left err -> Left (show err)
                           Right y  -> Right y

-- |Version range parsing
doVersionRangeParse :: String -> Either String VersionRange
doVersionRangeParse input
    = let x = parse parseVersionRange "" input
          in case x of
             Left err -> Left (show err)
             Right y  -> Right y
branch1 :: [Int]
branch1 = [1]

branch2 :: [Int]
branch2 = [1,2]

branch3 :: [Int]
branch3 = [1,2,3]

branch4 :: [Int]
branch4 = [1,2,3,4]

release1 :: Version
release1 = Version{versionBranch=branch1, versionTags=[]}

release2 :: Version
release2 = Version{versionBranch=branch2, versionTags=[]}

release3 :: Version
release3 = Version{versionBranch=branch3, versionTags=[]}

release4 :: Version
release4 = Version{versionBranch=branch4, versionTags=[]}

hunitTests :: [Test]
hunitTests
    = [
       "released version 1" ~: "failed"
            ~: (Right $ release1) ~=? doVersionParse "1",
       "released version 3" ~: "failed"
            ~: (Right $ release3) ~=? doVersionParse "1.2.3",

       -- Version ranges
       "Any version" ~: "failed"
            ~: (Right $ AnyVersion)
            ~=? doVersionRangeParse "-any",
       "Any version space" ~: "failed"
            ~: (Right $ AnyVersion)
            ~=? doVersionRangeParse "- any",
       "range comparison LaterVersion 1" ~: "failed"
            ~: True
            ~=? release3 `withinRange` (LaterVersion release2),
       "range comparison LaterVersion 2" ~: "failed"
            ~: False
            ~=? release2 `withinRange` (LaterVersion release3),
       "range comparison EarlierVersion 1" ~: "failed"
            ~: True
            ~=? release3 `withinRange` (LaterVersion release2),
       "range comparison EarlierVersion 2" ~: "failed"
            ~: False
            ~=? release2 `withinRange` (LaterVersion release3),
       "range comparison orLaterVersion 1" ~: "failed"
            ~: True
            ~=? release3 `withinRange` (orLaterVersion release3),
       "range comparison orLaterVersion 2" ~: "failed"
            ~: True
            ~=? release3 `withinRange` (orLaterVersion release2),
       "range comparison orLaterVersion 3" ~: "failed"
            ~: False
            ~=? release2 `withinRange` (orLaterVersion release3),
       "range comparison orEarlierVersion 1" ~: "failed"
            ~: True
            ~=? release2 `withinRange` (orEarlierVersion release2),
       "range comparison orEarlierVersion 2" ~: "failed"
            ~: True
            ~=? release2 `withinRange` (orEarlierVersion release3),
       "range comparison orEarlierVersion 3" ~: "failed"
            ~: False
            ~=? release3 `withinRange` (orEarlierVersion release2)
      ]

