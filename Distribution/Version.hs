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

module Distribution.Version where

import Time (Month(..))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

import HUnit

data Version = DateVersion {versionYear  :: Integer,
                            versionMonth :: Month,
                            versionDay   :: Integer}
             | NumberedVersion {versionMajor      :: Integer,
                                versionMinor      :: Integer,
                                versionPatchLevel :: Integer}
             | NoVersion
               deriving (Read, Show, Eq, Ord)

-- |FIX: add between versions? striclyBetween, etc?
data VersionRange
  = AnyVersion
  | ExactlyThisVersion     Version -- = version
  | OrLaterVersion         Version -- >= version
  | OrEarlierVersion       Version -- <= version
  | StrictlyLaterVersion   Version -- > version
  | StrictlyEarlierVersion Version -- < version
-- v1 < x <= v3, etc. Note exactly and any don't make sense here:
  | Between VersionRange VersionRange
    deriving (Read, Show, Eq)

number :: (Integral a, Read a) => Parser a
number  = do{ ds <- many1 digit
            ; return (read ds)
            }
        <?> "number"

showVer :: Version -> String
showVer (DateVersion yr mn day)
    = (show yr) ++ "." ++ (show mn) ++ "." ++ (show day)
showVer (NumberedVersion mj mn p)
    = (show mj) ++ "." ++ (show mn) ++ "-" ++ (show p)
showVer NoVersion = "none"

-- |Does this version fall within the given range?
withinRange :: Version -> VersionRange -> Bool
withinRange _  AnyVersion                  = True
withinRange v1 (ExactlyThisVersion v2)     = v1 == v2
withinRange v1 (OrLaterVersion v2)         = v2 <= v1
withinRange v1 (OrEarlierVersion v2)       = v1 <= v2
withinRange v1 (StrictlyEarlierVersion v2) = v1 < v2
withinRange v1 (StrictlyLaterVersion v2)   = v2 < v1
withinRange v  (Between v1 v2)   = (withinRange v v1) && (withinRange v v2)

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

word :: Parser String
word = many1 letter <?> "word"

--  -----------------------------------------------------------
parseVersionRange :: Parser VersionRange
parseVersionRange = try (do reservedOp "<"
                            v <- versionParser
                            return $ StrictlyEarlierVersion v)
                    <|> (do reservedOp ">"
                            v <- versionParser
                            return $ StrictlyLaterVersion v)
                    <|> (do reservedOp ">="
                            v <- versionParser
                            return $ OrLaterVersion v)
                    <|> (do reservedOp "<="
                            v <- versionParser
                            return $ OrEarlierVersion v)
                    <|> (do reservedOp "=="
                            v <- versionParser
                            return $ ExactlyThisVersion v)
                    <|> (do reservedOp "-"
                            reserved "any"
                            return $ AnyVersion)


--  -----------------------------------------------------------
-- |Parse any kind of version
versionParser :: Parser Version
versionParser
    = do try numberedVersionParser
         <|> dateVersionParser


--  -----------------------------------------------------------
-- |Parse a version of the form 1.2-3
numberedVersionParser :: Parser Version
numberedVersionParser
    = do n1 <- number
         char '.'
         n2 <- number
         char '-'
         n3 <- number
         return $ NumberedVersion n1 n2 n3


-- ----------------------------------------------------------
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

-- ----------------------------------------------------------
-- |Parse a version in a variety of date formats
dateVersionParser :: Parser Version
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
-- Most of the testing is for version related stuff.  Move to Version?

-- |Simple version parser wrapper
doVersionParse :: String -> Either String Version
doVersionParse input = let x = parse versionParser "" input
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

tDateVersion :: Version
tDateVersion  = DateVersion 2003 October 31
tDateVersion2 :: Version
tDateVersion2 = DateVersion 2002 November 0
tDateVersion3 :: Version
tDateVersion3 = DateVersion 2002 March 0
tDateVersion4 :: Version
tDateVersion4 = DateVersion 2002 May 0

hunitTests :: [Test]
hunitTests
    = [
       "simple dot date" ~: "failed" ~: Right tDateVersion
            ~=? doVersionParse "2003.10.31",
       "simple dash date" ~: "failed" ~: Right tDateVersion
            ~=? doVersionParse "2003-10-31",
       "year short day dot" ~: "failed"
            ~: Right tDateVersion ~=? doVersionParse "2003.Oct.31",
       "year short day dash" ~: "failed"
            ~: Right tDateVersion ~=? doVersionParse "2003-Oct-31",
       "hugs style" ~: "failed"
            ~: Right tDateVersion2 ~=? doVersionParse "Nov-2002",
       "hugs style may" ~: "failed"
            ~: Right tDateVersion3 ~=? doVersionParse "Mar-2002",
       "hugs style mar" ~: "failed"
            ~: Right tDateVersion4 ~=? doVersionParse "May-2002",
       "hugs style dot" ~: "failed"
            ~: Right tDateVersion2 ~=? doVersionParse "Nov.2002",
       "year-longmonth-day dash"
            ~: Right tDateVersion ~=? doVersionParse "2003-October-31",
       "year-longmonth-day dot"
            ~: Right tDateVersion ~=? doVersionParse "2003.October.31",
       "numbered version" ~: "failed"
            ~: (Right $ NumberedVersion 1 2 3) ~=? doVersionParse "1.2-3",

       -- Version ranges
       "greater than hugsStyle" ~: "failed"
            ~: (Right $ StrictlyLaterVersion tDateVersion2)
            ~=? doVersionRangeParse "> Nov-2002",
       "greater than hugsStyle nospace" ~: "failed"
            ~: (Right $ StrictlyLaterVersion tDateVersion2)
            ~=? doVersionRangeParse ">Nov-2002",
       "OrEarlier year-longmonth-day dash" ~: "failed"
            ~: (Right $ OrEarlierVersion tDateVersion)
            ~=? doVersionRangeParse "<=2003-October-31",
       "OrLater year-longmonth-day dash" ~: "failed"
            ~: (Right $ OrLaterVersion tDateVersion)
            ~=? doVersionRangeParse ">=2003-October-31",
       "Exactly This year-longmonth-day dot" ~: "failed"
            ~: (Right $ ExactlyThisVersion tDateVersion)
            ~=? doVersionRangeParse "==2003.October.31",
       "Any version" ~: "failed"
            ~: (Right $ AnyVersion)
            ~=? doVersionRangeParse "-any",
       "Any version space" ~: "failed"
            ~: (Right $ AnyVersion)
            ~=? doVersionRangeParse "- any",
       "range comparison OrLaterVersion" ~: "failed"
            ~: True
            ~=? tDateVersion `withinRange` (OrLaterVersion tDateVersion2),
       "range comparison Equal" ~: "failed"
            ~: True
            ~=? tDateVersion `withinRange` (ExactlyThisVersion tDateVersion),
       "range comparison OrEarlierVersion1" ~: "failed"
            ~: True
            ~=? tDateVersion2 `withinRange` (OrEarlierVersion tDateVersion),
       "range comparison OrEarlierVersion2" ~: "failed"
            ~: False
            ~=? tDateVersion `withinRange` (OrEarlierVersion tDateVersion2),
       "range comparison OrEarlierVersion3" ~: "failed"
            ~: True
            ~=? tDateVersion `withinRange` (OrEarlierVersion tDateVersion),
       "range comparison OrEarlierVersion4" ~: "failed"
            ~: True
            ~=? (NumberedVersion 1 2 3)
                    `withinRange` (OrLaterVersion $ NumberedVersion 0 0 0),
       "range comparison StrictlyGreaterVersion" ~: "failed"
            ~: False
            ~=? (NumberedVersion 2 1 0)
                    `withinRange` (StrictlyLaterVersion $ NumberedVersion 3 0 0),
       "range comparison StrictlyGreaterVersion 2" ~: "failed"
            ~: True
            ~=? (NumberedVersion 10 0 0)
                    `withinRange` (StrictlyLaterVersion $ NumberedVersion 3 0 0),
       -- Comparing versions
       "Different kinds" ~: "failed"
            ~: True ~=? (NumberedVersion 1 2 3 > tDateVersion),
       "Two dates" ~: "failed"
            ~: True ~=? (tDateVersion > tDateVersion2)
      ]

