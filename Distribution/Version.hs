-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Version
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Versions for packages, based on the 'Version' datatype.

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
  -- * Package versions
  Version(..),
  showVersion,
  readVersion,
  parseVersion,

  -- * Version ranges
  VersionRange(..), 
  orLaterVersion, orEarlierVersion,
  betweenVersionsInclusive,
  withinRange,
  showVersionRange,
  parseVersionRange,
  isAnyVersion,

 ) where

import Data.Version	( Version(..), showVersion )

import Control.Monad    ( liftM )
import Data.Char	( isSpace, isDigit, isAlphaNum )
import Data.Maybe	( listToMaybe )

import Distribution.Compat.ReadP

-- -----------------------------------------------------------------------------
-- Version utils

parseVersion :: ReadP r Version
parseVersion = do branch <- sepBy1 digits (char '.')
                  tags   <- many (char '-' >> munch1 isAlphaNum)
                  return Version{versionBranch=branch, versionTags=tags}
  where
    digits  :: ReadP r Int
    digits   = do first <- satisfy isDigit
                  if first == '0'
		    then return 0
		    else do rest <- munch isDigit
                            return (read (first : rest))

readVersion :: String -> Maybe Version
readVersion str =
  listToMaybe [ r | (r,s) <- readP_to_S parseVersion str, all isSpace s ]

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

isAnyVersion :: VersionRange -> Bool
isAnyVersion AnyVersion = True
isAnyVersion _ = False

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
showVersionRange (ThisVersion v) = '=' : '=' : showVersion v
showVersionRange (LaterVersion v) = '>' : showVersion v
showVersionRange (EarlierVersion v) = '<' : showVersion v
showVersionRange (UnionVersionRanges (ThisVersion v1) (LaterVersion v2))
  | v1 == v2 = '>' : '=' : showVersion v1
showVersionRange (UnionVersionRanges (LaterVersion v2) (ThisVersion v1))
  | v1 == v2 = '>' : '=' : showVersion v1
showVersionRange (UnionVersionRanges (ThisVersion v1) (EarlierVersion v2))
  | v1 == v2 = '<' : '=' : showVersion v1
showVersionRange (UnionVersionRanges (EarlierVersion v2) (ThisVersion v1))
  | v1 == v2 = '<' : '=' : showVersion v1
showVersionRange (UnionVersionRanges r1 r2) 
  = showVersionRange r1 ++ "||" ++ showVersionRange r2
showVersionRange (IntersectVersionRanges r1 r2) 
  = showVersionRange r1 ++ "&&" ++ showVersionRange r2

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

--  -----------------------------------------------------------
parseVersionRange :: ReadP r VersionRange
parseVersionRange = do
  f1 <- factor
  skipSpaces
  (do
     string "||"
     skipSpaces
     f2 <- factor
     return (UnionVersionRanges f1 f2)
   +++
   do    
     string "&&"
     skipSpaces
     f2 <- factor
     return (IntersectVersionRanges f1 f2)
   +++
   return f1)
  where 
        factor   = choice ((string "-any" >> return AnyVersion) :
                                    map parseRangeOp rangeOps)
        parseRangeOp (s,f) = string s >> skipSpaces >> liftM f parseVersion
        rangeOps = [ ("<",  EarlierVersion),
                     ("<=", orEarlierVersion),
                     (">",  LaterVersion),
                     (">=", orLaterVersion),
                     ("==", ThisVersion) ]
