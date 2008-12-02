-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Version
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
--                Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Exports the 'Version' type along with a parser and pretty printer. A version
-- is something like @\"1.3.3\"@. It also defines the 'VersionRange' data
-- types. Version ranges are like @\">= 1.2 && < 2\"@.

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

  -- * Version ranges
  VersionRange(..), notThisVersion,
  orLaterVersion, orEarlierVersion,
  betweenVersionsInclusive,
  withinRange,
  isAnyVersion,

 ) where

import Data.Version     ( Version(..) )

import Distribution.Text ( Text(..) )
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((+++))
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>), (<+>))
import qualified Data.Char as Char (isDigit)

-- -----------------------------------------------------------------------------
-- Version ranges

-- Todo: maybe move this to Distribution.Package.Version?
-- (package-specific versioning scheme).

data VersionRange
  = AnyVersion
  | ThisVersion            Version -- = version
  | LaterVersion           Version -- > version  (NB. not >=)
  | EarlierVersion         Version -- < version
  | WildcardVersion        Version -- == ver.*   (same as >= ver && < ver+1)
  | UnionVersionRanges      VersionRange VersionRange
  | IntersectVersionRanges  VersionRange VersionRange
  deriving (Show,Read,Eq)

isAnyVersion :: VersionRange -> Bool
isAnyVersion AnyVersion = True
isAnyVersion _ = False

notThisVersion :: Version -> VersionRange
notThisVersion v = UnionVersionRanges (EarlierVersion v) (LaterVersion v)

orLaterVersion :: Version -> VersionRange
orLaterVersion   v = UnionVersionRanges (ThisVersion v) (LaterVersion v)

orEarlierVersion :: Version -> VersionRange
orEarlierVersion v = UnionVersionRanges (ThisVersion v) (EarlierVersion v)


betweenVersionsInclusive :: Version -> Version -> VersionRange
betweenVersionsInclusive v1 v2 =
  IntersectVersionRanges (orLaterVersion v1) (orEarlierVersion v2)

foldVersionRange :: a -> (Version -> a) -> (Version -> a) -> (Version -> a)
                 -> (Version -> Version -> a)
                 -> (a -> a -> a)  -> (a -> a -> a)
                 -> VersionRange -> a
foldVersionRange anyv this later earlier wildcard union intersection = fold
  where
    fold AnyVersion                     = anyv
    fold (ThisVersion v)                = this v
    fold (LaterVersion v)               = later v
    fold (EarlierVersion v)             = earlier v
    fold (WildcardVersion v)            = wildcard v (wildcardUpperBound v)
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersection (fold v1) (fold v2)

-- | Does this version fall within the given range?
--
-- This is the evaluation function for the 'VersionRange' type.
--
withinRange :: Version -> VersionRange -> Bool
withinRange v = foldVersionRange
                   True
                   (\v'  -> versionBranch v == versionBranch v')
                   (\v'  -> versionBranch v >  versionBranch v')
                   (\v'  -> versionBranch v <  versionBranch v')
                   (\l u -> versionBranch v >= versionBranch l
                         && versionBranch v <  versionBranch u)
                   (||)
                   (&&)

----------------------------
-- Wildcard range utilities
--

wildcardUpperBound :: Version -> Version
wildcardUpperBound (Version lowerBound ts) = (Version upperBound ts)
  where
    upperBound = init lowerBound ++ [last lowerBound + 1]

-------------------------------
-- Parsing and pretty printing
--

instance Text VersionRange where
  disp AnyVersion           = Disp.text "-any"
  disp (ThisVersion    v)   = Disp.text "==" <> disp v
  disp (LaterVersion   v)   = Disp.char '>'  <> disp v
  disp (EarlierVersion v)   = Disp.char '<'  <> disp v
  disp (WildcardVersion v)  = Disp.text "==" <> dispWild v
    where dispWild (Version b _) =
               Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int b))
            <> Disp.text ".*"
  disp (UnionVersionRanges (ThisVersion  v1) (LaterVersion v2))
    | v1 == v2 = Disp.text ">=" <> disp v1
  disp (UnionVersionRanges (LaterVersion v2) (ThisVersion  v1))
    | v1 == v2 = Disp.text ">=" <> disp v1
  disp (UnionVersionRanges (ThisVersion v1) (EarlierVersion v2))
    | v1 == v2 = Disp.text "<=" <> disp v1
  disp (UnionVersionRanges (EarlierVersion v2) (ThisVersion v1))
    | v1 == v2 = Disp.text "<=" <> disp v1
  disp (UnionVersionRanges r1 r2)
    = disp r1 <+> Disp.text "||" <+> disp r2
  disp (IntersectVersionRanges r1 r2)
    = disp r1 <+> Disp.text "&&" <+> disp r2

  parse = do
    f1 <- factor
    Parse.skipSpaces
    (do
       Parse.string "||"
       Parse.skipSpaces
       f2 <- factor
       return (UnionVersionRanges f1 f2)
     +++
     do
       Parse.string "&&"
       Parse.skipSpaces
       f2 <- factor
       return (IntersectVersionRanges f1 f2)
     +++
     return f1)
   where
        factor   = Parse.choice $ parseAnyVersion
                                : parseWildcardRange
                                : map parseRangeOp rangeOps
        parseAnyVersion    = Parse.string "-any" >> return AnyVersion

        parseWildcardRange = do
          Parse.string "=="
          Parse.skipSpaces
          branch <- Parse.sepBy1 digits (Parse.char '.')
          Parse.char '.'
          Parse.char '*'
          return (WildcardVersion (Version branch []))

        digits = do
          first <- Parse.satisfy Char.isDigit
          if first == '0'
            then return 0
            else do rest <- Parse.munch Char.isDigit
                    return (read (first : rest))

        parseRangeOp (s,f) = Parse.string s >> Parse.skipSpaces >> fmap f parse
        rangeOps = [ ("<",  EarlierVersion),
                     ("<=", orEarlierVersion),
                     (">",  LaterVersion),
                     (">=", orLaterVersion),
                     ("==", ThisVersion) ]
