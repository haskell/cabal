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
  VersionRange(..),

  -- ** Constructing
  anyVersion, noVersion,
  thisVersion, notThisVersion,
  laterVersion, earlierVersion,
  orLaterVersion, orEarlierVersion,
  unionVersionRanges, intersectVersionRanges,
  betweenVersionsInclusive,

  -- ** General
  withinRange,
  isAnyVersion,
  isNoVersion,
  isSpecificVersion,
  simplifyVersionRange,
  foldVersionRange,

  -- * Version intervals view
  VersionIntervals(..),
  LowerBound(..),
  UpperBound(..),
  Bound(..),
  toVersionIntervals,
  fromVersionIntervals,
  withinIntervals,

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

{-# DEPRECATED AnyVersion "Use 'anyVersion', 'foldVersionRange' or 'toVersionIntervals'" #-}
{-# DEPRECATED ThisVersion "use 'thisVersion', 'foldVersionRange' or 'toVersionIntervals'" #-}
{-# DEPRECATED LaterVersion "use 'laterVersion', 'foldVersionRange' or 'toVersionIntervals'" #-}
{-# DEPRECATED EarlierVersion "use 'earlierVersion', 'foldVersionRange' or 'toVersionIntervals'" #-}
{-# DEPRECATED WildcardVersion "use 'anyVersion', 'foldVersionRange' or 'toVersionIntervals'" #-}
{-# DEPRECATED UnionVersionRanges "use 'unionVersionRanges', 'foldVersionRange' or 'toVersionIntervals'" #-}
{-# DEPRECATED IntersectVersionRanges "use 'intersectVersionRanges', 'foldVersionRange' or 'toVersionIntervals'" #-}

anyVersion :: VersionRange
anyVersion = AnyVersion

noVersion :: VersionRange
noVersion = IntersectVersionRanges (LaterVersion v) (EarlierVersion v)
  where v = Version [1] []

thisVersion :: Version -> VersionRange
thisVersion = ThisVersion

notThisVersion :: Version -> VersionRange
notThisVersion v = UnionVersionRanges (EarlierVersion v) (LaterVersion v)

laterVersion :: Version -> VersionRange
laterVersion = LaterVersion

orLaterVersion :: Version -> VersionRange
orLaterVersion   v = UnionVersionRanges (ThisVersion v) (LaterVersion v)

earlierVersion :: Version -> VersionRange
earlierVersion = EarlierVersion

orEarlierVersion :: Version -> VersionRange
orEarlierVersion v = UnionVersionRanges (ThisVersion v) (EarlierVersion v)

unionVersionRanges :: VersionRange -> VersionRange -> VersionRange
unionVersionRanges = UnionVersionRanges

intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges = IntersectVersionRanges

betweenVersionsInclusive :: Version -> Version -> VersionRange
betweenVersionsInclusive v1 v2 =
  IntersectVersionRanges (orLaterVersion v1) (orEarlierVersion v2)

foldVersionRange :: a -> (Version -> a) -> (Version -> a) -> (Version -> a)
                 -> (Version -> Version -> a)
                 -> (a -> a -> a)  -> (a -> a -> a)
                 -> VersionRange -> a
foldVersionRange anyv this later earlier wildcard union intersect = fold
  where
    fold AnyVersion                     = anyv
    fold (ThisVersion v)                = this v
    fold (LaterVersion v)               = later v
    fold (EarlierVersion v)             = earlier v
    fold (WildcardVersion v)            = wildcard v (wildcardUpperBound v)
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersect (fold v1) (fold v2)

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

-- | Does this 'VersionRange' place any restriction on the 'Version' or is it
-- in fact equivalent to 'AnyVersion'.
--
-- Note this is a semantic check, not simply a syntactic check. So for example
-- the following is @True@ (for all @v@).
--
-- > isAnyVersion (EarlierVersion v `UnionVersionRanges` orLaterVersion v)
--
isAnyVersion :: VersionRange -> Bool
isAnyVersion vr = case toVersionIntervals vr of
  VersionIntervals [(NoLowerBound, NoUpperBound)] -> True
  _                                               -> False

-- | This is the converse of 'isAnyVersion'. It check if the version range is
-- empty, if there is no possible version that satisfies the version range.
--
-- For example this is @True@ (for all @v@):
--
-- > isNoVersion (EarlierVersion v `IntersectVersionRanges` LaterVersion v)
--
isNoVersion :: VersionRange -> Bool
isNoVersion vr = case toVersionIntervals vr of
  VersionIntervals [] -> True
  _                   -> False

-- | Is this version range in fact just a specific version?
--
-- For example the version range @\">= 3 && <= 3\"@ contains only the version
-- @3@.
--
isSpecificVersion :: VersionRange -> Maybe Version
isSpecificVersion vr = case toVersionIntervals vr of
  VersionIntervals [(LowerBound v  InclusiveBound
                    ,UpperBound v' InclusiveBound)]
                   | v == v' -> Just v
  _                          -> Nothing

-- | Simplify a 'VersionRange' expression into a canonical form.
--
-- It just uses @fromVersionIntervals . toVersionIntervals@
--
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange = fromVersionIntervals . toVersionIntervals

----------------------------
-- Wildcard range utilities
--

wildcardUpperBound :: Version -> Version
wildcardUpperBound (Version lowerBound ts) = (Version upperBound ts)
  where
    upperBound = init lowerBound ++ [last lowerBound + 1]

isWildcardRange :: Version -> Version -> Bool
isWildcardRange (Version branch1 _) (Version branch2 _) = check branch1 branch2
  where check (n:[]) (m:[]) | n+1 == m = True
        check (n:ns) (m:ms) | n   == m = check ns ms
        check _      _                 = False

------------------
-- Intervals view
--

-- | A complementary representation of a 'VersionRange'. Instead of a boolean
-- version predicate it uses an increasing sequence of non-overlapping
-- intervals.
--
-- The key point is that this representation gives a canonical representation
-- for the semantics of 'VersionRange's. This makes it easier to check things
-- like whether a version range is empty, covers all versions, or requires a
-- certain minimum or maximum version. It also makes it easy to check equality
-- or containment. It also makes it easier to identify \'simple\' version
-- predicates for translation into foreign packaging systems that do not
-- support complex version range expressions.
--
newtype VersionIntervals = VersionIntervals [VersionInterval]
  deriving (Eq, Show)

type VersionInterval = (LowerBound, UpperBound)
data LowerBound = NoLowerBound | LowerBound Version !Bound deriving (Eq, Show)
data UpperBound = NoUpperBound | UpperBound Version !Bound deriving (Eq, Show)
data Bound      = ExclusiveBound | InclusiveBound          deriving (Eq, Show)

instance Ord LowerBound where
  NoLowerBound   <= _            = True
  LowerBound _ _ <= NoLowerBound = False
  LowerBound ver bound <= LowerBound ver' bound' = case compare ver ver' of
    LT -> True
    EQ -> not (bound == ExclusiveBound && bound' == InclusiveBound)
    GT -> False

instance Ord UpperBound where
  _            <= NoUpperBound   = True
  NoUpperBound <= UpperBound _ _ = False
  UpperBound ver bound <= UpperBound ver' bound' = case compare ver ver' of
    LT -> True
    EQ -> not (bound == InclusiveBound && bound' == ExclusiveBound)
    GT -> False

-- | Test if a version falls within the version intervals.
--
-- It exists mostly for completeness. It satisfies the following properties:
--
-- > withinIntervals v (toVersionIntervals vr) = withinRange v vr
-- > withinIntervals v ivs = withinRange v (fromVersionIntervals ivs)
--
withinIntervals :: Version -> VersionIntervals -> Bool
withinIntervals v (VersionIntervals intervals) = any withinInterval intervals
  where
    withinInterval (lowerBound, upperBound) = withinLower lowerBound
                                           && withinUpper upperBound
    withinLower NoLowerBound                   = True
    withinLower (LowerBound v' ExclusiveBound) = v' <  v
    withinLower (LowerBound v' InclusiveBound) = v' <= v

    withinUpper NoUpperBound                   = True
    withinUpper (UpperBound v' ExclusiveBound) = v  >  v'
    withinUpper (UpperBound v' InclusiveBound) = v  >= v'

-- | View a 'VersionRange' as a sequence of version intervals.
--
toVersionIntervals :: VersionRange -> VersionIntervals
toVersionIntervals =
    VersionIntervals
  . foldVersionRange
              [(NoLowerBound,                NoUpperBound)]
    (\v    -> [(LowerBound v InclusiveBound, UpperBound v InclusiveBound)])
    (\v    -> [(LowerBound v ExclusiveBound, NoUpperBound)])
    (\v    -> [(NoLowerBound,                UpperBound v ExclusiveBound)])
    (\v v' -> [(LowerBound v InclusiveBound, UpperBound v' ExclusiveBound)])
    unionIntervals
    intersectIntervals
  where
    unionIntervals is []  = is
    unionIntervals [] is' = is'
    unionIntervals (i:is) (i':is') = case unionInterval i i' of
      Left  Nothing    -> i  : unionIntervals      is  (i' :is')
      Left  (Just i'') ->      unionIntervals      is  (i'':is')
      Right Nothing    -> i' : unionIntervals (i  :is)      is'
      Right (Just i'') ->      unionIntervals (i'':is)      is'

    intersectIntervals _  [] = []
    intersectIntervals [] _  = []
    intersectIntervals (i:is) (i':is') = case intersectInterval i i' of
      Left  Nothing    ->       intersectIntervals is (i':is')
      Left  (Just i'') -> i'' : intersectIntervals is (i':is')
      Right Nothing    ->       intersectIntervals (i:is) is'
      Right (Just i'') -> i'' : intersectIntervals (i:is) is'

-- | Convert a 'VersionIntervals' value back into a 'VersionRange' expression
-- representing the version intervals.
--
fromVersionIntervals :: VersionIntervals -> VersionRange
fromVersionIntervals (VersionIntervals []) = noVersion
fromVersionIntervals (VersionIntervals intervals) =
    foldr1 UnionVersionRanges [ interval l u | (l, u) <- intervals ]

  where
    interval (LowerBound v  InclusiveBound)
             (UpperBound v' InclusiveBound) | v == v'
                 = ThisVersion v
    interval (LowerBound v  InclusiveBound)
             (UpperBound v' ExclusiveBound) | isWildcardRange v v'
                 = WildcardVersion v
    interval l u = lowerBound l `intersectVersionRanges'` upperBound u

    lowerBound NoLowerBound                  = AnyVersion
    lowerBound (LowerBound v InclusiveBound) = orLaterVersion v
    lowerBound (LowerBound v ExclusiveBound) = LaterVersion v

    upperBound NoUpperBound                  = AnyVersion
    upperBound (UpperBound v InclusiveBound) = orEarlierVersion v
    upperBound (UpperBound v ExclusiveBound) = EarlierVersion v

    intersectVersionRanges' vr AnyVersion = vr
    intersectVersionRanges' AnyVersion vr = vr
    intersectVersionRanges' vr vr'        = IntersectVersionRanges vr vr'

unionInterval :: VersionInterval -> VersionInterval
              -> Either (Maybe VersionInterval) (Maybe VersionInterval)
unionInterval (lower , upper ) (lower', upper')

  -- Non-intersecting intervals with the left interval ending first
  | upper `doesNotTouch` lower' = Left Nothing

  -- Non-intersecting intervals with the right interval first
  | upper' `doesNotTouch` lower = Right Nothing

  -- Complete or partial overlap, with the left interval ending first
  | upper <= upper' = lowerBound `seq`
                      Left (Just (lowerBound, upper'))

  -- Complete or partial overlap, with the left interval ending first
  | otherwise = lowerBound `seq`
                Right (Just (lowerBound, upper))
  where
    lowerBound = min lower lower'

    -- Check an upper bound does not intersect, or even touch a lower bound:
    --
    --   ---|      or  ---)     but not  ---]     or  ---)     or  ---]
    --       |---         (---              (---         [---         [---
    --
    doesNotTouch :: UpperBound -> LowerBound -> Bool
    doesNotTouch NoUpperBound _ = False
    doesNotTouch _ NoLowerBound = False
    doesNotTouch (UpperBound u ub) (LowerBound l lb) =
          u <  l
      || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)

intersectInterval :: VersionInterval -> VersionInterval
                  -> Either (Maybe VersionInterval) (Maybe VersionInterval)
intersectInterval (lower , upper ) (lower', upper')

  -- Non-intersecting intervals with the left interval ending first
  | upper `doesNotIntersect` lower' = Left Nothing

  -- Non-intersecting intervals with the right interval first
  | upper' `doesNotIntersect` lower = Right Nothing

  -- Complete or partial overlap, with the left interval ending first
  | upper <= upper' = lowerBound `seq`
                      Left (Just (lowerBound, upper))

  -- Complete or partial overlap, with the right interval ending first
  | otherwise = lowerBound `seq`
                Right (Just (lowerBound, upper'))
  where
    lowerBound = max lower lower'

    -- Check an upper bound does not intersect a lower bound:
    --
    --   ---|      or  ---)     or  ---]     or  ---)     but not  ---]
    --       |---         (---         (---         [---              [---
    --
    doesNotIntersect :: UpperBound -> LowerBound -> Bool
    doesNotIntersect NoUpperBound _ = False
    doesNotIntersect _ NoLowerBound = False
    doesNotIntersect (UpperBound u ub) (LowerBound l lb) =
          u <  l
      || (u == l && not (ub == InclusiveBound && lb == InclusiveBound))

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
