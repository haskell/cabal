-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.Version
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
--                Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--

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

module Distribution.Compat.Version (

  simplifyVersionRange,

  -- * Version intervals view
  asVersionIntervals,
  VersionInterval,
  LowerBound(..),
  UpperBound(..),
  Bound(..),

  -- ** 'VersionIntervals' abstract type
  -- | The 'VersionIntervals' type and the accompanying functions are exposed
  -- primarily for completeness and testing purposes. In practice 
  -- 'asVersionIntervals' is the main function to use to
  -- view a 'VersionRange' as a bunch of 'VersionInterval's.
  --
  VersionIntervals,
  toVersionIntervals,
  fromVersionIntervals,
  withinIntervals,
  versionIntervals,
  mkVersionIntervals,
  unionVersionIntervals,
  intersectVersionIntervals,

 ) where

import Distribution.Version

import Control.Exception (assert)

-- | The empty version range, that is a version range containing no versions.
--
-- This can be constructed using any unsatisfiable version range expression,
-- for example @> 1 && < 1@.
--
-- > withinRange v anyVersion = False
--
noVersion :: VersionRange
noVersion = IntersectVersionRanges (LaterVersion v) (EarlierVersion v)
  where v = Version [1] []

-- | Fold over the syntactic structure of a 'VersionRange'.
--
-- This provides a syntacic view of the expression defining the version range.
-- For a semantic view use 'asVersionIntervals'.
--
foldVersionRange :: a -> (Version -> a) -> (Version -> a) -> (Version -> a)
                 -> (Version -> Version -> a)
                 -> (a -> a -> a)  -> (a -> a -> a)
                 -> VersionRange -> a
foldVersionRange anyv this later earlier _wildcard union intersect = fold
  where
    fold AnyVersion                     = anyv
    fold (ThisVersion v)                = this v
    fold (LaterVersion v)               = later v
    fold (EarlierVersion v)             = earlier v
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersect (fold v1) (fold v2)

-- | View a 'VersionRange' as a union of intervals.
--
-- This provides a canonical view of the semantics of a 'VersionRange' as
-- opposed to the syntax of the expression used to define it. For the syntactic
-- view use 'foldVersionRange'.
--
-- Each interval is non-empty. The sequence is in increasing order and no
-- intervals overlap or touch. Therefore only the first and last can be
-- unbounded. The sequence can be empty if the range is empty
-- (e.g. a range expression like @< 1 && > 2@).
--
-- Other checks are trivial to implement using this view. For example:
--
-- > isNoVersion vr | [] <- asVersionIntervals vr = True
-- >                | otherwise                   = False
--
-- > isSpecificVersion vr
-- >    | [(LowerBound v  InclusiveBound
-- >       ,UpperBound v' InclusiveBound)] <- asVersionIntervals vr
-- >    , v == v'   = Just v
-- >    | otherwise = Nothing
--
asVersionIntervals :: VersionRange -> [VersionInterval]
asVersionIntervals = versionIntervals . toVersionIntervals

-- | Simplify a 'VersionRange' expression into a canonical form.
--
-- It just uses @fromVersionIntervals . toVersionIntervals@
--
-- It satisfies the following properties:
--
-- > withinRange v (simplifyVersionRange r) = withinRange v r
--
-- >     withinRange v r = withinRange v r'
-- > ==> simplifyVersionRange r = simplifyVersionRange r'
--
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange = fromVersionIntervals . toVersionIntervals

------------------
-- Intervals view
--

-- | A complementary representation of a 'VersionRange'. Instead of a boolean
-- version predicate it uses an increasing sequence of non-overlapping,
-- non-empty intervals.
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

-- | Inspect the list of version intervals.
--
versionIntervals :: VersionIntervals -> [VersionInterval]
versionIntervals (VersionIntervals is) = is

type VersionInterval = (LowerBound, UpperBound)
data LowerBound =                LowerBound Version !Bound deriving (Eq, Show)
data UpperBound = NoUpperBound | UpperBound Version !Bound deriving (Eq, Show)
data Bound      = ExclusiveBound | InclusiveBound          deriving (Eq, Show)

minLowerBound :: LowerBound
minLowerBound = LowerBound (Version [0] []) InclusiveBound

isVersion0 :: Version -> Bool
isVersion0 (Version [0] _) = True
isVersion0 _               = False

instance Ord LowerBound where
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

invariant :: VersionIntervals -> Bool
invariant (VersionIntervals intervals) = all validInterval intervals
                                      && all doesNotTouch' adjacentIntervals
  where
    doesNotTouch' :: (VersionInterval, VersionInterval) -> Bool
    doesNotTouch' ((_,u), (l',_)) = doesNotTouch u l'

    adjacentIntervals :: [(VersionInterval, VersionInterval)]
    adjacentIntervals
      | null intervals = []
      | otherwise      = zip intervals (tail intervals)

checkInvariant :: VersionIntervals -> VersionIntervals
checkInvariant is = assert (invariant is) is

-- | Directly construct a 'VersionIntervals' from a list of intervals.
--
-- Each interval must be non-empty. The sequence must be in increasing order
-- and no invervals may overlap or touch. If any of these conditions are not
-- satisfied the function returns @Nothing@.
--
mkVersionIntervals :: [VersionInterval] -> Maybe VersionIntervals
mkVersionIntervals intervals
  | invariant (VersionIntervals intervals) = Just (VersionIntervals intervals)
  | otherwise                              = Nothing

validVersion :: Version -> Bool
validVersion (Version [] _) = False
validVersion (Version vs _) = all (>=0) vs

validInterval :: (LowerBound, UpperBound) -> Bool
validInterval i@(l, u) = validLower l && validUpper u && nonEmpty i
  where
    validLower (LowerBound v _) = validVersion v
    validUpper NoUpperBound     = True
    validUpper (UpperBound v _) = validVersion v

-- Check an interval is non-empty
--
nonEmpty :: VersionInterval -> Bool
nonEmpty (_,               NoUpperBound   ) = True
nonEmpty (LowerBound l lb, UpperBound u ub) =
  (l < u) || (l == u && lb == InclusiveBound && ub == InclusiveBound)

-- Check an upper bound does not intersect, or even touch a lower bound:
--
--   ---|      or  ---)     but not  ---]     or  ---)     or  ---]
--       |---         (---              (---         [---         [---
--
doesNotTouch :: UpperBound -> LowerBound -> Bool
doesNotTouch NoUpperBound _ = False
doesNotTouch (UpperBound u ub) (LowerBound l lb) =
      u <  l
  || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)

-- | Check an upper bound does not intersect a lower bound:
--
--   ---|      or  ---)     or  ---]     or  ---)     but not  ---]
--       |---         (---         (---         [---              [---
--
doesNotIntersect :: UpperBound -> LowerBound -> Bool
doesNotIntersect NoUpperBound _ = False
doesNotIntersect (UpperBound u ub) (LowerBound l lb) =
      u <  l
  || (u == l && not (ub == InclusiveBound && lb == InclusiveBound))

-- | Test if a version falls within the version intervals.
--
-- It exists mostly for completeness and testing. It satisfies the following
-- properties:
--
-- > withinIntervals v (toVersionIntervals vr) = withinRange v vr
-- > withinIntervals v ivs = withinRange v (fromVersionIntervals ivs)
--
withinIntervals :: Version -> VersionIntervals -> Bool
withinIntervals v (VersionIntervals intervals) = any withinInterval intervals
  where
    withinInterval (lowerBound, upperBound)    = withinLower lowerBound
                                              && withinUpper upperBound
    withinLower (LowerBound v' ExclusiveBound) = v' <  v
    withinLower (LowerBound v' InclusiveBound) = v' <= v

    withinUpper NoUpperBound                   = True
    withinUpper (UpperBound v' ExclusiveBound) = v' >  v
    withinUpper (UpperBound v' InclusiveBound) = v' >= v

-- | Convert a 'VersionRange' to a sequence of version intervals.
--
toVersionIntervals :: VersionRange -> VersionIntervals
toVersionIntervals = foldVersionRange
  (         chkIvl (minLowerBound,               NoUpperBound))
  (\v    -> chkIvl (LowerBound v InclusiveBound, UpperBound v InclusiveBound))
  (\v    -> chkIvl (LowerBound v ExclusiveBound, NoUpperBound))
  (\v    -> if isVersion0 v then VersionIntervals [] else
            chkIvl (minLowerBound,               UpperBound v ExclusiveBound))
  (\v v' -> chkIvl (LowerBound v InclusiveBound, UpperBound v' ExclusiveBound))
  unionVersionIntervals
  intersectVersionIntervals
  where
    chkIvl interval = checkInvariant (VersionIntervals [interval])

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
    interval l u = lowerBound l `intersectVersionRanges'` upperBound u

    lowerBound (LowerBound v InclusiveBound)
                              | isVersion0 v = AnyVersion
                              | otherwise    = orLaterVersion v
    lowerBound (LowerBound v ExclusiveBound) = LaterVersion v

    upperBound NoUpperBound                  = AnyVersion
    upperBound (UpperBound v InclusiveBound) = orEarlierVersion v
    upperBound (UpperBound v ExclusiveBound) = EarlierVersion v

    intersectVersionRanges' vr AnyVersion = vr
    intersectVersionRanges' AnyVersion vr = vr
    intersectVersionRanges' vr vr'        = IntersectVersionRanges vr vr'

unionVersionIntervals :: VersionIntervals -> VersionIntervals
                      -> VersionIntervals
unionVersionIntervals (VersionIntervals is0) (VersionIntervals is'0) =
  checkInvariant (VersionIntervals (union is0 is'0))
  where
    union is []  = is
    union [] is' = is'
    union (i:is) (i':is') = case unionInterval i i' of
      Left  Nothing    -> i  : union      is  (i' :is')
      Left  (Just i'') ->      union      is  (i'':is')
      Right Nothing    -> i' : union (i  :is)      is'
      Right (Just i'') ->      union (i'':is)      is'

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

intersectVersionIntervals :: VersionIntervals -> VersionIntervals
                          -> VersionIntervals
intersectVersionIntervals (VersionIntervals is0) (VersionIntervals is'0) =
  checkInvariant (VersionIntervals (intersect is0 is'0))
  where
    intersect _  [] = []
    intersect [] _  = []
    intersect (i:is) (i':is') = case intersectInterval i i' of
      Left  Nothing    ->       intersect is (i':is')
      Left  (Just i'') -> i'' : intersect is (i':is')
      Right Nothing    ->       intersect (i:is) is'
      Right (Just i'') -> i'' : intersect (i:is) is'

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

