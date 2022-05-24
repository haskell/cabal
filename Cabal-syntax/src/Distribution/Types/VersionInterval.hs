{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module implements a view of a 'VersionRange' as a finite
-- list of separated version intervals.
--
-- In conversion from and to 'VersionRange' it makes some effort to
-- preserve the caret operator @^>=x.y@.  This constraint a priori
-- specifies the same interval as @==x.y.*@, but indicates that newer
-- versions could be acceptable (@allow-newer: ^@).
--
module Distribution.Types.VersionInterval (
    -- * Version intervals
    VersionIntervals,
    unVersionIntervals,

    -- * Conversions
    toVersionIntervals,
    fromVersionIntervals,

    -- ** Normalisation
    normaliseVersionRange2,

    -- * Relaxation
    relaxLastInterval,
    relaxHeadInterval,

    -- * Version intervals view
    asVersionIntervals,
    VersionInterval (..),
    LowerBound(..),
    UpperBound(..),
    Bound(..),

    -- * Invariants
    invariantVersionIntervals,
    ) where

import Control.Applicative         (liftA2)
import Control.Exception           (assert)
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal

-- To test this module, and to run version range normalisation benchmarks:
--
-- cabal run Cabal-tests:unit-tests -- -p Distribution.Version
-- cabal run cabal-benchmarks -- -o bench.html normaliseVersionRange

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

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
  deriving (Eq, Show, Typeable)

-- | Inspect the list of version intervals.
--
unVersionIntervals :: VersionIntervals -> [VersionInterval]
unVersionIntervals (VersionIntervals is) = is

data VersionInterval = VersionInterval !LowerBound !UpperBound    deriving (Eq, Show)
data LowerBound      = LowerBound !Version !Bound                 deriving (Eq, Show)
data UpperBound      = NoUpperBound | UpperBound !Version !Bound  deriving (Eq, Show)
data Bound           = ExclusiveBound | InclusiveBound            deriving (Eq, Show)

zeroLowerBound :: LowerBound
zeroLowerBound = LowerBound version0 InclusiveBound

isVersion0 :: Version -> Bool
isVersion0 = (==) version0

-------------------------------------------------------------------------------
-- Stage1
-------------------------------------------------------------------------------

stage1 :: VersionRange -> [VersionInterval]
stage1 = cataVersionRange alg where
    -- version range leafs transform into singleton intervals
    alg (ThisVersionF v)                = [VersionInterval (LowerBound v InclusiveBound) (UpperBound v InclusiveBound)]
    alg (LaterVersionF v)               = [VersionInterval (LowerBound v ExclusiveBound) NoUpperBound]
    alg (OrLaterVersionF v)             = [VersionInterval (LowerBound v InclusiveBound) NoUpperBound]
    alg (EarlierVersionF v)
        | isVersion0 v                  = []
        | otherwise                     = [VersionInterval zeroLowerBound                (UpperBound v ExclusiveBound)]
    alg (OrEarlierVersionF v)           = [VersionInterval zeroLowerBound                (UpperBound v InclusiveBound)]

    -- ^>= version-range's upper bound should be MajorBound
    alg (MajorBoundVersionF v)          = [VersionInterval (LowerBound v InclusiveBound) (UpperBound (majorUpperBound v) ExclusiveBound)]

    -- union: just merge the version intervals
    alg (UnionVersionRangesF v1 v2)     = v1 ++ v2

    -- intersection: pairwise intersect. Strip empty intervals. Sort to restore the invariant.
    alg (IntersectVersionRangesF v1 v2) = mapMaybe nonEmptyInterval $ liftA2 intersectInterval (stage2and3 v1) (stage2and3 v2)

-- | Check that interval is non-empty
nonEmptyInterval :: VersionInterval -> Maybe VersionInterval
nonEmptyInterval i | nonEmptyVI i = Just i
nonEmptyInterval _                = Nothing

-------------------------------------------------------------------------------
-- Stage2
-------------------------------------------------------------------------------

stage2 :: [VersionInterval] -> [VersionInterval]
stage2 = sortBy lowerboundCmp

lowerboundCmp :: VersionInterval -> VersionInterval -> Ordering
lowerboundCmp (VersionInterval (LowerBound v vb) _) (VersionInterval (LowerBound u ub) _) =
    compare v u `mappend` compareBound vb ub
  where
    compareBound :: Bound -> Bound -> Ordering
    compareBound InclusiveBound InclusiveBound = EQ
    compareBound InclusiveBound ExclusiveBound = LT
    compareBound ExclusiveBound InclusiveBound = GT
    compareBound ExclusiveBound ExclusiveBound = EQ

-------------------------------------------------------------------------------
-- Postprocess
-------------------------------------------------------------------------------

-- | Post-processing takes a list of ordered version intervals,
-- but possibly overlapping, and creates 'VersionIntervals'.
--
postprocess :: [VersionInterval] -> VersionIntervals
postprocess = checkInvariant . VersionIntervals . stage2and3

stage2and3 :: [VersionInterval] -> [VersionInterval]
stage2and3 = stage3 . stage2

stage3 :: [VersionInterval] -> [VersionInterval]
stage3 []                                     = []
stage3 (VersionInterval lb ub : rest)         = stage3go lb ub rest

stage3go :: LowerBound -> UpperBound -> [VersionInterval] -> [VersionInterval]
stage3go !lb NoUpperBound _                                 = [VersionInterval lb NoUpperBound]
stage3go !lb !ub          []                                = [VersionInterval lb ub]
stage3go !lb !ub          (VersionInterval lb' ub' : rest')
    | doesNotTouch ub lb'                                   = VersionInterval lb ub : stage3go lb' ub' rest'
    | otherwise                                             = stage3go lb (unionUpper ub ub') rest'

-------------------------------------------------------------------------------
-- Intersections
-------------------------------------------------------------------------------

intersectInterval :: VersionInterval -> VersionInterval -> VersionInterval
intersectInterval (VersionInterval lv uv) (VersionInterval lu uu) =
    VersionInterval (intersectLower lv lu) (intersectUpper uv uu)

intersectLower :: LowerBound -> LowerBound -> LowerBound
intersectLower (LowerBound v vb) (LowerBound u ub) = case compare v u of
    EQ -> LowerBound v (intersectBound vb ub)
    LT -> LowerBound u ub
    GT -> LowerBound v vb

intersectUpper :: UpperBound -> UpperBound -> UpperBound
intersectUpper NoUpperBound      b                 = b
intersectUpper b                 NoUpperBound      = b
intersectUpper (UpperBound v vb) (UpperBound u ub) = case compare v u of
    EQ -> UpperBound v (intersectBound vb ub)
    LT -> UpperBound v vb
    GT -> UpperBound u ub

intersectBound :: Bound -> Bound -> Bound
intersectBound InclusiveBound InclusiveBound = InclusiveBound
intersectBound _              _              = ExclusiveBound

-------------------------------------------------------------------------------
-- Unions
-------------------------------------------------------------------------------

unionUpper :: UpperBound -> UpperBound -> UpperBound
unionUpper NoUpperBound      _                 = NoUpperBound
unionUpper _                 NoUpperBound      = NoUpperBound
unionUpper (UpperBound v vb) (UpperBound u ub) = case compare v u of
    EQ -> UpperBound v (unionBound vb ub)
    LT -> UpperBound u ub
    GT -> UpperBound v vb

unionBound :: Bound -> Bound -> Bound
unionBound ExclusiveBound ExclusiveBound = ExclusiveBound
unionBound _              _              = InclusiveBound

-------------------------------------------------------------------------------
-- VersionRange
-------------------------------------------------------------------------------

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
asVersionIntervals = unVersionIntervals . toVersionIntervals

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Check an upper bound does not intersect, or even touch a lower bound:
--
-- @
--   ---|      or  ---)     but not  ---]     or  ---)     or  ---]
--       |---         (---              (---         [---         [---
-- @
--
doesNotTouch :: UpperBound -> LowerBound -> Bool
doesNotTouch NoUpperBound      _                 = False
doesNotTouch (UpperBound u ub) (LowerBound l lb) =
    (u < l) || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

-- | 'VersionIntervals' invariant:
--
-- * all intervals are valid (lower bound is less then upper bound, i.e. non-empty)
-- * intervals doesn't touch each other (distinct)
--
invariantVersionIntervals :: VersionIntervals -> Bool
invariantVersionIntervals (VersionIntervals intervals) =
    all validInterval intervals &&
    all doesNotTouch' adjacentIntervals
  where
    doesNotTouch' :: (VersionInterval, VersionInterval) -> Bool
    doesNotTouch' (VersionInterval _ u, VersionInterval l' _) = doesNotTouch u l'

    adjacentIntervals :: [(VersionInterval, VersionInterval)]
    adjacentIntervals = case intervals of
      []     -> []
      (_:tl) -> zip intervals tl

checkInvariant :: VersionIntervals -> VersionIntervals
checkInvariant is = assert (invariantVersionIntervals is) is
{-# INLINE checkInvariant #-}

validInterval :: VersionInterval -> Bool
validInterval i@(VersionInterval l u) = validLower l && validUpper u && nonEmptyVI i
  where
    validLower (LowerBound v _) = validVersion v
    validUpper NoUpperBound      = True
    validUpper (UpperBound v _)  = validVersion v

-- Check an interval is non-empty
--
nonEmptyVI :: VersionInterval -> Bool
nonEmptyVI (VersionInterval _                 NoUpperBound)      = True
nonEmptyVI (VersionInterval (LowerBound l lb) (UpperBound u ub)) =
  (l < u) || (l == u && lb == InclusiveBound && ub == InclusiveBound)

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Convert a 'VersionRange' to a sequence of version intervals.
--
toVersionIntervals :: VersionRange -> VersionIntervals
toVersionIntervals = postprocess . stage1

-- | Convert a 'VersionIntervals' value back into a 'VersionRange' expression
-- representing the version intervals.
--
fromVersionIntervals :: VersionIntervals -> VersionRange
fromVersionIntervals (VersionIntervals [])     = noVersion
fromVersionIntervals (VersionIntervals (x:xs)) = foldr1 unionVersionRanges (fmap intervalToVersionRange (x:|xs))

intervalToVersionRange :: VersionInterval -> VersionRange
intervalToVersionRange (VersionInterval (LowerBound v vb) upper') = case upper' of
    NoUpperBound
        -> lowerBound

    UpperBound u ub
        | vb == InclusiveBound
        , ub == InclusiveBound
        , v == u
        -> thisVersion v

    UpperBound u ub -> withLowerBound (makeUpperBound u ub)
  where
    lowerBound :: VersionRange
    lowerBound = case vb of
        InclusiveBound -> orLaterVersion v
        ExclusiveBound -> laterVersion v

    withLowerBound :: VersionRange -> VersionRange
    withLowerBound vr
        | isVersion0 v, vb == InclusiveBound = vr
        | otherwise                          = intersectVersionRanges lowerBound vr

    makeUpperBound :: Version -> Bound -> VersionRange
    makeUpperBound u InclusiveBound = orEarlierVersion u
    makeUpperBound u ExclusiveBound = earlierVersion u

-------------------------------------------------------------------------------
-- Normalisation
-------------------------------------------------------------------------------

-- | Since @Cabal-3.6@ this function.. TODO
--
normaliseVersionRange2 :: VersionRange -> VersionRange
normaliseVersionRange2 = fromVersionIntervals . toVersionIntervals

-------------------------------------------------------------------------------
-- Relaxation
-------------------------------------------------------------------------------

relaxLastInterval :: VersionIntervals -> VersionIntervals
relaxLastInterval (VersionIntervals xs) = VersionIntervals (relaxLastInterval' xs)
  where
    relaxLastInterval' []                    = []
    relaxLastInterval' [VersionInterval l _] = [VersionInterval l NoUpperBound]
    relaxLastInterval' (i:is)                = i : relaxLastInterval' is

relaxHeadInterval :: VersionIntervals -> VersionIntervals
relaxHeadInterval (VersionIntervals xs) = VersionIntervals (relaxHeadInterval' xs)
  where
    relaxHeadInterval' []                         = []
    relaxHeadInterval' (VersionInterval _ u : is) = VersionInterval zeroLowerBound u : is
