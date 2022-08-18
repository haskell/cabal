{-# LANGUAGE DeriveDataTypeable #-}

-- | This module implements a view of a 'VersionRange' as a finite
-- list of separated version intervals and provides the Boolean
-- algebra operations union, intersection, and complement.
--
-- It interprets the caret operator @^>=x.y@ as simply @==x.y.*@.
-- Until @Cabal < 3.6@, this module was called "Distribution.Types.VersionInterval".
-- The current module "Distribution.Types.VersionInterval" (refurbished since
-- @Cabal >= 3.6@) makes some effort to preserve the caret operator,
-- but so far does not expose the Boolean algebra structure.
--
module Distribution.Types.VersionInterval.Legacy (
    -- * Version intervals
    VersionIntervals,
    toVersionIntervals,
    fromVersionIntervals,
    withinIntervals,
    versionIntervals,
    mkVersionIntervals,
    unionVersionIntervals,
    intersectVersionIntervals,
    invertVersionIntervals,
    relaxLastInterval,
    relaxHeadInterval,

    -- * Version intervals view
    asVersionIntervals,
    VersionInterval,
    LowerBound(..),
    UpperBound(..),
    Bound(..),
    ) where

import Prelude ()
import Distribution.Compat.Prelude
import Control.Exception (assert)

import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal

-- NonEmpty
import qualified Prelude (foldr1)

-------------------------------------------------------------------------------
-- VersionRange
-------------------------------------------------------------------------------

-- | View a 'VersionRange' as a sequence of separated intervals.
--
-- This provides a canonical view of the semantics of a 'VersionRange' as
-- opposed to the syntax of the expression used to define it. For the syntactic
-- view use 'foldVersionRange'.
--
-- /Canonical/ means that two semantically equal ranges translate to the /same/
-- @['VersionInterval']@, thus its 'Eq' instance can decide semantical equality
-- of ranges.
--
-- In the returned sequence, each interval is non-empty.
-- The sequence is in increasing order and the intervals are separated, i.e., they
-- neither overlap nor touch. Therefore only the first and last interval can be
-- unbounded. The sequence can be empty if the range is empty
-- (e.g. a range expression like @> 2 && < 1@).
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


-------------------------------------------------------------------------------
-- VersionInterval
-------------------------------------------------------------------------------

-- | A complementary representation of a 'VersionRange',
-- using an increasing sequence of separated (i.e., non-overlapping, non-touching)
-- non-empty intervals.
-- The represented range is the union of these intervals, meaning
-- that the empty sequence denotes the empty range.
--
-- As ranges form a Boolean algebra, we can compute union,
-- intersection, and complement.  These operations are all linear in
-- the size of the input, thanks to the ordered representation.
--
-- The interval-sequence representation gives a canonical representation
-- for the semantics of 'VersionRange's. This makes it easier to check things
-- like whether a version range is empty, covers all versions, or requires a
-- certain minimum or maximum version. It also makes it easy to check equality (just '==')
-- or containment. It also makes it easier to identify \'simple\' version
-- predicates for translation into foreign packaging systems that do not
-- support complex version range expressions.
--
newtype VersionIntervals = VersionIntervals [VersionInterval]
  deriving (Eq, Show, Typeable)

-- | Inspect the list of version intervals.
--
versionIntervals :: VersionIntervals -> [VersionInterval]
versionIntervals (VersionIntervals is) = is

-- | Version intervals with exclusive or inclusive bounds, in all combinations:
--
-- 1. \( (lb,ub) \) meaning \( lb < \_ < ub \).
-- 2. \( (lb,ub] \) meaning \( lb < \_ ≤ ub \).
-- 3. \( [lb,ub) \) meaning \( lb ≤ \_ < ub \).
-- 4. \( [lb,ub] \) meaning \( lb ≤ \_ < ub \).
--
-- The upper bound can also be missing, meaning "\( ..,∞) \)".
--
type VersionInterval = (LowerBound, UpperBound)

data LowerBound
  = LowerBound Version !Bound  -- ^ Either exclusive @(v,..@ or inclusive @[v,..@.
  deriving (Eq, Show)

data UpperBound
  = NoUpperBound               -- ^ @..,∞)@
  | UpperBound Version !Bound  -- ^ Either exclusive @..,v)@ or inclusive @..,v]@.
  deriving (Eq, Show)

data Bound
  = ExclusiveBound   -- ^ @(v,..@ if used as lower bound, @..,v)@ if used as upper bound.
  | InclusiveBound   -- ^ @[v,..@ if used as lower bound, @..,v]@ if used as upper bound.
  deriving (Eq, Show)

-- | @[0,..@.
minLowerBound :: LowerBound
minLowerBound = LowerBound (mkVersion [0]) InclusiveBound

isVersion0 :: Version -> Bool
isVersion0 = (==) version0

-- | @lb1 <= lb2@ holds iff interval @lb1..@ is contained in interval @lb2..@.
--
instance Ord LowerBound where
  LowerBound ver bound <= LowerBound ver' bound' = case compare ver ver' of
    LT -> True
    EQ -> not (bound == ExclusiveBound && bound' == InclusiveBound)
    GT -> False

-- | @ub1 <= ub2@ holds iff interval @0..ub1@ is contained in interval @0..ub2@.
--
instance Ord UpperBound where
  _            <= NoUpperBound   = True
  NoUpperBound <= UpperBound _ _ = False
  UpperBound ver bound <= UpperBound ver' bound' = case compare ver ver' of
    LT -> True
    EQ -> not (bound == InclusiveBound && bound' == ExclusiveBound)
    GT -> False

-- | Check that the sequence is ordered,
-- adjacent intervals are separated (do not overlap),
-- an no interval is empty (which would be a redundant entry).
--
invariant :: VersionIntervals -> Bool
invariant (VersionIntervals intervals) = all validInterval intervals
                                      && all doesNotTouch' adjacentIntervals
  where
    doesNotTouch' :: (VersionInterval, VersionInterval) -> Bool
    doesNotTouch' ((_,u), (l',_)) = doesNotTouch u l'

    -- adjacentIntervals = zip intervals (tail intervals)
    adjacentIntervals :: [(VersionInterval, VersionInterval)]
    adjacentIntervals = case intervals of
      []     -> []
      (_:tl) -> zip intervals tl

-- | The partial identity function, erroring out on illformed 'VersionIntervals'.
--
checkInvariant :: VersionIntervals -> VersionIntervals
checkInvariant is = assert (invariant is) is

-- | Directly construct a 'VersionIntervals' from a list of intervals.
--
mkVersionIntervals :: [VersionInterval] -> VersionIntervals
mkVersionIntervals intervals
    | invariant (VersionIntervals intervals) = VersionIntervals intervals
    | otherwise
        = checkInvariant
        . foldl' (flip insertInterval) (VersionIntervals [])
        . filter validInterval
        $ intervals

-- | Add an interval to the sequence, fusing with existing intervals if necessary.
--
insertInterval :: VersionInterval -> VersionIntervals -> VersionIntervals
insertInterval i is = unionVersionIntervals (VersionIntervals [i]) is

-- | A valid interval is non-empty.
--
validInterval :: (LowerBound, UpperBound) -> Bool
validInterval i@(l, u) = validLower l && validUpper u && nonEmptyVI i
  where
    validLower (LowerBound v _) = validVersion v
    validUpper NoUpperBound     = True
    validUpper (UpperBound v _) = validVersion v

-- | Check that an interval is non-empty.
--
nonEmptyVI :: VersionInterval -> Bool
nonEmptyVI (_,               NoUpperBound   ) = True
nonEmptyVI (LowerBound l lb, UpperBound u ub) =
  (l < u) || (l == u && lb == InclusiveBound && ub == InclusiveBound)

-- | Check an upper bound does not intersect, or even touch a lower bound:
--
-- @
--
--   ---|      or  ---)     but not  ---]     or  ---)     or  ---]
--       |---         (---              (---         [---         [---
--
-- @
doesNotTouch :: UpperBound -> LowerBound -> Bool
doesNotTouch NoUpperBound _ = False
doesNotTouch (UpperBound u ub) (LowerBound l lb) =
      u <  l
  || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)

-- | Check an upper bound does not intersect a lower bound:
--
-- @
--
--   ---|      or  ---)     or  ---]     or  ---)     but not  ---]
--       |---         (---         (---         [---              [---
--
-- @
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
toVersionIntervals = cataVersionRange alg where
    -- @== v@
    alg (ThisVersionF v)                = chkIvl (LowerBound v InclusiveBound, UpperBound v InclusiveBound)
    -- @>  v@
    alg (LaterVersionF v)               = chkIvl (LowerBound v ExclusiveBound, NoUpperBound)
    -- @>= v@
    alg (OrLaterVersionF v)             = chkIvl (LowerBound v InclusiveBound, NoUpperBound)
    -- @<  v@
    alg (EarlierVersionF v)
        | isVersion0 v                  = VersionIntervals []
        | otherwise                     = chkIvl (minLowerBound,               UpperBound v ExclusiveBound)
    -- @<= v@
    alg (OrEarlierVersionF v)           = chkIvl (minLowerBound,               UpperBound v InclusiveBound)
    -- @^>= v@
    alg (MajorBoundVersionF v)          = chkIvl (LowerBound v InclusiveBound, UpperBound (majorUpperBound v) ExclusiveBound)
    -- @r || r'@
    alg (UnionVersionRangesF v1 v2)     = unionVersionIntervals v1 v2
    -- @r && r'@
    alg (IntersectVersionRangesF v1 v2) = intersectVersionIntervals v1 v2

    chkIvl interval = checkInvariant (VersionIntervals [interval])

-- | Convert a 'VersionIntervals' value back into a 'VersionRange' expression
-- representing the version intervals.
--
fromVersionIntervals :: VersionIntervals -> VersionRange
fromVersionIntervals (VersionIntervals []) = noVersion
fromVersionIntervals (VersionIntervals intervals) =
    Prelude.foldr1 unionVersionRanges [ interval l u | (l, u) <- intervals ]

  where
    interval (LowerBound v  InclusiveBound)
             (UpperBound v' InclusiveBound) | v == v'
                 = thisVersion v
    interval l u = lowerBound l `intersectVersionRanges'` upperBound u

    lowerBound (LowerBound v InclusiveBound)
                              | isVersion0 v = Nothing
                              | otherwise    = Just (orLaterVersion v)
    lowerBound (LowerBound v ExclusiveBound) = Just (laterVersion v)

    upperBound NoUpperBound                  = Nothing
    upperBound (UpperBound v InclusiveBound) = Just (orEarlierVersion v)
    upperBound (UpperBound v ExclusiveBound) = Just (earlierVersion v)

    intersectVersionRanges' Nothing Nothing      = anyVersion
    intersectVersionRanges' (Just vr) Nothing    = vr
    intersectVersionRanges' Nothing (Just vr)    = vr
    intersectVersionRanges' (Just vr) (Just vr') = intersectVersionRanges vr vr'

-- | Union two interval sequences, fusing intervals where necessary.
-- Computed \( O(n+m) \) time, resulting in sequence of length \( ≤ n+m \).
--
unionVersionIntervals :: VersionIntervals -> VersionIntervals
                      -> VersionIntervals
unionVersionIntervals (VersionIntervals is0) (VersionIntervals is'0) =
  checkInvariant (VersionIntervals (union is0 is'0))
  where
    union is []  = is
    union [] is' = is'
    union (i:is) (i':is') = case unionInterval i i' of

      -- @i < i'@ and separated: keep @i@.
      Left  Nothing    -> i  : union      is  (i' :is')

      -- @i'' = i ∪ i'@ and @i@ ends first: drop @i@, replace @i'@ by @i''@.
      Left  (Just i'') ->      union      is  (i'':is')

      -- @i' < i@ and separated: keep @i'@.
      Right Nothing    -> i' : union (i  :is)      is'

      -- @i'' = i ∪ i'@ and @i'@ ends first: drop @i'@, replace @i@ by @i''@.
      Right (Just i'') ->      union (i'':is)      is'

-- | Given two version intervals @i1@ and @i2@, return one of the following:
--
-- [@Left Nothing@]     when @i1 < i2@ and the intervals are separated.
-- [@Right Nothing@]    when @i2 < i1@ and the intervals are separated.
-- [@Left (i1 \/ i2)@]  when @ub(i1) <= ub(i2)@ and the intervals are not separated.
-- [@Right (i1 \/ i2)@] when @ub(i2) < ub(i1)@ and the intervals are not separated.
--
-- Herein, @i < i'@ means that the whole of the interval @i@ is strictly left of the whole of @i'@,
-- and @ub(i)@ returns the right boundary of interval @i@ which could be inclusive or exclusive.
--
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

-- | The intersection \( is \cap is' \) of two interval sequences \( is \) and \( is' \)
-- of lengths \( n \) and \( m \), resp.,
-- satisfies the specification \( is ∩ is' = \{ i ∩ i' \mid i ∈ is, i' ∈ is' \} \).
-- Thanks to the ordered representation of intervals it can be computed in \( O(n+m) \)
-- (rather than the naive \( O(nm) \).
--
-- The length of \( is \cap is' \) is \( ≤ \min(n,m) \).
--
intersectVersionIntervals :: VersionIntervals -> VersionIntervals
                          -> VersionIntervals
intersectVersionIntervals (VersionIntervals is0) (VersionIntervals is'0) =
  checkInvariant (VersionIntervals (intersect is0 is'0))
  where
    intersect _  [] = []
    intersect [] _  = []
    intersect (i:is) (i':is') = case intersectInterval i i' of

      -- @i < i'@: throw out @i@
      Left  Nothing    ->       intersect is (i':is')

      -- @i'' = i /\ i'@ and @i@ ends first: replace @i@ by @i''@.
      Left  (Just i'') -> i'' : intersect is (i':is')

      -- @i' < i@: throw out @i'@
      Right Nothing    ->       intersect (i:is) is'

      -- @i'' = i /\ i'@ and @i'@ ends first: replace @i'@ by @i''@.
      Right (Just i'') -> i'' : intersect (i:is) is'

-- | Given two version intervals @i1@ and @i2@, return one of the following:
--
-- [@Left Nothing@]     when @i1 < i2@.
-- [@Right Nothing@]    when @i2 < i1@.
-- [@Left (i1 /\ i2)@]  when @ub(i1) <= ub(i2)@.
-- [@Right (i1 /\ i2)@] when @ub(i2) < ub(i1)@.
--
-- Herein, @i < i'@ means that the whole of the interval @i@ is strictly left of the whole of @i'@,
-- and @ub(i)@ returns the right boundary of interval @i@ which could be inclusive or exclusive.
--
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

-- | Compute the complement.
-- \( O(n) \).
invertVersionIntervals :: VersionIntervals
                       -> VersionIntervals
invertVersionIntervals (VersionIntervals xs) =
    case xs of
      -- Empty interval set
      [] -> VersionIntervals [(noLowerBound, NoUpperBound)]
      -- Interval with no lower bound
      ((lb, ub) : more) | lb == noLowerBound ->
        VersionIntervals $ invertVersionIntervals' ub more
      -- Interval with a lower bound
      ((lb, ub) : more) ->
          VersionIntervals $ (noLowerBound, invertLowerBound lb)
          : invertVersionIntervals' ub more
    where
      -- Invert subsequent version intervals given the upper bound of
      -- the intervals already inverted.
      invertVersionIntervals' :: UpperBound
                              -> [(LowerBound, UpperBound)]
                              -> [(LowerBound, UpperBound)]
      invertVersionIntervals' NoUpperBound [] = []
      invertVersionIntervals' ub0 [] = [(invertUpperBound ub0, NoUpperBound)]
      invertVersionIntervals' ub0 [(lb, NoUpperBound)] =
          [(invertUpperBound ub0, invertLowerBound lb)]
      invertVersionIntervals' ub0 ((lb, ub1) : more) =
          (invertUpperBound ub0, invertLowerBound lb)
            : invertVersionIntervals' ub1 more

      invertLowerBound :: LowerBound -> UpperBound
      invertLowerBound (LowerBound v b) = UpperBound v (invertBound b)

      invertUpperBound :: UpperBound -> LowerBound
      invertUpperBound (UpperBound v b) = LowerBound v (invertBound b)
      invertUpperBound NoUpperBound = error "NoUpperBound: unexpected"

      invertBound :: Bound -> Bound
      invertBound ExclusiveBound = InclusiveBound
      invertBound InclusiveBound = ExclusiveBound

      noLowerBound :: LowerBound
      noLowerBound = LowerBound (mkVersion [0]) InclusiveBound

-- | Remove the last upper bound, enlarging the range.
-- But empty ranges stay empty.
-- \( O(n) \).
relaxLastInterval :: VersionIntervals -> VersionIntervals
relaxLastInterval (VersionIntervals xs) = VersionIntervals (relaxLastInterval' xs)
  where
    relaxLastInterval' []      = []
    relaxLastInterval' [(l,_)] = [(l, NoUpperBound)]
    relaxLastInterval' (i:is)  = i : relaxLastInterval' is

-- | Remove the first lower bound (i.e, make it \( [0 \).
-- Empty ranges stay empty.
-- \( O(1) \).
relaxHeadInterval :: VersionIntervals -> VersionIntervals
relaxHeadInterval (VersionIntervals xs) = VersionIntervals (relaxHeadInterval' xs)
  where
    relaxHeadInterval' []         = []
    relaxHeadInterval' ((_,u):is) = (minLowerBound,u) : is
