{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Types.VersionRange
  ( -- * Version Range
    VersionRange

    -- ** Predicates
    -- $predicate-examples

    -- *** Lower Bound
  , hasLowerBound
  , hasGTLowerBound

    -- *** Upper Bound
  , hasUpperBound
  , hasLEUpperBound
  , hasTrailingZeroUpperBound

    -- *** Any Version
  , isAnyVersion
  , isAnyVersionLight

    -- ** Constructing
  , anyVersion
  , noVersion
  , thisVersion
  , notThisVersion
  , laterVersion
  , earlierVersion
  , orLaterVersion
  , orEarlierVersion
  , unionVersionRanges
  , intersectVersionRanges
  , withinVersion
  , majorBoundVersion

    -- ** Modification
  , normaliseVersionRange
  , stripParensVersionRange

    -- ** Inspection
  , withinRange
  , foldVersionRange

  --   -- ** Parser
  -- , versionRangeParser

    -- * Version F-Algebra
  , VersionRangeF (..)
  , projectVersionRange
  , embedVersionRange
  , cataVersionRange
  , anaVersionRange
  , hyloVersionRange

    -- * Version Utilities

  -- See "Distribution.Version" for more utilities.
  , wildcardUpperBound
  , majorUpperBound
  , isWildcardRange
  ) where

import Distribution.Compat.Prelude
import Distribution.Types.Version
import Distribution.Types.VersionInterval
import Distribution.Types.VersionRange.Internal
import Prelude ()

-- | Fold over the basic syntactic structure of a 'VersionRange'.
--
-- This provides a syntactic view of the expression defining the version range.
-- The syntactic sugar @\">= v\"@, @\"<= v\"@ and @\"== v.*\"@ is presented
-- in terms of the other basic syntax.
--
-- For a semantic view use 'asVersionIntervals'.
foldVersionRange
  :: a
  -- ^ @\"-any\"@ version
  -> (Version -> a)
  -- ^ @\"== v\"@
  -> (Version -> a)
  -- ^ @\"> v\"@
  -> (Version -> a)
  -- ^ @\"< v\"@
  -> (a -> a -> a)
  -- ^ @\"_ || _\"@ union
  -> (a -> a -> a)
  -- ^ @\"_ && _\"@ intersection
  -> VersionRange
  -> a
foldVersionRange _any this later earlier union intersect = fold
  where
    fold = cataVersionRange alg

    alg (ThisVersionF v) = this v
    alg (LaterVersionF v) = later v
    alg (OrLaterVersionF v) = this v `union` later v
    alg (EarlierVersionF v) = earlier v
    alg (OrEarlierVersionF v) = this v `union` earlier v
    alg (MajorBoundVersionF v) = fold (majorBound v)
    alg (UnionVersionRangesF v1 v2) = v1 `union` v2
    alg (IntersectVersionRangesF v1 v2) = v1 `intersect` v2

    majorBound v =
      intersectVersionRanges
        (orLaterVersion v)
        (earlierVersion (majorUpperBound v))

-- | Normalise 'VersionRange'.
--
-- In particular collapse @(== v || > v)@ into @>= v@, and so on.
normaliseVersionRange :: VersionRange -> VersionRange
normaliseVersionRange = hyloVersionRange embed projectVersionRange
  where
    -- == v || > v, > v || == v  ==>  >= v
    embed (UnionVersionRangesF (ThisVersion v) (LaterVersion v'))
      | v == v' =
          orLaterVersion v
    embed (UnionVersionRangesF (LaterVersion v) (ThisVersion v'))
      | v == v' =
          orLaterVersion v
    -- == v || < v, < v || == v  ==>  <= v
    embed (UnionVersionRangesF (ThisVersion v) (EarlierVersion v'))
      | v == v' =
          orEarlierVersion v
    embed (UnionVersionRangesF (EarlierVersion v) (ThisVersion v'))
      | v == v' =
          orEarlierVersion v
    -- otherwise embed normally
    embed vr = embedVersionRange vr

-- |  Remove 'VersionRangeParens' constructors.
--
-- Since version 3.4 this function is 'id', there aren't 'VersionRangeParens' constructor in 'VersionRange' anymore.
--
-- @since 2.2
stripParensVersionRange :: VersionRange -> VersionRange
stripParensVersionRange = id

-- | Does this version fall within the given range?
--
-- This is the evaluation function for the 'VersionRange' type.
withinRange :: Version -> VersionRange -> Bool
withinRange v =
  foldVersionRange
    True
    (\v' -> v == v')
    (\v' -> v > v')
    (\v' -> v < v')
    (||)
    (&&)

-- | Does this 'VersionRange' place any restriction on the 'Version' or is it
-- in fact equivalent to 'AnyVersion'.
--
-- Note this is a semantic check, not simply a syntactic check. So for example
-- the following is @True@ (for all @v@).
--
-- > isAnyVersion (EarlierVersion v `UnionVersionRanges` orLaterVersion v)
isAnyVersion :: VersionRange -> Bool
isAnyVersion vr = case asVersionIntervals vr of
  [VersionInterval (LowerBound v InclusiveBound) NoUpperBound] -> v == version0
  _ -> False

-- A fast and non-precise version of 'isAnyVersion',
-- returns 'True' only for @>= 0@ 'VersionRange's.
--
-- /Do not use/. The "VersionIntervals don't destroy MajorBoundVersion"
-- https://github.com/haskell/cabal/pull/6736 pull-request
-- will change 'simplifyVersionRange' to properly preserve semantics.
-- Then we can use it to normalise 'VersionRange's in tests.
--
isAnyVersionLight :: VersionRange -> Bool
isAnyVersionLight (OrLaterVersion v) = v == version0
isAnyVersionLight _vr = False

----------------------------
-- Wildcard range utilities
--

isWildcardRange :: Version -> Version -> Bool
isWildcardRange ver1 ver2 = check (versionNumbers ver1) (versionNumbers ver2)
  where
    check [n] [m] | n + 1 == m = True
    check (n : ns) (m : ms) | n == m = check ns ms
    check _ _ = False

-- | Does the version range have an upper bound?
--
-- @since 1.24.0.0
--
-- >>> forM ["< 1", ">= 0 && < 1", ">= 0 || < 1", "^>= 4.20.0.0"] (fmap hasUpperBound . simpleParsec)
-- Just [True,True,False,True]
hasUpperBound :: VersionRange -> Bool
hasUpperBound =
  foldVersionRange
    False
    (const True)
    (const False)
    (const True)
    (&&)
    (||)

-- | Does the version range have an explicit lower bound?
--
-- Note: this function only considers the user-specified lower bounds, but not
-- the implicit >=0 lower bound.
--
-- @since 1.24.0.0
--
-- >>> forM ["< 1", ">= 0 && < 1", ">= 0 || < 1", "^>= 4.20.0.0"] (fmap hasLowerBound . simpleParsec)
-- Just [False,True,False,True]
hasLowerBound :: VersionRange -> Bool
hasLowerBound =
  foldVersionRange
    False
    (const True)
    (const True)
    (const False)
    (&&)
    (||)

-- | Is the upper bound version range (less than or equal (LE, <=)?
--
-- >>> forM ["< 1", "<= 1", ">= 0 && < 1", ">= 0 || < 1", ">= 0 && <= 1", ">= 0 || <= 1", "^>= 4.20.0.0"] (fmap hasLEUpperBound . simpleParsec)
-- Just [False,True,False,False,True,True,False]
hasLEUpperBound :: VersionRange -> Bool
hasLEUpperBound = queryVersionRange (\case LEUpperBound -> True; _ -> False) hasLEUpperBound

-- | Is the lower bound version range greater than (GT, >)?
--
-- >>> forM ["< 1", ">= 0 && < 1", ">= 0 || < 1", "> 0 && < 1", "> 0 || < 1", "^>= 4.20.0.0"] (fmap hasGTLowerBound . simpleParsec)
-- Just [False,False,False,True,True,False]
hasGTLowerBound :: VersionRange -> Bool
hasGTLowerBound = queryVersionRange (\case GTLowerBound -> True; _ -> False) hasGTLowerBound

-- | Does the upper bound version range have a trailing zero?
--
-- >>> forM ["< 1", "< 1.1", "< 1.0", "< 1.1.0", "^>= 4.20.0.0"] (fmap hasTrailingZeroUpperBound . simpleParsec)
-- Just [False,False,True,True,False]
hasTrailingZeroUpperBound :: VersionRange -> Bool
hasTrailingZeroUpperBound = queryVersionRange (\case TZUpperBound -> True; _ -> False) hasTrailingZeroUpperBound

queryVersionRange :: (VersionRangeF VersionRange -> Bool) -> (VersionRange -> Bool) -> VersionRange -> Bool
queryVersionRange pf p (projectVersionRange -> v) =
  let f = queryVersionRange pf p
   in pf v || case v of
        IntersectVersionRangesF x y -> f x || f y
        UnionVersionRangesF x y -> f x || f y
        _ -> False

-- $setup
-- >>> import Distribution.Parsec
-- >>> import Data.Traversable

-- $predicate-examples
--
-- The parsed 'VersionRange' of each version constraint used in the examples for
-- 'hasUpperBound' and 'hasLowerBound' are:
--
-- >>> simpleParsec "< 1" :: Maybe VersionRange
-- Just (EarlierVersion (mkVersion [1]))
-- >>> simpleParsec ">= 0 && < 1" :: Maybe VersionRange
-- Just (IntersectVersionRanges (OrLaterVersion (mkVersion [0])) (EarlierVersion (mkVersion [1])))
-- >>> simpleParsec ">= 0 || < 1" :: Maybe VersionRange
-- Just (UnionVersionRanges (OrLaterVersion (mkVersion [0])) (EarlierVersion (mkVersion [1])))
-- >>> simpleParsec "^>= 4.20.0.0" :: Maybe VersionRange
-- Just (MajorBoundVersion (mkVersion [4,20,0,0]))
