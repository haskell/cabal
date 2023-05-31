module Distribution.Types.VersionRange
  ( -- * Version ranges
    VersionRange

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

    -- ** Inspection

  --
  -- See "Distribution.Version" for more utilities.
  , withinRange
  , foldVersionRange
  , normaliseVersionRange
  , stripParensVersionRange
  , hasUpperBound
  , hasLowerBound

    -- ** Cata & ana
  , VersionRangeF (..)
  , cataVersionRange
  , anaVersionRange
  , hyloVersionRange
  , projectVersionRange
  , embedVersionRange

    -- ** Utilities
  , isAnyVersion
  , isAnyVersionLight
  , wildcardUpperBound
  , majorUpperBound
  , isWildcardRange
  , versionRangeParser
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
    alg (OrLaterVersionF v) = union (this v) (later v)
    alg (EarlierVersionF v) = earlier v
    alg (OrEarlierVersionF v) = union (this v) (earlier v)
    alg (MajorBoundVersionF v) = fold (majorBound v)
    alg (UnionVersionRangesF v1 v2) = union v1 v2
    alg (IntersectVersionRangesF v1 v2) = intersect v1 v2

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
    check (n : []) (m : []) | n + 1 == m = True
    check (n : ns) (m : ms) | n == m = check ns ms
    check _ _ = False

-- | Does the version range have an upper bound?
--
-- @since 1.24.0.0
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
hasLowerBound :: VersionRange -> Bool
hasLowerBound =
  foldVersionRange
    False
    (const True)
    (const True)
    (const False)
    (&&)
    (||)
