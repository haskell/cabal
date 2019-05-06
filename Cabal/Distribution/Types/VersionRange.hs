module Distribution.Types.VersionRange (
    -- * Version ranges
    VersionRange,

    -- ** Constructing
    anyVersion, noVersion,
    thisVersion, notThisVersion,
    laterVersion, earlierVersion,
    orLaterVersion, orEarlierVersion,
    unionVersionRanges, intersectVersionRanges,
    withinVersion,
    majorBoundVersion,

    -- ** Inspection
    --
    -- See "Distribution.Version" for more utilities.
    withinRange,
    foldVersionRange,
    normaliseVersionRange,
    stripParensVersionRange,
    hasUpperBound,
    hasLowerBound,

    -- ** Cata & ana
    VersionRangeF (..),
    cataVersionRange,
    anaVersionRange,
    hyloVersionRange,
    projectVersionRange,
    embedVersionRange,

    -- ** Utilities
    wildcardUpperBound,
    majorUpperBound,
    isWildcardRange,
    versionRangeParser,
    ) where

import Distribution.Compat.Prelude
import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal
import Prelude ()

-- | Fold over the basic syntactic structure of a 'VersionRange'.
--
-- This provides a syntactic view of the expression defining the version range.
-- The syntactic sugar @\">= v\"@, @\"<= v\"@ and @\"== v.*\"@ is presented
-- in terms of the other basic syntax.
--
-- For a semantic view use 'asVersionIntervals'.
--
foldVersionRange :: a                         -- ^ @\"-any\"@ version
                 -> (Version -> a)            -- ^ @\"== v\"@
                 -> (Version -> a)            -- ^ @\"> v\"@
                 -> (Version -> a)            -- ^ @\"< v\"@
                 -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                 -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                 -> VersionRange -> a
foldVersionRange anyv this later earlier union intersect = fold
  where
    fold = cataVersionRange alg

    alg AnyVersionF                     = anyv
    alg (ThisVersionF v)                = this v
    alg (LaterVersionF v)               = later v
    alg (OrLaterVersionF v)             = union (this v) (later v)
    alg (EarlierVersionF v)             = earlier v
    alg (OrEarlierVersionF v)           = union (this v) (earlier v)
    alg (WildcardVersionF v)            = fold (wildcard v)
    alg (MajorBoundVersionF v)          = fold (majorBound v)
    alg (UnionVersionRangesF v1 v2)     = union v1 v2
    alg (IntersectVersionRangesF v1 v2) = intersect v1 v2
    alg (VersionRangeParensF v)         = v

    wildcard v = intersectVersionRanges
                   (orLaterVersion v)
                   (earlierVersion (wildcardUpperBound v))

    majorBound v = intersectVersionRanges
                     (orLaterVersion v)
                     (earlierVersion (majorUpperBound v))

-- | Normalise 'VersionRange'.
--
-- In particular collapse @(== v || > v)@ into @>= v@, and so on.
normaliseVersionRange :: VersionRange -> VersionRange
normaliseVersionRange = hyloVersionRange embed projectVersionRange
  where
    -- == v || > v, > v || == v  ==>  >= v
    embed (UnionVersionRangesF (ThisVersion v) (LaterVersion v')) | v == v' =
        orLaterVersion v
    embed (UnionVersionRangesF (LaterVersion v) (ThisVersion v')) | v == v' =
        orLaterVersion v

    -- == v || < v, < v || == v  ==>  <= v
    embed (UnionVersionRangesF (ThisVersion v) (EarlierVersion v')) | v == v' =
        orEarlierVersion v
    embed (UnionVersionRangesF (EarlierVersion v) (ThisVersion v')) | v == v' =
        orEarlierVersion v

    -- otherwise embed normally
    embed vr = embedVersionRange vr

-- |  Remove 'VersionRangeParens' constructors.
--
-- @since 2.2
stripParensVersionRange :: VersionRange -> VersionRange
stripParensVersionRange = hyloVersionRange embed projectVersionRange
  where
    embed (VersionRangeParensF vr) = vr
    embed vr = embedVersionRange vr

-- | Does this version fall within the given range?
--
-- This is the evaluation function for the 'VersionRange' type.
--
withinRange :: Version -> VersionRange -> Bool
withinRange v = foldVersionRange
                   True
                   (\v'  -> v == v')
                   (\v'  -> v >  v')
                   (\v'  -> v <  v')
                   (||)
                   (&&)

----------------------------
-- Wildcard range utilities
--

-- | @since 2.2
wildcardUpperBound :: Version -> Version
wildcardUpperBound = alterVersion $
    \lowerBound -> init lowerBound ++ [last lowerBound + 1]

isWildcardRange :: Version -> Version -> Bool
isWildcardRange ver1 ver2 = check (versionNumbers ver1) (versionNumbers ver2)
  where check (n:[]) (m:[]) | n+1 == m = True
        check (n:ns) (m:ms) | n   == m = check ns ms
        check _      _                 = False

-- | Does the version range have an upper bound?
--
-- @since 1.24.0.0
hasUpperBound :: VersionRange -> Bool
hasUpperBound = foldVersionRange
                False
                (const True)
                (const False)
                (const True)
                (&&) (||)

-- | Does the version range have an explicit lower bound?
--
-- Note: this function only considers the user-specified lower bounds, but not
-- the implicit >=0 lower bound.
--
-- @since 1.24.0.0
hasLowerBound :: VersionRange -> Bool
hasLowerBound = foldVersionRange
                False
                (const True)
                (const True)
                (const False)
                (&&) (||)
