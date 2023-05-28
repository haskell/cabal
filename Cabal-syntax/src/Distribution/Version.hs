-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Version
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
--                Duncan Coutts 2008
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Exports the 'Version' type along with a parser and pretty printer. A version
-- is something like @\"1.3.3\"@. It also defines the 'VersionRange' data
-- types. Version ranges are like @\">= 1.2 && < 2\"@.
module Distribution.Version
  ( -- * Package versions
    Version
  , version0
  , mkVersion
  , mkVersion'
  , versionNumbers
  , nullVersion
  , alterVersion

    -- * Version ranges
  , VersionRange

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
  , withinRange
  , isAnyVersion
  , isNoVersion
  , isSpecificVersion
  , simplifyVersionRange
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
  , wildcardUpperBound
  , majorUpperBound

    -- ** Modification
  , removeUpperBound
  , removeLowerBound
  , transformCaret
  , transformCaretUpper
  , transformCaretLower

    -- * Version intervals view
  , asVersionIntervals
  , VersionInterval (..)
  , LowerBound (..)
  , UpperBound (..)
  , Bound (..)

    -- ** 'VersionIntervals' abstract type

    -- | The 'VersionIntervals' type and the accompanying functions are exposed
    -- primarily for completeness and testing purposes. In practice
    -- 'asVersionIntervals' is the main function to use to
    -- view a 'VersionRange' as a bunch of 'VersionInterval's.
  , VersionIntervals
  , toVersionIntervals
  , fromVersionIntervals
  , unVersionIntervals
  ) where

import Distribution.Types.Version
import Distribution.Types.VersionInterval
import Distribution.Types.VersionRange

-------------------------------------------------------------------------------
-- Utilities on VersionRange requiring VersionInterval
-------------------------------------------------------------------------------

-- | This is the converse of 'isAnyVersion'. It check if the version range is
-- empty, if there is no possible version that satisfies the version range.
--
-- For example this is @True@ (for all @v@):
--
-- > isNoVersion (EarlierVersion v `IntersectVersionRanges` LaterVersion v)
isNoVersion :: VersionRange -> Bool
isNoVersion vr = case asVersionIntervals vr of
  [] -> True
  _ -> False

-- | Is this version range in fact just a specific version?
--
-- For example the version range @\">= 3 && <= 3\"@ contains only the version
-- @3@.
isSpecificVersion :: VersionRange -> Maybe Version
isSpecificVersion vr = case asVersionIntervals vr of
  [VersionInterval (LowerBound v InclusiveBound) (UpperBound v' InclusiveBound)]
    | v == v' -> Just v
  _ -> Nothing

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

-- | Simplify a 'VersionRange' expression. For non-empty version ranges
-- this produces a canonical form. Empty or inconsistent version ranges
-- are left as-is because that provides more information.
--
-- If you need a canonical form use
-- @fromVersionIntervals . toVersionIntervals@
--
-- It satisfies the following properties:
--
-- > withinRange v (simplifyVersionRange r) = withinRange v r
--
-- >     withinRange v r = withinRange v r'
-- > ==> simplifyVersionRange r = simplifyVersionRange r'
-- >  || isNoVersion r
-- >  || isNoVersion r'
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange vr
  -- If the version range is inconsistent then we just return the
  -- original since that has more information than ">1 && < 1", which
  -- is the canonical inconsistent version range.
  | null (unVersionIntervals vi) = vr
  | otherwise = fromVersionIntervals vi
  where
    vi = toVersionIntervals vr

-- | Given a version range, remove the highest upper bound. Example: @(>= 1 && <
-- 3) || (>= 4 && < 5)@ is converted to @(>= 1 && < 3) || (>= 4)@.
removeUpperBound :: VersionRange -> VersionRange
removeUpperBound = fromVersionIntervals . relaxLastInterval . toVersionIntervals

-- | Given a version range, remove the lowest lower bound.
-- Example: @(>= 1 && < 3) || (>= 4 && < 5)@ is converted to
-- @(>= 0 && < 3) || (>= 4 && < 5)@.
removeLowerBound :: VersionRange -> VersionRange
removeLowerBound = fromVersionIntervals . relaxHeadInterval . toVersionIntervals

-- | Rewrite @^>= x.y.z@ into @>= x.y.z && < x.(y+1)@
--
-- @since 3.6.0.0
transformCaret :: VersionRange -> VersionRange
transformCaret = hyloVersionRange embed projectVersionRange
  where
    embed (MajorBoundVersionF v) = orLaterVersion v `intersectVersionRanges` earlierVersion (majorUpperBound v)
    embed vr = embedVersionRange vr

-- | Rewrite @^>= x.y.z@ into @>= x.y.z@
--
-- @since 3.6.0.0
transformCaretUpper :: VersionRange -> VersionRange
transformCaretUpper = hyloVersionRange embed projectVersionRange
  where
    embed (MajorBoundVersionF v) = orLaterVersion v
    embed vr = embedVersionRange vr

-- | Rewrite @^>= x.y.z@ into @<x.(y+1)@
--
-- @since 3.6.0.0
transformCaretLower :: VersionRange -> VersionRange
transformCaretLower = hyloVersionRange embed projectVersionRange
  where
    embed (MajorBoundVersionF v) = earlierVersion (majorUpperBound v)
    embed vr = embedVersionRange vr
