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

module Distribution.Version (
  -- * Package versions
  Version,
  mkVersion,
  mkVersion',
  versionNumbers,
  nullVersion,
  alterVersion,

  -- * Version ranges
  VersionRange(..),

  -- ** Constructing
  anyVersion, noVersion,
  thisVersion, notThisVersion,
  laterVersion, earlierVersion,
  orLaterVersion, orEarlierVersion,
  unionVersionRanges, intersectVersionRanges,
  differenceVersionRanges,
  invertVersionRange,
  withinVersion,
  majorBoundVersion,
  betweenVersionsInclusive,

  -- ** Inspection
  withinRange,
  isAnyVersion,
  isNoVersion,
  isSpecificVersion,
  simplifyVersionRange,
  foldVersionRange,
  foldVersionRange',
  hasUpperBound,
  hasLowerBound,

  -- ** Modification
  removeUpperBound,
  removeLowerBound,

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
  invertVersionIntervals

 ) where

import Distribution.Version.Internal
