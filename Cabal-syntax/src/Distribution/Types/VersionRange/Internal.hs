{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | The only purpose of this module is to prevent the export of
-- 'VersionRange' constructors from
-- "Distribution.Types.VersionRange". To avoid creating orphan
-- instances, a lot of related code had to be moved here too.
module Distribution.Types.VersionRange.Internal
  ( VersionRange (..)
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
  , VersionRangeF (.., LEUpperBound, GTLowerBound, TZUpperBound)
  , projectVersionRange
  , embedVersionRange
  , cataVersionRange
  , anaVersionRange
  , hyloVersionRange
  , majorUpperBound
  , wildcardUpperBound
  ) where

import Distribution.Compat.Prelude
import Distribution.Types.Version.Internal
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Utils.Generic (unsnoc)

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Text.PrettyPrint as Disp

import Distribution.Types.Namespace

data VersionRange
  = ThisVersion Version -- = version
  | LaterVersion Version -- > version  (NB. not >=)
  | OrLaterVersion Version -- >= version
  | EarlierVersion Version -- < version
  | OrEarlierVersion Version -- <= version
  | MajorBoundVersion Version -- @^>= ver@ (same as >= ver && < MAJ(ver)+1)
  | UnionVersionRanges VersionRange VersionRange
  | IntersectVersionRanges VersionRange VersionRange
  deriving (Data, Eq, Ord, Generic, Read, Show)

instance Binary VersionRange
instance Structured VersionRange
instance NFData VersionRange where rnf = genericRnf

instance Namespace VersionRange

-- | The version range @-any@. That is, a version range containing all
-- versions.
--
-- > withinRange v anyVersion = True
anyVersion :: VersionRange
anyVersion = OrLaterVersion (mkVersion [0])

-- | The empty version range @-none@, that is a version range containing no versions.
--
-- This can be constructed using any unsatisfiable version range expression,
-- for example @< 0@.
--
-- > withinRange v noVersion = False
noVersion :: VersionRange
noVersion = EarlierVersion (mkVersion [0])

-- | The version range @== v@.
--
-- > withinRange v' (thisVersion v) = v' == v
thisVersion :: Version -> VersionRange
thisVersion = ThisVersion

-- | The version range @/= v@.
--
-- > withinRange v' (notThisVersion v) = v' /= v
notThisVersion :: Version -> VersionRange
notThisVersion v = UnionVersionRanges (EarlierVersion v) (LaterVersion v)

-- | The version range @> v@.
--
-- > withinRange v' (laterVersion v) = v' > v
laterVersion :: Version -> VersionRange
laterVersion = LaterVersion

-- | The version range @>= v@.
--
-- > withinRange v' (orLaterVersion v) = v' >= v
orLaterVersion :: Version -> VersionRange
orLaterVersion = OrLaterVersion

-- | The version range @< v@.
--
-- > withinRange v' (earlierVersion v) = v' < v
earlierVersion :: Version -> VersionRange
earlierVersion = EarlierVersion

-- | The version range @<= v@.
--
-- > withinRange v' (orEarlierVersion v) = v' <= v
orEarlierVersion :: Version -> VersionRange
orEarlierVersion = OrEarlierVersion

-- | The version range @vr1 || vr2@.
--
-- >   withinRange v' (unionVersionRanges vr1 vr2)
-- > = withinRange v' vr1 || withinRange v' vr2
unionVersionRanges :: VersionRange -> VersionRange -> VersionRange
unionVersionRanges = UnionVersionRanges

-- | The version range @vr1 && vr2@.
--
-- >   withinRange v' (intersectVersionRanges vr1 vr2)
-- > = withinRange v' vr1 && withinRange v' vr2
intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges = IntersectVersionRanges

-- | The version range @== v.*@.
--
-- For example, for version @1.2@, the version range @== 1.2.*@ is the same as
-- @>= 1.2 && < 1.3@.
--
-- > withinRange v' (withinVersion v) = v' >= v && v' < upper v
-- >   where
-- >     upper (Version lower t) = Version (init lower ++ [last lower + 1]) t
withinVersion :: Version -> VersionRange
withinVersion v =
  intersectVersionRanges
    (orLaterVersion v)
    (earlierVersion (wildcardUpperBound v))

-- | The version range @^>= v@.
--
-- For example, for version @1.2.3.4@, the version range @^>= 1.2.3.4@
-- is the same as @>= 1.2.3.4 && < 1.3@.
--
-- Note that @^>= 1@ is equivalent to @>= 1 && < 1.1@.
--
-- @since 2.0.0.2
majorBoundVersion :: Version -> VersionRange
majorBoundVersion = MajorBoundVersion

-- | F-Algebra of 'VersionRange'. See 'cataVersionRange'.
--
-- @since 2.2
data VersionRangeF a
  = -- | @== version@.
    ThisVersionF Version
  | -- | @>  version@.   NB: not @>=@
    LaterVersionF Version
  | -- | @>= version@.
    OrLaterVersionF Version
  | -- | @<  version@.
    EarlierVersionF Version
  | -- | @<= version@.
    OrEarlierVersionF Version
  | -- | @^>= version@, same as @>= version && < MAJ(version)+1@.
    MajorBoundVersionF Version
  | -- | @||@.
    UnionVersionRangesF a a
  | -- | @&&@.
    IntersectVersionRangesF a a
  deriving
    ( Data
    , Eq
    , Generic
    , Read
    , Show
    , Functor
    , Foldable
    , Traversable
    )

pattern LEUpperBound, GTLowerBound, TZUpperBound :: VersionRangeF a
pattern LEUpperBound <- OrEarlierVersionF _
pattern GTLowerBound <- LaterVersionF _
pattern TZUpperBound <- (upperTrailingZero -> True)

upperTrailingZero :: VersionRangeF a -> Bool
upperTrailingZero (OrEarlierVersionF x) = trailingZero x
upperTrailingZero (EarlierVersionF x) = trailingZero x
upperTrailingZero _ = False

trailingZero :: Version -> Bool
trailingZero (versionNumbers -> vs)
  | [0] <- vs = False
  | 0 : _ <- reverse vs = True
  | otherwise = False

-- | Generic destructor for 'VersionRange'.
--
-- @since 2.2
projectVersionRange :: VersionRange -> VersionRangeF VersionRange
projectVersionRange (ThisVersion v) = ThisVersionF v
projectVersionRange (LaterVersion v) = LaterVersionF v
projectVersionRange (OrLaterVersion v) = OrLaterVersionF v
projectVersionRange (EarlierVersion v) = EarlierVersionF v
projectVersionRange (OrEarlierVersion v) = OrEarlierVersionF v
projectVersionRange (MajorBoundVersion v) = MajorBoundVersionF v
projectVersionRange (UnionVersionRanges a b) = UnionVersionRangesF a b
projectVersionRange (IntersectVersionRanges a b) = IntersectVersionRangesF a b

-- | Fold 'VersionRange'.
--
-- @since 2.2
cataVersionRange :: (VersionRangeF a -> a) -> VersionRange -> a
cataVersionRange f = c where c = f . fmap c . projectVersionRange

-- | Generic constructor for 'VersionRange'.
--
-- @since 2.2
embedVersionRange :: VersionRangeF VersionRange -> VersionRange
embedVersionRange (ThisVersionF v) = ThisVersion v
embedVersionRange (LaterVersionF v) = LaterVersion v
embedVersionRange (OrLaterVersionF v) = OrLaterVersion v
embedVersionRange (EarlierVersionF v) = EarlierVersion v
embedVersionRange (OrEarlierVersionF v) = OrEarlierVersion v
embedVersionRange (MajorBoundVersionF v) = MajorBoundVersion v
embedVersionRange (UnionVersionRangesF a b) = UnionVersionRanges a b
embedVersionRange (IntersectVersionRangesF a b) = IntersectVersionRanges a b

-- | Unfold 'VersionRange'.
--
-- @since 2.2
anaVersionRange :: (a -> VersionRangeF a) -> a -> VersionRange
anaVersionRange g = a where a = embedVersionRange . fmap a . g

-- | Refold 'VersionRange'.
--
-- @since 2.2
hyloVersionRange
  :: (VersionRangeF VersionRange -> VersionRange)
  -> (VersionRange -> VersionRangeF VersionRange)
  -> VersionRange
  -> VersionRange
hyloVersionRange f g = h where h = f . fmap h . g

-------------------------------------------------------------------------------
-- Parsec & Pretty
-------------------------------------------------------------------------------

----------------------------
-- Wildcard range utilities
--

-- | Compute next greater major version to be used as upper bound.
--
-- Example: @0.4.1@ produces the version @0.5@ which then can be used
-- to construct a range @>= 0.4.1 && < 0.5@
--
-- @since 2.2
majorUpperBound :: Version -> Version
majorUpperBound = alterVersion $ \numbers -> case numbers of
  [] -> [0, 1] -- should not happen
  [m1] -> [m1, 1] -- e.g. version '1'
  (m1 : m2 : _) -> [m1, m2 + 1]

-- | Increment the last version number.
--
-- Example: For @1.2@ this returns @1.3@
-- so that it can be used as upper bound when resolving @== 1.2.*@.
-- For @0.4.1@ it returns @0.4.2@.
--
-- @since 2.2
wildcardUpperBound :: Version -> Version
wildcardUpperBound = alterVersion $
  \lowerBound -> case unsnoc lowerBound of
    Nothing -> []
    Just (xs, x) -> xs ++ [x + 1]
