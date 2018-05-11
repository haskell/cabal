{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

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
  normaliseVersionRange,
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

import Prelude ()
import Distribution.Compat.Prelude
import qualified Data.Version as Base
import Data.Bits (shiftL, shiftR, (.|.), (.&.))

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text
import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.Compat.Parsec as P
import Distribution.Compat.ReadP hiding (get, many)

import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<+>))
import Control.Exception (assert)

import qualified Text.Read as Read

-- -----------------------------------------------------------------------------
-- Versions

-- | A 'Version' represents the version of a software entity.
--
-- Instances of 'Eq' and 'Ord' are provided, which gives exact
-- equality and lexicographic ordering of the version number
-- components (i.e. 2.1 > 2.0, 1.2.3 > 1.2.2, etc.).
--
-- This type is opaque and distinct from the 'Base.Version' type in
-- "Data.Version" since @Cabal-2.0@. The difference extends to the
-- 'Binary' instance using a different (and more compact) encoding.
--
-- @since 2.0.0.2
data Version = PV0 {-# UNPACK #-} !Word64
             | PV1 !Int [Int]
             -- NOTE: If a version fits into the packed Word64
             -- representation (i.e. at most four version components
             -- which all fall into the [0..0xfffe] range), then PV0
             -- MUST be used. This is essential for the 'Eq' instance
             -- to work.
             deriving (Data,Eq,Generic,Typeable)

instance Ord Version where
    compare (PV0 x)    (PV0 y)    = compare x y
    compare (PV1 x xs) (PV1 y ys) = case compare x y of
        EQ -> compare xs ys
        c  -> c
    compare (PV0 w)    (PV1 y ys) = case compare x y of
        EQ -> compare [x2,x3,x4] ys
        c  -> c
      where
        x  = fromIntegral ((w `shiftR` 48) .&. 0xffff) - 1
        x2 = fromIntegral ((w `shiftR` 32) .&. 0xffff) - 1
        x3 = fromIntegral ((w `shiftR` 16) .&. 0xffff) - 1
        x4 = fromIntegral               (w .&. 0xffff) - 1
    compare (PV1 x xs) (PV0 w)    = case compare x y of
        EQ -> compare xs [y2,y3,y4]
        c  -> c
      where
        y  = fromIntegral ((w `shiftR` 48) .&. 0xffff) - 1
        y2 = fromIntegral ((w `shiftR` 32) .&. 0xffff) - 1
        y3 = fromIntegral ((w `shiftR` 16) .&. 0xffff) - 1
        y4 = fromIntegral               (w .&. 0xffff) - 1

instance Show Version where
    showsPrec d v = showParen (d > 10)
        $ showString "mkVersion "
        . showsPrec 11 (versionNumbers v)

instance Read Version where
    readPrec = Read.parens $ do
        Read.Ident "mkVersion" <- Read.lexP
        v <- Read.step Read.readPrec
        return (mkVersion v)

instance Binary Version

instance NFData Version where
    rnf (PV0 _) = ()
    rnf (PV1 _ ns) = rnf ns

instance Pretty Version where
  pretty ver
    = Disp.hcat (Disp.punctuate (Disp.char '.')
                                (map Disp.int $ versionNumbers ver))

instance Parsec Version where
    parsec = mkVersion <$> P.sepBy1 P.integral (P.char '.') <* tags
      where
        tags = do
            ts <- many $ P.char '-' *> some (P.satisfy isAlphaNum)
            case ts of
                []      -> pure ()
                (_ : _) -> parsecWarning PWTVersionTag "version with tags"

instance Text Version where
  parse = do
      branch <- Parse.sepBy1 parseNat (Parse.char '.')
                -- allow but ignore tags:
      _tags  <- Parse.many (Parse.char '-' >> Parse.munch1 isAlphaNum)
      return (mkVersion branch)
    where
      parseNat = read `fmap` Parse.munch1 isDigit

-- | Construct 'Version' from list of version number components.
--
-- For instance, @mkVersion [3,2,1]@ constructs a 'Version'
-- representing the version @3.2.1@.
--
-- All version components must be non-negative. @mkVersion []@
-- currently represents the special /null/ version; see also 'nullVersion'.
--
-- @since 2.0.0.2
mkVersion :: [Int] -> Version
-- TODO: add validity check; disallow 'mkVersion []' (we have
-- 'nullVersion' for that)
mkVersion []                    = nullVersion
mkVersion (v1:[])
  | inWord16VerRep1 v1          = PV0 (mkWord64VerRep1 v1)
  | otherwise                   = PV1 v1 []
  where
    inWord16VerRep1 x1 = inWord16 (x1 .|. (x1+1))
    mkWord64VerRep1 y1 = mkWord64VerRep (y1+1) 0 0 0

mkVersion (v1:vs@(v2:[]))
  | inWord16VerRep2 v1 v2       = PV0 (mkWord64VerRep2 v1 v2)
  | otherwise                   = PV1 v1 vs
  where
    inWord16VerRep2 x1 x2 = inWord16 (x1 .|. (x1+1)
                                  .|. x2 .|. (x2+1))
    mkWord64VerRep2 y1 y2 = mkWord64VerRep (y1+1) (y2+1) 0 0

mkVersion (v1:vs@(v2:v3:[]))
  | inWord16VerRep3 v1 v2 v3    = PV0 (mkWord64VerRep3 v1 v2 v3)
  | otherwise                   = PV1 v1 vs
  where
    inWord16VerRep3 x1 x2 x3 = inWord16 (x1 .|. (x1+1)
                                     .|. x2 .|. (x2+1)
                                     .|. x3 .|. (x3+1))
    mkWord64VerRep3 y1 y2 y3 = mkWord64VerRep (y1+1) (y2+1) (y3+1) 0

mkVersion (v1:vs@(v2:v3:v4:[]))
  | inWord16VerRep4 v1 v2 v3 v4 = PV0 (mkWord64VerRep4 v1 v2 v3 v4)
  | otherwise                   = PV1 v1 vs
  where
    inWord16VerRep4 x1 x2 x3 x4 = inWord16 (x1 .|. (x1+1)
                                        .|. x2 .|. (x2+1)
                                        .|. x3 .|. (x3+1)
                                        .|. x4 .|. (x4+1))
    mkWord64VerRep4 y1 y2 y3 y4 = mkWord64VerRep (y1+1) (y2+1) (y3+1) (y4+1)

mkVersion (v1:vs)               = PV1 v1 vs


{-# INLINE mkWord64VerRep #-}
mkWord64VerRep :: Int -> Int -> Int -> Int -> Word64
mkWord64VerRep v1 v2 v3 v4 =
      (fromIntegral v1 `shiftL` 48)
  .|. (fromIntegral v2 `shiftL` 32)
  .|. (fromIntegral v3 `shiftL` 16)
  .|.  fromIntegral v4

{-# INLINE inWord16 #-}
inWord16 :: Int -> Bool
inWord16 x = (fromIntegral x :: Word) <= 0xffff

-- | Variant of 'Version' which converts a "Data.Version" 'Version'
-- into Cabal's 'Version' type.
--
-- @since 2.0.0.2
mkVersion' :: Base.Version -> Version
mkVersion' = mkVersion . Base.versionBranch

-- | Unpack 'Version' into list of version number components.
--
-- This is the inverse to 'mkVersion', so the following holds:
--
-- > (versionNumbers . mkVersion) vs == vs
--
-- @since 2.0.0.2
versionNumbers :: Version -> [Int]
versionNumbers (PV1 n ns) = n:ns
versionNumbers (PV0 w)
  | v1 < 0    = []
  | v2 < 0    = [v1]
  | v3 < 0    = [v1,v2]
  | v4 < 0    = [v1,v2,v3]
  | otherwise = [v1,v2,v3,v4]
  where
    v1 = fromIntegral ((w `shiftR` 48) .&. 0xffff) - 1
    v2 = fromIntegral ((w `shiftR` 32) .&. 0xffff) - 1
    v3 = fromIntegral ((w `shiftR` 16) .&. 0xffff) - 1
    v4 = fromIntegral (w .&. 0xffff) - 1


-- | Constant representing the special /null/ 'Version'
--
-- The 'nullVersion' compares (via 'Ord') as less than every proper
-- 'Version' value.
--
-- @since 2.0.0.2
nullVersion :: Version
-- TODO: at some point, 'mkVersion' may disallow creating /null/
-- 'Version's
nullVersion = PV0 0

-- | Apply function to list of version number components
--
-- > alterVersion f == mkVersion . f . versionNumbers
--
-- @since 2.0.0.2
alterVersion :: ([Int] -> [Int]) -> Version -> Version
alterVersion f = mkVersion . f . versionNumbers

-- internal helper
validVersion :: Version -> Bool
validVersion v = v /= nullVersion && all (>=0) (versionNumbers v)

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
  | MajorBoundVersion      Version -- @^>= ver@ (same as >= ver && < MAJ(ver)+1)
  | UnionVersionRanges     VersionRange VersionRange
  | IntersectVersionRanges VersionRange VersionRange
  | VersionRangeParens     VersionRange -- just '(exp)' parentheses syntax
  deriving (Data, Eq, Generic, Read, Show, Typeable)

instance Binary VersionRange

instance NFData VersionRange where rnf = genericRnf

{-# DeprecateD AnyVersion
    "Use 'anyVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED ThisVersion
    "Use 'thisVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED LaterVersion
    "Use 'laterVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED EarlierVersion
    "Use 'earlierVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED WildcardVersion
    "Use 'anyVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED UnionVersionRanges
    "Use 'unionVersionRanges', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED IntersectVersionRanges
    "Use 'intersectVersionRanges', 'foldVersionRange' or 'asVersionIntervals'"#-}

-- | The version range @-any@. That is, a version range containing all
-- versions.
--
-- > withinRange v anyVersion = True
--
anyVersion :: VersionRange
anyVersion = AnyVersion

-- | The empty version range, that is a version range containing no versions.
--
-- This can be constructed using any unsatisfiable version range expression,
-- for example @> 1 && < 1@.
--
-- > withinRange v noVersion = False
--
noVersion :: VersionRange
noVersion = IntersectVersionRanges (LaterVersion v) (EarlierVersion v)
  where v = mkVersion [1]

-- | The version range @== v@
--
-- > withinRange v' (thisVersion v) = v' == v
--
thisVersion :: Version -> VersionRange
thisVersion = ThisVersion

-- | The version range @< v || > v@
--
-- > withinRange v' (notThisVersion v) = v' /= v
--
notThisVersion :: Version -> VersionRange
notThisVersion v = UnionVersionRanges (EarlierVersion v) (LaterVersion v)

-- | The version range @> v@
--
-- > withinRange v' (laterVersion v) = v' > v
--
laterVersion :: Version -> VersionRange
laterVersion = LaterVersion

-- | The version range @>= v@
--
-- > withinRange v' (orLaterVersion v) = v' >= v
--
orLaterVersion :: Version -> VersionRange
orLaterVersion   v = UnionVersionRanges (ThisVersion v) (LaterVersion v)

-- | The version range @< v@
--
-- > withinRange v' (earlierVersion v) = v' < v
--
earlierVersion :: Version -> VersionRange
earlierVersion = EarlierVersion

-- | The version range @<= v@
--
-- > withinRange v' (orEarlierVersion v) = v' <= v
--
orEarlierVersion :: Version -> VersionRange
orEarlierVersion v = UnionVersionRanges (ThisVersion v) (EarlierVersion v)

-- | The version range @vr1 || vr2@
--
-- >   withinRange v' (unionVersionRanges vr1 vr2)
-- > = withinRange v' vr1 || withinRange v' vr2
--
unionVersionRanges :: VersionRange -> VersionRange -> VersionRange
unionVersionRanges = UnionVersionRanges

-- | The version range @vr1 && vr2@
--
-- >   withinRange v' (intersectVersionRanges vr1 vr2)
-- > = withinRange v' vr1 && withinRange v' vr2
--
intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges = IntersectVersionRanges

-- | The difference of two version ranges
--
-- >   withinRange v' (differenceVersionRanges vr1 vr2)
-- > = withinRange v' vr1 && not (withinRange v' vr2)
--
-- @since 1.24.1.0
differenceVersionRanges :: VersionRange -> VersionRange -> VersionRange
differenceVersionRanges vr1 vr2 =
    intersectVersionRanges vr1 (invertVersionRange vr2)

-- | The inverse of a version range
--
-- >   withinRange v' (invertVersionRange vr)
-- > = not (withinRange v' vr)
--
invertVersionRange :: VersionRange -> VersionRange
invertVersionRange =
    fromVersionIntervals . invertVersionIntervals
    . VersionIntervals . asVersionIntervals

-- | The version range @== v.*@.
--
-- For example, for version @1.2@, the version range @== 1.2.*@ is the same as
-- @>= 1.2 && < 1.3@
--
-- > withinRange v' (laterVersion v) = v' >= v && v' < upper v
-- >   where
-- >     upper (Version lower t) = Version (init lower ++ [last lower + 1]) t
--
withinVersion :: Version -> VersionRange
withinVersion = WildcardVersion

-- | The version range @^>= v@.
--
-- For example, for version @1.2.3.4@, the version range @^>= 1.2.3.4@ is the same as
-- @>= 1.2.3.4 && < 1.3@.
--
-- Note that @^>= 1@ is equivalent to @>= 1 && < 1.1@.
--
-- @since 2.0.0.2
majorBoundVersion :: Version -> VersionRange
majorBoundVersion = MajorBoundVersion

-- In practice this is not very useful because we normally use inclusive lower
-- bounds and exclusive upper bounds.
--
-- > withinRange v' (laterVersion v) = v' > v
--
betweenVersionsInclusive :: Version -> Version -> VersionRange
betweenVersionsInclusive v1 v2 =
  IntersectVersionRanges (orLaterVersion v1) (orEarlierVersion v2)

{-# DEPRECATED betweenVersionsInclusive
    "In practice this is not very useful because we normally use inclusive lower bounds and exclusive upper bounds" #-}

-- | Given a version range, remove the highest upper bound. Example: @(>= 1 && <
-- 3) || (>= 4 && < 5)@ is converted to @(>= 1 && < 3) || (>= 4)@.
removeUpperBound :: VersionRange -> VersionRange
removeUpperBound = fromVersionIntervals . relaxLastInterval . toVersionIntervals
  where
    relaxLastInterval (VersionIntervals intervals) =
      VersionIntervals (relaxLastInterval' intervals)

    relaxLastInterval' []      = []
    relaxLastInterval' [(l,_)] = [(l, NoUpperBound)]
    relaxLastInterval' (i:is)  = i : relaxLastInterval' is

-- | Given a version range, remove the lowest lower bound.
-- Example: @(>= 1 && < 3) || (>= 4 && < 5)@ is converted to
-- @(>= 0 && < 3) || (>= 4 && < 5)@.
removeLowerBound :: VersionRange -> VersionRange
removeLowerBound = fromVersionIntervals . relaxHeadInterval . toVersionIntervals
  where
    relaxHeadInterval (VersionIntervals intervals) =
      VersionIntervals (relaxHeadInterval' intervals)

    relaxHeadInterval' []         = []
    relaxHeadInterval' ((_,u):is) = (minLowerBound,u) : is

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
    fold AnyVersion                     = anyv
    fold (ThisVersion v)                = this v
    fold (LaterVersion v)               = later v
    fold (EarlierVersion v)             = earlier v
    fold (WildcardVersion v)            = fold (wildcard v)
    fold (MajorBoundVersion v)          = fold (majorBound v)
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersect (fold v1) (fold v2)
    fold (VersionRangeParens v)         = fold v

    wildcard v = intersectVersionRanges
                   (orLaterVersion v)
                   (earlierVersion (wildcardUpperBound v))

    majorBound v = intersectVersionRanges
                     (orLaterVersion v)
                     (earlierVersion (majorUpperBound v))

-- | An extended variant of 'foldVersionRange' that also provides a view of the
-- expression in which the syntactic sugar @\">= v\"@, @\"<= v\"@ and @\"==
-- v.*\"@ is presented explicitly rather than in terms of the other basic
-- syntax.
--
foldVersionRange' :: a                         -- ^ @\"-any\"@ version
                  -> (Version -> a)            -- ^ @\"== v\"@
                  -> (Version -> a)            -- ^ @\"> v\"@
                  -> (Version -> a)            -- ^ @\"< v\"@
                  -> (Version -> a)            -- ^ @\">= v\"@
                  -> (Version -> a)            -- ^ @\"<= v\"@
                  -> (Version -> Version -> a) -- ^ @\"== v.*\"@ wildcard. The
                                               -- function is passed the
                                               -- inclusive lower bound and the
                                               -- exclusive upper bounds of the
                                               -- range defined by the wildcard.
                  -> (Version -> Version -> a) -- ^ @\"^>= v\"@ major upper bound
                                               -- The function is passed the
                                               -- inclusive lower bound and the
                                               -- exclusive major upper bounds
                                               -- of the range defined by this
                                               -- operator.
                  -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                  -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                  -> (a -> a)                  -- ^ @\"(_)\"@ parentheses
                  -> VersionRange -> a
foldVersionRange' anyv this later earlier orLater orEarlier
                  wildcard major union intersect parens = fold
  where
    fold AnyVersion                     = anyv
    fold (ThisVersion v)                = this v
    fold (LaterVersion v)               = later v
    fold (EarlierVersion v)             = earlier v

    fold (UnionVersionRanges (ThisVersion    v)
                             (LaterVersion   v')) | v==v' = orLater v
    fold (UnionVersionRanges (LaterVersion   v)
                             (ThisVersion    v')) | v==v' = orLater v
    fold (UnionVersionRanges (ThisVersion    v)
                             (EarlierVersion v')) | v==v' = orEarlier v
    fold (UnionVersionRanges (EarlierVersion v)
                             (ThisVersion    v')) | v==v' = orEarlier v

    fold (WildcardVersion v)            = wildcard v (wildcardUpperBound v)
    fold (MajorBoundVersion v)          = major v (majorUpperBound v)
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersect (fold v1) (fold v2)
    fold (VersionRangeParens v)         = parens (fold v)

-- | Normalise 'VersionRange'.
--
-- @foldVersionRange' anyVersion thisVersion ...@
--
-- Note: strips parens constructor.
normaliseVersionRange :: VersionRange -> VersionRange
normaliseVersionRange = foldVersionRange'
    anyVersion
    thisVersion
    laterVersion
    earlierVersion
    orLaterVersion
    orEarlierVersion
    (\v _ -> withinVersion v)
    (\v _ -> majorBoundVersion v)
    unionVersionRanges
    intersectVersionRanges
    id

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

-- | Does this 'VersionRange' place any restriction on the 'Version' or is it
-- in fact equivalent to 'AnyVersion'.
--
-- Note this is a semantic check, not simply a syntactic check. So for example
-- the following is @True@ (for all @v@).
--
-- > isAnyVersion (EarlierVersion v `UnionVersionRanges` orLaterVersion v)
--
isAnyVersion :: VersionRange -> Bool
isAnyVersion vr = case asVersionIntervals vr of
  [(LowerBound v InclusiveBound, NoUpperBound)] | isVersion0 v -> True
  _                                                            -> False

-- | This is the converse of 'isAnyVersion'. It check if the version range is
-- empty, if there is no possible version that satisfies the version range.
--
-- For example this is @True@ (for all @v@):
--
-- > isNoVersion (EarlierVersion v `IntersectVersionRanges` LaterVersion v)
--
isNoVersion :: VersionRange -> Bool
isNoVersion vr = case asVersionIntervals vr of
  [] -> True
  _  -> False

-- | Is this version range in fact just a specific version?
--
-- For example the version range @\">= 3 && <= 3\"@ contains only the version
-- @3@.
--
isSpecificVersion :: VersionRange -> Maybe Version
isSpecificVersion vr = case asVersionIntervals vr of
  [(LowerBound v  InclusiveBound
   ,UpperBound v' InclusiveBound)]
    | v == v' -> Just v
  _           -> Nothing

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
--
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange vr
    -- If the version range is inconsistent then we just return the
    -- original since that has more information than ">1 && < 1", which
    -- is the canonical inconsistent version range.
    | null (versionIntervals vi) = vr
    | otherwise                  = fromVersionIntervals vi
  where
    vi = toVersionIntervals vr

----------------------------
-- Wildcard range utilities
--

wildcardUpperBound :: Version -> Version
wildcardUpperBound = alterVersion $
    \lowerBound -> init lowerBound ++ [last lowerBound + 1]

isWildcardRange :: Version -> Version -> Bool
isWildcardRange ver1 ver2 = check (versionNumbers ver1) (versionNumbers ver2)
  where check (n:[]) (m:[]) | n+1 == m = True
        check (n:ns) (m:ms) | n   == m = check ns ms
        check _      _                 = False

-- | Compute next greater major version to be used as upper bound
--
-- Example: @0.4.1@ produces the version @0.5@ which then can be used
-- to construct a range @>= 0.4.1 && < 0.5@
majorUpperBound :: Version -> Version
majorUpperBound = alterVersion $ \numbers -> case numbers of
    []        -> [0,1] -- should not happen
    [m1]      -> [m1,1] -- e.g. version '1'
    (m1:m2:_) -> [m1,m2+1]

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
minLowerBound = LowerBound (mkVersion [0]) InclusiveBound

isVersion0 :: Version -> Bool
isVersion0 = (== mkVersion [0])

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
-- and no intervals may overlap or touch. If any of these conditions are not
-- satisfied the function returns @Nothing@.
--
mkVersionIntervals :: [VersionInterval] -> Maybe VersionIntervals
mkVersionIntervals intervals
  | invariant (VersionIntervals intervals) = Just (VersionIntervals intervals)
  | otherwise                              = Nothing

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
    interval (LowerBound v  InclusiveBound)
             (UpperBound v' ExclusiveBound) | isWildcardRange v v'
                 = WildcardVersion v
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

-------------------------------
-- Parsing and pretty printing
--

instance Pretty VersionRange where
  pretty = fst
       . foldVersionRange'                         -- precedence:
           (         Disp.text "-any"                           , 0 :: Int)
           (\v   -> (Disp.text "==" <<>> pretty v                   , 0))
           (\v   -> (Disp.char '>'  <<>> pretty v                   , 0))
           (\v   -> (Disp.char '<'  <<>> pretty v                   , 0))
           (\v   -> (Disp.text ">=" <<>> pretty v                   , 0))
           (\v   -> (Disp.text "<=" <<>> pretty v                   , 0))
           (\v _ -> (Disp.text "==" <<>> dispWild v               , 0))
           (\v _ -> (Disp.text "^>=" <<>> pretty v                  , 0))
           -- @punct@ aren't symmetric, because || and && are infixr
           (\(r1, p1) (r2, p2) ->
             (punct 1 p1 r1 <+> Disp.text "||" <+> punct 2 p2 r2 , 2))
           (\(r1, p1) (r2, p2) ->
             (punct 0 p1 r1 <+> Disp.text "&&" <+> punct 1 p2 r2 , 1))
           (\(r, _)   -> (Disp.parens r, 0))

    where dispWild ver =
               Disp.hcat (Disp.punctuate (Disp.char '.')
                                         (map Disp.int $ versionNumbers ver))
            <<>> Disp.text ".*"
          punct p p' | p < p'    = Disp.parens
                     | otherwise = id

instance Parsec VersionRange where
    parsec = normaliseVersionRange <$> expr
      where
        expr   = do P.spaces
                    t <- term
                    P.spaces
                    (do _  <- P.string "||"
                        P.spaces
                        e <- expr
                        return (unionVersionRanges t e)
                     <|>
                     return t)
        term   = do f <- factor
                    P.spaces
                    (do _  <- P.string "&&"
                        P.spaces
                        t <- term
                        return (intersectVersionRanges f t)
                     <|>
                     return f)
        factor = P.choice
            $ parens expr
            : parseAnyVersion
            : parseNoVersion
            : parseWildcardRange
            : map parseRangeOp rangeOps
        parseAnyVersion    = P.string "-any" >> return anyVersion
        parseNoVersion     = P.string "-none" >> return noVersion

        parseWildcardRange = P.try $ do
          _ <- P.string "=="
          P.spaces
          branch <- some (P.integral <* P.char '.')
          _ <- P.char '*'
          return (withinVersion (mkVersion branch))

        parens p = P.between
            (P.char '(' >> P.spaces)
            (P.char ')' >> P.spaces)
            (do a <- p
                P.spaces
                return (VersionRangeParens a))

        -- TODO: make those non back-tracking
        parseRangeOp (s,f) = P.try (P.string s *> P.spaces *> fmap f parsec)
        rangeOps = [ ("<",  earlierVersion),
                     ("<=", orEarlierVersion),
                     (">",  laterVersion),
                     (">=", orLaterVersion),
                     ("^>=", majorBoundVersion),
                     ("==", thisVersion) ]

instance Text VersionRange where
  parse = normaliseVersionRange <$> expr
   where
        expr   = do Parse.skipSpaces
                    t <- term
                    Parse.skipSpaces
                    (do _  <- Parse.string "||"
                        Parse.skipSpaces
                        e <- expr
                        return (UnionVersionRanges t e)
                     +++
                     return t)
        term   = do f <- factor
                    Parse.skipSpaces
                    (do _  <- Parse.string "&&"
                        Parse.skipSpaces
                        t <- term
                        return (IntersectVersionRanges f t)
                     +++
                     return f)
        factor = Parse.choice $ parens expr
                              : parseAnyVersion
                              : parseNoVersion
                              : parseWildcardRange
                              : map parseRangeOp rangeOps
        parseAnyVersion    = Parse.string "-any" >> return AnyVersion
        parseNoVersion     = Parse.string "-none" >> return noVersion

        parseWildcardRange = do
          _ <- Parse.string "=="
          Parse.skipSpaces
          branch <- Parse.sepBy1 digits (Parse.char '.')
          _ <- Parse.char '.'
          _ <- Parse.char '*'
          return (WildcardVersion (mkVersion branch))

        parens p = Parse.between (Parse.char '(' >> Parse.skipSpaces)
                                 (Parse.char ')' >> Parse.skipSpaces)
                                 (do a <- p
                                     Parse.skipSpaces
                                     return (VersionRangeParens a))

        digits = do
          firstDigit <- Parse.satisfy isDigit
          if firstDigit == '0'
            then return 0
            else do rest <- Parse.munch isDigit
                    return (read (firstDigit : rest)) -- TODO: eradicateNoParse

        parseRangeOp (s,f) = Parse.string s >> Parse.skipSpaces >> fmap f parse
        rangeOps = [ ("<",  EarlierVersion),
                     ("<=", orEarlierVersion),
                     (">",  LaterVersion),
                     (">=", orLaterVersion),
                     ("^>=", MajorBoundVersion),
                     ("==", ThisVersion) ]

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
