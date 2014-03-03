{-# LANGUAGE CPP, DeriveDataTypeable, StandaloneDeriving #-}
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
  Version(..),

  -- * Version ranges
  VersionRange(..),

  -- ** Constructing
  anyVersion, noVersion,
  thisVersion, notThisVersion,
  laterVersion, earlierVersion,
  orLaterVersion, orEarlierVersion,
  unionVersionRanges, intersectVersionRanges,
  withinVersion,
  betweenVersionsInclusive,

  -- ** Inspection
  withinRange,
  isAnyVersion,
  isNoVersion,
  isSpecificVersion,
  simplifyVersionRange,
  foldVersionRange,
  foldVersionRange',

  -- ** Modification
  removeUpperBound,

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

import Data.Data        ( Data )
import Data.Typeable    ( Typeable )
import Data.Version     ( Version(..) )

import Distribution.Text ( Text(..) )
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((+++))
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>), (<+>))
import qualified Data.Char as Char (isDigit)
import Control.Exception (assert)

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
  | UnionVersionRanges     VersionRange VersionRange
  | IntersectVersionRanges VersionRange VersionRange
  | VersionRangeParens     VersionRange -- just '(exp)' parentheses syntax
  deriving (Show,Read,Eq,Typeable,Data)

#if __GLASGOW_HASKELL__ < 707
-- starting with ghc-7.7/base-4.7 this instance is provided in "Data.Data"
deriving instance Data Version
#endif

{-# DEPRECATED AnyVersion "Use 'anyVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED ThisVersion "use 'thisVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED LaterVersion "use 'laterVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED EarlierVersion "use 'earlierVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED WildcardVersion "use 'anyVersion', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED UnionVersionRanges "use 'unionVersionRanges', 'foldVersionRange' or 'asVersionIntervals'" #-}
{-# DEPRECATED IntersectVersionRanges "use 'intersectVersionRanges', 'foldVersionRange' or 'asVersionIntervals'" #-}

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
  where v = Version [1] []

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

-- | The version range @>= v1 && <= v2@.
--
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

-- | Fold over the basic syntactic structure of a 'VersionRange'.
--
-- This provides a syntacic view of the expression defining the version range.
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
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersect (fold v1) (fold v2)
    fold (VersionRangeParens v)         = fold v

    wildcard v = intersectVersionRanges
                   (orLaterVersion v)
                   (earlierVersion (wildcardUpperBound v))

-- | An extended variant of 'foldVersionRange' that also provides a view of
-- in which the syntactic sugar @\">= v\"@, @\"<= v\"@ and @\"== v.*\"@ is presented
-- explicitly rather than in terms of the other basic syntax.
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
                  -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                  -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                  -> (a -> a)                  -- ^ @\"(_)\"@ parentheses
                  -> VersionRange -> a
foldVersionRange' anyv this later earlier orLater orEarlier
                  wildcard union intersect parens = fold
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
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersect (fold v1) (fold v2)
    fold (VersionRangeParens v)         = parens (fold v)


-- | Does this version fall within the given range?
--
-- This is the evaluation function for the 'VersionRange' type.
--
withinRange :: Version -> VersionRange -> Bool
withinRange v = foldVersionRange
                   True
                   (\v'  -> versionBranch v == versionBranch v')
                   (\v'  -> versionBranch v >  versionBranch v')
                   (\v'  -> versionBranch v <  versionBranch v')
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
wildcardUpperBound (Version lowerBound ts) = Version upperBound ts
  where
    upperBound = init lowerBound ++ [last lowerBound + 1]

isWildcardRange :: Version -> Version -> Bool
isWildcardRange (Version branch1 _) (Version branch2 _) = check branch1 branch2
  where check (n:[]) (m:[]) | n+1 == m = True
        check (n:ns) (m:ms) | n   == m = check ns ms
        check _      _                 = False

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

-------------------------------
-- Parsing and pretty printing
--

instance Text VersionRange where
  disp = fst
       . foldVersionRange'                         -- precedence:
           (         Disp.text "-any"                           , 0 :: Int)
           (\v   -> (Disp.text "==" <> disp v                   , 0))
           (\v   -> (Disp.char '>'  <> disp v                   , 0))
           (\v   -> (Disp.char '<'  <> disp v                   , 0))
           (\v   -> (Disp.text ">=" <> disp v                   , 0))
           (\v   -> (Disp.text "<=" <> disp v                   , 0))
           (\v _ -> (Disp.text "==" <> dispWild v               , 0))
           (\(r1, p1) (r2, p2) -> (punct 2 p1 r1 <+> Disp.text "||" <+> punct 2 p2 r2 , 2))
           (\(r1, p1) (r2, p2) -> (punct 1 p1 r1 <+> Disp.text "&&" <+> punct 1 p2 r2 , 1))
           (\(r, p)   -> (Disp.parens r, p))

    where dispWild (Version b _) =
               Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int b))
            <> Disp.text ".*"
          punct p p' | p < p'    = Disp.parens
                     | otherwise = id

  parse = expr
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
                              : parseWildcardRange
                              : map parseRangeOp rangeOps
        parseAnyVersion    = Parse.string "-any" >> return AnyVersion

        parseWildcardRange = do
          _ <- Parse.string "=="
          Parse.skipSpaces
          branch <- Parse.sepBy1 digits (Parse.char '.')
          _ <- Parse.char '.'
          _ <- Parse.char '*'
          return (WildcardVersion (Version branch []))

        parens p = Parse.between (Parse.char '(' >> Parse.skipSpaces)
                                 (Parse.char ')' >> Parse.skipSpaces)
                                 (do a <- p
                                     Parse.skipSpaces
                                     return (VersionRangeParens a))

        digits = do
          first <- Parse.satisfy Char.isDigit
          if first == '0'
            then return 0
            else do rest <- Parse.munch Char.isDigit
                    return (read (first : rest))

        parseRangeOp (s,f) = Parse.string s >> Parse.skipSpaces >> fmap f parse
        rangeOps = [ ("<",  EarlierVersion),
                     ("<=", orEarlierVersion),
                     (">",  LaterVersion),
                     (">=", orLaterVersion),
                     ("==", ThisVersion) ]
