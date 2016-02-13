{-# OPTIONS_GHC -fno-warn-orphans
                -fno-warn-incomplete-patterns
                -fno-warn-deprecations
                -fno-warn-unused-binds #-} --FIXME
module UnitTests.Distribution.Version (versionTests) where

import Distribution.Version
import Distribution.Text

import Text.PrettyPrint as Disp (text, render, parens, hcat
                                ,punctuate, int, char, (<>), (<+>))

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Utils
import qualified Test.Laws as Laws

import Control.Monad (liftM, liftM2)
import Data.Maybe (isJust, fromJust)
import Data.List (sort, sortBy, nub)
import Data.Ord  (comparing)

versionTests :: [TestTree]
versionTests =
  zipWith (\n p -> testProperty ("Range Property " ++ show n) p) [1::Int ..]
    -- properties to validate the test framework
  [ property prop_nonNull
  , property prop_gen_intervals1
  , property prop_gen_intervals2
--, property prop_equivalentVersionRange --FIXME: runs out of test cases
  , property prop_intermediateVersion

    -- the basic syntactic version range functions
  , property prop_anyVersion
  , property prop_noVersion
  , property prop_thisVersion
  , property prop_notThisVersion
  , property prop_laterVersion
  , property prop_orLaterVersion
  , property prop_earlierVersion
  , property prop_orEarlierVersion
  , property prop_unionVersionRanges
  , property prop_intersectVersionRanges
  , property prop_invertVersionRange
  , property prop_withinVersion
  , property prop_foldVersionRange
  , property prop_foldVersionRange'

    -- the semantic query functions
--, property prop_isAnyVersion1       --FIXME: runs out of test cases
--, property prop_isAnyVersion2       --FIXME: runs out of test cases
--, property prop_isNoVersion         --FIXME: runs out of test cases
--, property prop_isSpecificVersion1  --FIXME: runs out of test cases
--, property prop_isSpecificVersion2  --FIXME: runs out of test cases
  , property prop_simplifyVersionRange1
  , property prop_simplifyVersionRange1'
--, property prop_simplifyVersionRange2   --FIXME: runs out of test cases
--, property prop_simplifyVersionRange2'  --FIXME: runs out of test cases
--, property prop_simplifyVersionRange2'' --FIXME: actually wrong

    -- converting between version ranges and version intervals
  , property prop_to_intervals
--, property prop_to_intervals_canonical  --FIXME: runs out of test cases
--, property prop_to_intervals_canonical' --FIXME: runs out of test cases
  , property prop_from_intervals
  , property prop_to_from_intervals
  , property prop_from_to_intervals
  , property prop_from_to_intervals'

    -- union and intersection of version intervals
  , property prop_unionVersionIntervals
  , property prop_unionVersionIntervals_idempotent
  , property prop_unionVersionIntervals_commutative
  , property prop_unionVersionIntervals_associative
  , property prop_intersectVersionIntervals
  , property prop_intersectVersionIntervals_idempotent
  , property prop_intersectVersionIntervals_commutative
  , property prop_intersectVersionIntervals_associative
  , property prop_union_intersect_distributive
  , property prop_intersect_union_distributive

    -- inversion of version intervals
  , property prop_invertVersionIntervals
  , property prop_invertVersionIntervalsTwice
  ]

-- parseTests :: [TestTree]
-- parseTests =
--   zipWith (\n p -> testProperty ("Parse Property " ++ show n) p) [1::Int ..]
--    -- parsing and pretty printing
--   [ -- property prop_parse_disp1  --FIXME: actually wrong

--     --  These are also wrong, see
--     --  https://github.com/haskell/cabal/issues/3037#issuecomment-177671011

--     --   property prop_parse_disp2
--     -- , property prop_parse_disp3
--     -- , property prop_parse_disp4
--     -- , property prop_parse_disp5
--   ]

instance Arbitrary Version where
  arbitrary = do
    branch <- smallListOf1 $
                frequency [(3, return 0)
                          ,(3, return 1)
                          ,(2, return 2)
                          ,(1, return 3)]
    return (Version branch []) -- deliberate []
    where
      smallListOf1 = adjustSize (\n -> min 5 (n `div` 3)) . listOf1

  shrink (Version branch []) =
    [ Version branch' [] | branch' <- shrink branch, not (null branch') ]
  shrink (Version branch _tags) =
    [ Version branch [] ]

instance Arbitrary VersionRange where
  arbitrary = sized verRangeExp
    where
      verRangeExp n = frequency $
        [ (2, return anyVersion)
        , (1, liftM thisVersion arbitrary)
        , (1, liftM laterVersion arbitrary)
        , (1, liftM orLaterVersion arbitrary)
        , (1, liftM orLaterVersion' arbitrary)
        , (1, liftM earlierVersion arbitrary)
        , (1, liftM orEarlierVersion arbitrary)
        , (1, liftM orEarlierVersion' arbitrary)
        , (1, liftM withinVersion arbitrary)
        , (2, liftM VersionRangeParens arbitrary)
        ] ++ if n == 0 then [] else
        [ (2, liftM2 unionVersionRanges     verRangeExp2 verRangeExp2)
        , (2, liftM2 intersectVersionRanges verRangeExp2 verRangeExp2)
        ]
        where
          verRangeExp2 = verRangeExp (n `div` 2)

      orLaterVersion'   v =
        unionVersionRanges (LaterVersion v)   (ThisVersion v)
      orEarlierVersion' v =
        unionVersionRanges (EarlierVersion v) (ThisVersion v)

---------------------------
-- VersionRange properties
--

prop_nonNull :: Version -> Bool
prop_nonNull = not . null . versionBranch

prop_anyVersion :: Version -> Bool
prop_anyVersion v' =
  withinRange v' anyVersion == True

prop_noVersion :: Version -> Bool
prop_noVersion v' =
  withinRange v' noVersion == False

prop_thisVersion :: Version -> Version -> Bool
prop_thisVersion v v' =
     withinRange v' (thisVersion v)
  == (v' == v)

prop_notThisVersion :: Version -> Version -> Bool
prop_notThisVersion v v' =
     withinRange v' (notThisVersion v)
  == (v' /= v)

prop_laterVersion :: Version -> Version -> Bool
prop_laterVersion v v' =
     withinRange v' (laterVersion v)
  == (v' > v)

prop_orLaterVersion :: Version -> Version -> Bool
prop_orLaterVersion v v' =
     withinRange v' (orLaterVersion v)
  == (v' >= v)

prop_earlierVersion :: Version -> Version -> Bool
prop_earlierVersion v v' =
     withinRange v' (earlierVersion v)
  == (v' < v)

prop_orEarlierVersion :: Version -> Version -> Bool
prop_orEarlierVersion v v' =
     withinRange v' (orEarlierVersion v)
  == (v' <= v)

prop_unionVersionRanges :: VersionRange -> VersionRange -> Version -> Bool
prop_unionVersionRanges vr1 vr2 v' =
     withinRange v' (unionVersionRanges vr1 vr2)
  == (withinRange v' vr1 || withinRange v' vr2)

prop_intersectVersionRanges :: VersionRange -> VersionRange -> Version -> Bool
prop_intersectVersionRanges vr1 vr2 v' =
     withinRange v' (intersectVersionRanges vr1 vr2)
  == (withinRange v' vr1 && withinRange v' vr2)

prop_invertVersionRange :: VersionRange -> Version -> Bool
prop_invertVersionRange vr v' =
     withinRange v' (invertVersionRange vr)
  == not (withinRange v' vr)

prop_withinVersion :: Version -> Version -> Bool
prop_withinVersion v v' =
     withinRange v' (withinVersion v)
  == (v' >= v && v' < upper v)
  where
    upper (Version lower t) = Version (init lower ++ [last lower + 1]) t

prop_foldVersionRange :: VersionRange -> Bool
prop_foldVersionRange range =
     expandWildcard range
  == foldVersionRange anyVersion thisVersion
                      laterVersion earlierVersion
                      unionVersionRanges intersectVersionRanges
                      range
  where
    expandWildcard (WildcardVersion v) =
        intersectVersionRanges (orLaterVersion v) (earlierVersion (upper v))
      where
        upper (Version lower t) = Version (init lower ++ [last lower + 1]) t

    expandWildcard (UnionVersionRanges     v1 v2) =
      UnionVersionRanges (expandWildcard v1) (expandWildcard v2)
    expandWildcard (IntersectVersionRanges v1 v2) =
      IntersectVersionRanges (expandWildcard v1) (expandWildcard v2)
    expandWildcard (VersionRangeParens v) = expandWildcard v
    expandWildcard v = v


prop_foldVersionRange' :: VersionRange -> Bool
prop_foldVersionRange' range =
     canonicalise range
  == foldVersionRange' anyVersion thisVersion
                       laterVersion earlierVersion
                       orLaterVersion orEarlierVersion
                       (\v _ -> withinVersion v)
                       unionVersionRanges intersectVersionRanges id
                       range
  where
    canonicalise (UnionVersionRanges (LaterVersion v)
                                     (ThisVersion  v')) | v == v'
                = UnionVersionRanges (ThisVersion   v')
                                     (LaterVersion  v)
    canonicalise (UnionVersionRanges (EarlierVersion v)
                                     (ThisVersion    v')) | v == v'
                = UnionVersionRanges (ThisVersion    v')
                                     (EarlierVersion v)
    canonicalise (UnionVersionRanges v1 v2) =
      UnionVersionRanges (canonicalise v1) (canonicalise v2)
    canonicalise (IntersectVersionRanges v1 v2) =
      IntersectVersionRanges (canonicalise v1) (canonicalise v2)
    canonicalise (VersionRangeParens v) = canonicalise v
    canonicalise v = v


prop_isAnyVersion1 :: VersionRange -> Version -> Property
prop_isAnyVersion1 range version =
  isAnyVersion range ==> withinRange version range

prop_isAnyVersion2 :: VersionRange -> Property
prop_isAnyVersion2 range =
  isAnyVersion range ==>
    foldVersionRange True (\_ -> False) (\_ -> False) (\_ -> False)
                          (\_ _ -> False) (\_ _ -> False)
      (simplifyVersionRange range)

prop_isNoVersion :: VersionRange -> Version -> Property
prop_isNoVersion range version =
  isNoVersion range ==> not (withinRange version range)

prop_isSpecificVersion1 :: VersionRange -> NonEmptyList Version -> Property
prop_isSpecificVersion1 range (NonEmpty versions) =
  isJust version && not (null versions') ==>
    allEqual (fromJust version : versions')
  where
    version     = isSpecificVersion range
    versions'   = filter (`withinRange` range) versions
    allEqual xs = and (zipWith (==) xs (tail xs))

prop_isSpecificVersion2 :: VersionRange -> Property
prop_isSpecificVersion2 range =
  isJust version ==>
    foldVersionRange Nothing Just (\_ -> Nothing) (\_ -> Nothing)
                     (\_ _ -> Nothing) (\_ _ -> Nothing)
      (simplifyVersionRange range)
    == version

  where
    version = isSpecificVersion range

-- | 'simplifyVersionRange' is a semantic identity on 'VersionRange'.
--
prop_simplifyVersionRange1 :: VersionRange -> Version -> Bool
prop_simplifyVersionRange1 range version =
  withinRange version range == withinRange version (simplifyVersionRange range)

prop_simplifyVersionRange1' :: VersionRange -> Bool
prop_simplifyVersionRange1' range =
  range `equivalentVersionRange` (simplifyVersionRange range)

-- | 'simplifyVersionRange' produces a canonical form for ranges with
-- equivalent semantics.
--
prop_simplifyVersionRange2 :: VersionRange -> VersionRange -> Version -> Property
prop_simplifyVersionRange2 r r' v =
  r /= r' && simplifyVersionRange r == simplifyVersionRange r' ==>
    withinRange v r == withinRange v r'

prop_simplifyVersionRange2' :: VersionRange -> VersionRange -> Property
prop_simplifyVersionRange2' r r' =
  r /= r' && simplifyVersionRange r == simplifyVersionRange r' ==>
    r `equivalentVersionRange` r'

--FIXME: see equivalentVersionRange for details
prop_simplifyVersionRange2'' :: VersionRange -> VersionRange -> Property
prop_simplifyVersionRange2'' r r' =
  r /= r' && r `equivalentVersionRange` r' ==>
       simplifyVersionRange r == simplifyVersionRange r'
    || isNoVersion r
    || isNoVersion r'

--------------------
-- VersionIntervals
--

-- | Generating VersionIntervals
--
-- This is a tad tricky as VersionIntervals is an abstract type, so we first
-- make a local type for generating the internal representation. Then we check
-- that this lets us construct valid 'VersionIntervals'.
--
newtype VersionIntervals' = VersionIntervals' [VersionInterval]
  deriving (Eq, Show)

instance Arbitrary VersionIntervals' where
  arbitrary = do
    ubound <- arbitrary
    bounds <- arbitrary
    let intervals = mergeTouching
                  . map fixEmpty
                  . replaceUpper ubound
                  . pairs
                  . sortBy (comparing fst)
                  $ bounds
    return (VersionIntervals' intervals)

    where
      pairs ((l, lb):(u, ub):bs) = (LowerBound l lb, UpperBound u ub)
                                 : pairs bs
      pairs _                    = []

      replaceUpper NoUpperBound [(l,_)] = [(l, NoUpperBound)]
      replaceUpper NoUpperBound (i:is)  = i : replaceUpper NoUpperBound is
      replaceUpper _               is   = is

      -- merge adjacent intervals that touch
      mergeTouching (i1@(l,u):i2@(l',u'):is)
        | doesNotTouch u l' = i1 : mergeTouching (i2:is)
        | otherwise         =      mergeTouching ((l,u'):is)
      mergeTouching is      = is

      doesNotTouch :: UpperBound -> LowerBound -> Bool
      doesNotTouch NoUpperBound _ = False
      doesNotTouch (UpperBound u ub) (LowerBound l lb) =
            u <  l
        || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)

      fixEmpty (LowerBound l _, UpperBound u _)
        | l == u = (LowerBound l InclusiveBound, UpperBound u InclusiveBound)
      fixEmpty i = i

  shrink (VersionIntervals' intervals) =
    [ VersionIntervals' intervals' | intervals' <- shrink intervals ]

instance Arbitrary Bound where
  arbitrary = elements [ExclusiveBound, InclusiveBound]

instance Arbitrary LowerBound where
  arbitrary = liftM2 LowerBound arbitrary arbitrary

instance Arbitrary UpperBound where
  arbitrary = oneof [return NoUpperBound
                    ,liftM2 UpperBound arbitrary arbitrary]

-- | Check that our VersionIntervals' arbitrary instance generates intervals
-- that satisfies the invariant.
--
prop_gen_intervals1 :: VersionIntervals' -> Bool
prop_gen_intervals1 (VersionIntervals' intervals) =
  isJust (mkVersionIntervals intervals)

instance Arbitrary VersionIntervals where
  arbitrary = do
    VersionIntervals' intervals <- arbitrary
    case mkVersionIntervals intervals of
      Just xs -> return xs

-- | Check that constructing our intervals type and converting it to a
-- 'VersionRange' and then into the true intervals type gives us back
-- the exact same sequence of intervals. This tells us that our arbitrary
-- instance for 'VersionIntervals'' is ok.
--
prop_gen_intervals2 :: VersionIntervals' -> Bool
prop_gen_intervals2 (VersionIntervals' intervals') =
    asVersionIntervals (fromVersionIntervals intervals) == intervals'
  where
    Just intervals = mkVersionIntervals intervals'

-- | Check that 'VersionIntervals' models 'VersionRange' via
-- 'toVersionIntervals'.
--
prop_to_intervals :: VersionRange -> Version -> Bool
prop_to_intervals range version =
  withinRange version range == withinIntervals version intervals
  where
    intervals = toVersionIntervals range

-- | Check that semantic equality on 'VersionRange's is the same as converting
-- to 'VersionIntervals' and doing syntactic equality.
--
prop_to_intervals_canonical :: VersionRange -> VersionRange -> Property
prop_to_intervals_canonical r r' =
  r /= r' && r `equivalentVersionRange` r' ==>
    toVersionIntervals r == toVersionIntervals r'

prop_to_intervals_canonical' :: VersionRange -> VersionRange -> Property
prop_to_intervals_canonical' r r' =
  r /= r' && toVersionIntervals r == toVersionIntervals r' ==>
    r `equivalentVersionRange` r'

-- | Check that 'VersionIntervals' models 'VersionRange' via
-- 'fromVersionIntervals'.
--
prop_from_intervals :: VersionIntervals -> Version -> Bool
prop_from_intervals intervals version =
  withinRange version range == withinIntervals version intervals
  where
    range = fromVersionIntervals intervals

-- | @'toVersionIntervals' . 'fromVersionIntervals'@ is an exact identity on
-- 'VersionIntervals'.
--
prop_to_from_intervals :: VersionIntervals -> Bool
prop_to_from_intervals intervals =
  toVersionIntervals (fromVersionIntervals intervals) == intervals

-- | @'fromVersionIntervals' . 'toVersionIntervals'@ is a semantic identity on
-- 'VersionRange', though not necessarily a syntactic identity.
--
prop_from_to_intervals :: VersionRange -> Bool
prop_from_to_intervals range =
  range' `equivalentVersionRange` range
  where
    range' = fromVersionIntervals (toVersionIntervals range)

-- | Equivalent of 'prop_from_to_intervals'
--
prop_from_to_intervals' :: VersionRange -> Version -> Bool
prop_from_to_intervals' range version =
  withinRange version range' == withinRange version range
  where
    range' = fromVersionIntervals (toVersionIntervals range)

-- | The semantics of 'unionVersionIntervals' is (||).
--
prop_unionVersionIntervals :: VersionIntervals -> VersionIntervals
                           -> Version -> Bool
prop_unionVersionIntervals is1 is2 v =
     withinIntervals v (unionVersionIntervals is1 is2)
  == (withinIntervals v is1 || withinIntervals v is2)

-- | 'unionVersionIntervals' is idempotent
--
prop_unionVersionIntervals_idempotent :: VersionIntervals -> Bool
prop_unionVersionIntervals_idempotent =
  Laws.idempotent_binary unionVersionIntervals

-- | 'unionVersionIntervals' is commutative
--
prop_unionVersionIntervals_commutative :: VersionIntervals
                                       -> VersionIntervals -> Bool
prop_unionVersionIntervals_commutative =
  Laws.commutative unionVersionIntervals

-- | 'unionVersionIntervals' is associative
--
prop_unionVersionIntervals_associative :: VersionIntervals
                                       -> VersionIntervals
                                       -> VersionIntervals -> Bool
prop_unionVersionIntervals_associative =
  Laws.associative unionVersionIntervals

-- | The semantics of 'intersectVersionIntervals' is (&&).
--
prop_intersectVersionIntervals :: VersionIntervals -> VersionIntervals
                               -> Version -> Bool
prop_intersectVersionIntervals is1 is2 v =
     withinIntervals v (intersectVersionIntervals is1 is2)
  == (withinIntervals v is1 && withinIntervals v is2)

-- | 'intersectVersionIntervals' is idempotent
--
prop_intersectVersionIntervals_idempotent :: VersionIntervals -> Bool
prop_intersectVersionIntervals_idempotent =
  Laws.idempotent_binary intersectVersionIntervals

-- | 'intersectVersionIntervals' is commutative
--
prop_intersectVersionIntervals_commutative :: VersionIntervals
                                           -> VersionIntervals -> Bool
prop_intersectVersionIntervals_commutative =
  Laws.commutative intersectVersionIntervals

-- | 'intersectVersionIntervals' is associative
--
prop_intersectVersionIntervals_associative :: VersionIntervals
                                           -> VersionIntervals
                                           -> VersionIntervals -> Bool
prop_intersectVersionIntervals_associative =
  Laws.associative intersectVersionIntervals

-- | 'unionVersionIntervals' distributes over 'intersectVersionIntervals'
--
prop_union_intersect_distributive :: Property
prop_union_intersect_distributive =
      Laws.distributive_left  unionVersionIntervals intersectVersionIntervals
  .&. Laws.distributive_right unionVersionIntervals intersectVersionIntervals

-- | 'intersectVersionIntervals' distributes over 'unionVersionIntervals'
--
prop_intersect_union_distributive :: Property
prop_intersect_union_distributive =
      Laws.distributive_left  intersectVersionIntervals unionVersionIntervals
  .&. Laws.distributive_right intersectVersionIntervals unionVersionIntervals

-- | The semantics of 'invertVersionIntervals' is 'not'.
--
prop_invertVersionIntervals :: VersionIntervals
                               -> Version -> Bool
prop_invertVersionIntervals vi v =
     withinIntervals v (invertVersionIntervals vi)
  == not (withinIntervals v vi)

-- | Double application of 'invertVersionIntervals' is the identity function
prop_invertVersionIntervalsTwice :: VersionIntervals -> Bool
prop_invertVersionIntervalsTwice vi =
    invertVersionIntervals (invertVersionIntervals vi) == vi



--------------------------------
-- equivalentVersionRange helper

prop_equivalentVersionRange :: VersionRange  -> VersionRange
                            -> Version -> Property
prop_equivalentVersionRange range range' version =
  equivalentVersionRange range range' && range /= range' ==>
    withinRange version range == withinRange version range'

--FIXME: this is wrong. consider version ranges "<=1" and "<1.0"
--       this algorithm cannot distinguish them because there is no version
--       that is included by one that is excluded by the other.
--       Alternatively we must reconsider the semantics of '<' and '<='
--       in version ranges / version intervals. Perhaps the canonical
--       representation should use just < v and interpret "<= v" as "< v.0".
equivalentVersionRange :: VersionRange -> VersionRange -> Bool
equivalentVersionRange vr1 vr2 =
  let allVersionsUsed = nub (sort (versionsUsed vr1 ++ versionsUsed vr2))
      minPoint = Version [0] []
      maxPoint | null allVersionsUsed = minPoint
               | otherwise = case maximum allVersionsUsed of
                   Version vs _ -> Version (vs ++ [1]) []
      probeVersions = minPoint : maxPoint
                    : intermediateVersions allVersionsUsed

  in all (\v -> withinRange v vr1 == withinRange v vr2) probeVersions

  where
    versionsUsed = foldVersionRange [] (\x->[x]) (\x->[x]) (\x->[x]) (++) (++)
    intermediateVersions (v1:v2:vs) = v1 : intermediateVersion v1 v2
                                         : intermediateVersions (v2:vs)
    intermediateVersions vs = vs

intermediateVersion :: Version -> Version -> Version
intermediateVersion v1 v2 | v1 >= v2 = error "intermediateVersion: v1 >= v2"
intermediateVersion (Version v1 _) (Version v2 _) =
  Version (intermediateList v1 v2) []
  where
    intermediateList :: [Int] -> [Int] -> [Int]
    intermediateList []     (_:_) = [0]
    intermediateList (x:xs) (y:ys)
        | x <  y    = x : xs ++ [0]
        | otherwise = x : intermediateList xs ys

prop_intermediateVersion :: Version -> Version -> Property
prop_intermediateVersion v1 v2 =
  (v1 /= v2) && not (adjacentVersions v1 v2) ==>
  if v1 < v2
    then let v = intermediateVersion v1 v2
          in (v1 < v && v < v2)
    else let v = intermediateVersion v2 v1
          in v1 > v && v > v2

adjacentVersions :: Version -> Version -> Bool
adjacentVersions (Version v1 _) (Version v2 _) = v1 ++ [0] == v2
                                              || v2 ++ [0] == v1

--------------------------------
-- Parsing and pretty printing
--

prop_parse_disp1 :: VersionRange -> Bool
prop_parse_disp1 vr =
  fmap stripParens (simpleParse (display vr)) == Just (canonicalise vr)

  where
    canonicalise = swizzle . swap

    swizzle     (UnionVersionRanges (UnionVersionRanges v1 v2) v3)
      | not (isOrLaterVersion v1 v2) && not (isOrEarlierVersion v1 v2)
      = swizzle (UnionVersionRanges v1 (UnionVersionRanges v2  v3))

    swizzle     (IntersectVersionRanges (IntersectVersionRanges v1 v2) v3)
      = swizzle (IntersectVersionRanges v1 (IntersectVersionRanges v2  v3))

    swizzle (UnionVersionRanges v1 v2) =
      UnionVersionRanges (swizzle v1) (swizzle v2)
    swizzle (IntersectVersionRanges v1 v2) =
      IntersectVersionRanges (swizzle v1) (swizzle v2)
    swizzle (VersionRangeParens v) = swizzle v
    swizzle v = v

    isOrLaterVersion (ThisVersion  v) (LaterVersion v') = v == v'
    isOrLaterVersion _                _                 = False

    isOrEarlierVersion (ThisVersion v)    (EarlierVersion v') = v == v'
    isOrEarlierVersion _                  _                   = False

    swap =
      foldVersionRange' anyVersion thisVersion
                        laterVersion earlierVersion
                        orLaterVersion orEarlierVersion
                        (\v _ -> withinVersion v)
                        unionVersionRanges intersectVersionRanges id

    stripParens :: VersionRange -> VersionRange
    stripParens (VersionRangeParens v) = stripParens v
    stripParens (UnionVersionRanges v1 v2) =
      UnionVersionRanges (stripParens v1) (stripParens v2)
    stripParens (IntersectVersionRanges v1 v2) =
      IntersectVersionRanges (stripParens v1) (stripParens v2)
    stripParens v = v

prop_parse_disp2 :: VersionRange -> Property
prop_parse_disp2 vr =
  let b = fmap (display :: VersionRange -> String) (simpleParse (display vr))
      a = Just (display vr)
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp3 :: VersionRange -> Property
prop_parse_disp3 vr =
  let a = Just (display vr)
      b = fmap displayRaw (simpleParse (display vr))
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp4 :: VersionRange -> Property
prop_parse_disp4 vr =
  let a = Just vr
      b = (simpleParse (display vr))
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp5 :: VersionRange -> Property
prop_parse_disp5 vr =
  let a = Just vr
      b = simpleParse (displayRaw vr)
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

displayRaw :: VersionRange -> String
displayRaw =
   Disp.render
 . foldVersionRange'                         -- precedence:
     -- All the same as the usual pretty printer, except for the parens
     (          Disp.text "-any")
     (\v     -> Disp.text "==" <> disp v)
     (\v     -> Disp.char '>'  <> disp v)
     (\v     -> Disp.char '<'  <> disp v)
     (\v     -> Disp.text ">=" <> disp v)
     (\v     -> Disp.text "<=" <> disp v)
     (\v _   -> Disp.text "==" <> dispWild v)
     (\r1 r2 -> r1 <+> Disp.text "||" <+> r2)
     (\r1 r2 -> r1 <+> Disp.text "&&" <+> r2)
     (\r     -> Disp.parens r) -- parens

  where
    dispWild (Version b _) =
           Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int b))
        <> Disp.text ".*"
