{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns
                -fno-warn-deprecations
                -fno-warn-unused-binds #-} --FIXME
module UnitTests.Distribution.Version (versionTests) where

import Distribution.Compat.Prelude.Internal
import Prelude ()

import Distribution.Parsec                      (simpleParsec)
import Distribution.Pretty
import Distribution.Types.VersionRange.Internal
import Distribution.Utils.Generic
import Distribution.Version


import Data.Maybe                      (fromJust)
import Data.Typeable                   (typeOf)
import Test.QuickCheck                 (Arbitrary (..), NonEmptyList (..), NonNegative (..), Property, Testable, counterexample, property, (===), (==>), vectorOf, sized, choose, arbitrarySizedNatural)
import Test.QuickCheck.Instances.Cabal ()
import Test.Tasty                      (TestTree)
import Test.Tasty.QuickCheck           (testProperty)

import qualified Distribution.Types.VersionInterval        as New
import qualified Distribution.Types.VersionInterval.Legacy as Old
import qualified Text.PrettyPrint                          as Disp

versionTests :: [TestTree]
versionTests =
    -- test 'Version' type
    [ tp "versionNumbers . mkVersion = id @[NonNegative Int]"  prop_VersionId
    , tp "versionNumbers . mkVersion = id @Base.Version"       prop_VersionId2
    , tp "(==) = (==) `on` versionNumbers"                     prop_VersionEq
    , tp "(==) = (==) `on` mkVersion"                          prop_VersionEq2
    , tp "compare = compare `on` versionNumbers"               prop_VersionOrd
    , tp "compare = compare `on` mkVersion"                    prop_VersionOrd2

    , tp "readMaybe . show = Just"                             prop_ShowRead
    , tp "read example"                                        prop_ShowRead_example

    , tp "parsec . prettyShow involutive"                      prop_parsec_disp_inv

    , tp "normaliseVersionRange involutive"                    prop_normalise_inv
    , tp "normaliseVersionRange equivalent"                    prop_normalise_equiv
    , tp "normaliseVersionRange caretequiv"                    prop_normalise_caret_equiv
    , tp "normaliseVersionRange model"                         prop_normalise_model

    , tp "simplifyVersionRange involutive"                     prop_simplify_inv
    , tp "simplifyVersionRange equivalent"                     prop_simplify_equiv
    -- , tp "simplifyVersionRange caretequiv"                     prop_simplify_caret_equiv

    , tp "simpleParsec . prettyShow = Just" prop_parse_disp
    ]

    ++
    zipWith
    (\n (rep, p) -> testProperty ("Range Property " ++ show n ++ " (" ++ show rep ++ ")") p)
    [1::Int ..]
      -- properties to validate the test framework
    [ typProperty prop_nonNull
    , typProperty prop_gen_intervals1
    , typProperty prop_gen_intervals2

    , typProperty prop_anyVersion
    , typProperty prop_noVersion
    , typProperty prop_thisVersion
    , typProperty prop_notThisVersion
    , typProperty prop_laterVersion
    , typProperty prop_orLaterVersion
    , typProperty prop_earlierVersion
    , typProperty prop_orEarlierVersion
    , typProperty prop_unionVersionRanges
    , typProperty prop_intersectVersionRanges
    , typProperty prop_withinVersion
    , typProperty prop_foldVersionRange

      -- converting between version ranges and version intervals
    , typProperty prop_to_from_intervals
    ]
  where
    tp :: Testable p => String -> p -> TestTree
    tp = testProperty

    typProperty p = (typeOf p, property p)

-------------------------------------------------------------------------------
-- Arbitrary for inputs of mkVersion
-------------------------------------------------------------------------------

newtype VersionArb = VersionArb [Int]
                   deriving (Eq,Ord,Show)

-- | 'Version' instance as used by QC 2.9
instance Arbitrary VersionArb where
  arbitrary = sized $ \n ->
    do k <- choose (0, log2 n)
       xs <- vectorOf (k+1) arbitrarySizedNatural
       return (VersionArb xs)
    where
      log2 :: Int -> Int
      log2 n | n <= 1 = 0
             | otherwise = 1 + log2 (n `div` 2)

  shrink (VersionArb xs) =
    [ VersionArb xs'
    | xs' <- shrink xs
    , length xs' > 0
    , all (>=0) xs'
    ]

---------------------
-- Version properties
--

prop_VersionId :: [NonNegative Int] -> Bool
prop_VersionId lst0 =
    (versionNumbers . mkVersion) lst == lst
  where
    lst = map getNonNegative lst0

prop_VersionId2 :: VersionArb -> Bool
prop_VersionId2 (VersionArb lst) =
    (versionNumbers . mkVersion) lst == lst

prop_VersionEq :: Version -> Version -> Bool
prop_VersionEq v1 v2 = (==) v1 v2 == ((==) `on` versionNumbers) v1 v2

prop_VersionEq2 :: VersionArb -> VersionArb -> Bool
prop_VersionEq2 (VersionArb v1) (VersionArb v2) =
    (==) v1 v2 == ((==) `on` mkVersion) v1 v2

prop_VersionOrd :: Version -> Version -> Bool
prop_VersionOrd v1 v2 =
    compare v1 v2 == (compare `on` versionNumbers) v1 v2

prop_VersionOrd2 :: VersionArb -> VersionArb -> Bool
prop_VersionOrd2 (VersionArb v1) (VersionArb v2) =
    (==) v1 v2 == ((==) `on` mkVersion) v1 v2

prop_ShowRead :: Version -> Property
prop_ShowRead v = Just v === readMaybe (show v)

prop_ShowRead_example :: Bool
prop_ShowRead_example = show (mkVersion [1,2,3]) == "mkVersion [1,2,3]"

---------------------------
-- VersionRange properties
--

prop_normalise_inv :: VersionRange -> Property
prop_normalise_inv vr = normaliseVersionRange vr === normaliseVersionRange (normaliseVersionRange vr)

prop_normalise_equiv :: VersionRange -> Version -> Property
prop_normalise_equiv vr =
    prop_equivalentVersionRange vr (normaliseVersionRange vr)

prop_normalise_caret_equiv :: VersionRange -> Version -> Property
prop_normalise_caret_equiv vr = prop_equivalentVersionRange
    (transformCaretUpper vr)
    (transformCaretUpper (normaliseVersionRange vr))

prop_normalise_model :: VersionRange -> Property
prop_normalise_model vr =
    oldNormaliseVersionRange vr' === newNormaliseVersionRange vr'
  where
    vr' = transformCaret vr

    oldNormaliseVersionRange :: VersionRange -> VersionRange
    oldNormaliseVersionRange = Old.fromVersionIntervals . Old.toVersionIntervals

    newNormaliseVersionRange :: VersionRange -> VersionRange
    newNormaliseVersionRange = New.normaliseVersionRange2

prop_simplify_inv :: VersionRange -> Property
prop_simplify_inv vr =
    simplifyVersionRange vr === simplifyVersionRange (simplifyVersionRange vr)

prop_simplify_equiv :: VersionRange -> Version -> Property
prop_simplify_equiv vr v =
    counterexample (show vr') $ prop_equivalentVersionRange vr vr' v
  where
    vr' = simplifyVersionRange vr

-- TODO: Doesn't hold yet
-- prop_simplify_caret_equiv :: VersionRange -> Version -> Property
-- prop_simplify_caret_equiv vr = prop_equivalentVersionRange
--     (transformCaretUpper vr)
--     (transformCaretUpper (simplifyVersionRange vr))

prop_nonNull :: Version -> Bool
prop_nonNull = (/= nullVersion)

prop_anyVersion :: Version -> Bool
prop_anyVersion v' =
  withinRange v' anyVersion

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

prop_withinVersion :: Version -> Version -> Property
prop_withinVersion v v' =
    withinRange v' (withinVersion v)
    ===
    (v' >= v && v' < upper v)
  where
    upper = alterVersion $ \numbers -> case unsnoc numbers of
      Nothing      -> []
      Just (xs, x) -> xs ++ [x + 1]

prop_foldVersionRange :: VersionRange -> Property
prop_foldVersionRange range =
     expandVR range
  === foldVersionRange anyVersion thisVersion
                      laterVersion earlierVersion
                      unionVersionRanges intersectVersionRanges
                      range
  where
    expandVR (MajorBoundVersion v) =
        intersectVersionRanges (expandVR (orLaterVersion v)) (earlierVersion (majorUpperBound v))
    expandVR (OrEarlierVersion v) =
        unionVersionRanges (thisVersion v) (earlierVersion v)
    expandVR (OrLaterVersion v) =
        unionVersionRanges (thisVersion v) (laterVersion v)
    expandVR (UnionVersionRanges     v1 v2) =
      UnionVersionRanges (expandVR v1) (expandVR v2)
    expandVR (IntersectVersionRanges v1 v2) =
      IntersectVersionRanges (expandVR v1) (expandVR v2)
    expandVR v = v

    upper = alterVersion $ \numbers -> case unsnoc numbers of
      Nothing      -> []
      Just (xs, x) -> xs ++ [x + 1]

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
    allEqual (fromJust version) versions'
  where
    version       = isSpecificVersion range
    versions'     = filter (`withinRange` range) versions
    allEqual x xs = and (zipWith (==) (x:xs) xs)

prop_isSpecificVersion2 :: VersionRange -> Property
prop_isSpecificVersion2 range =
  isJust version ==>
    foldVersionRange Nothing Just (\_ -> Nothing) (\_ -> Nothing)
                     (\_ _ -> Nothing) (\_ _ -> Nothing)
      (simplifyVersionRange range)
    == version

  where
    version = isSpecificVersion range

-- | Check that our VersionIntervals' arbitrary instance generates intervals
-- that satisfies the invariant.
--
prop_gen_intervals1 :: VersionIntervals -> Property
prop_gen_intervals1 = property . New.invariantVersionIntervals

-- | Check that constructing our intervals type and converting it to a
-- 'VersionRange' and then into the true intervals type gives us back
-- the exact same sequence of intervals. This tells us that our arbitrary
-- instance for 'VersionIntervals'' is ok.
--
prop_gen_intervals2 :: VersionIntervals -> Property
prop_gen_intervals2 intervals =
    toVersionIntervals (fromVersionIntervals intervals) === intervals
--
-- | @'toVersionIntervals' . 'fromVersionIntervals'@ is an exact identity on
-- 'VersionIntervals'.
--
prop_to_from_intervals :: VersionIntervals -> Bool
prop_to_from_intervals intervals =
  toVersionIntervals (fromVersionIntervals intervals) == intervals

--------------------------------
-- equivalentVersionRange helper

prop_equivalentVersionRange
    :: VersionRange  -> VersionRange -> Version -> Property
prop_equivalentVersionRange range range' version =
    withinRange version range === withinRange version range'

--------------------------------
-- Parsing and pretty printing
--
prop_parsec_disp_inv :: VersionRange -> Property
prop_parsec_disp_inv vr =
    parseDisp vr === (parseDisp vr >>= parseDisp)
  where
    parseDisp = simpleParsec . prettyShow

prop_parse_disp :: VersionRange -> Property
prop_parse_disp vr = counterexample (show (prettyShow vr')) $
    fmap s (simpleParsec (prettyShow vr')) === Just vr'
  where
    -- we have to strip parens, because arbitrary 'VersionRange' may have
    -- too little parens constructors.
    s = stripParensVersionRange
    vr' = s vr

prop_parse_disp1 :: VersionRange -> Bool
prop_parse_disp1 vr =
    simpleParsec (prettyShow vr) == Just (normaliseVersionRange vr)

prop_parse_disp2 :: VersionRange -> Property
prop_parse_disp2 vr =
  let b = fmap (prettyShow :: VersionRange -> String) (simpleParsec (prettyShow vr))
      a = Just (prettyShow vr)
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp3 :: VersionRange -> Property
prop_parse_disp3 vr =
  let a = Just (prettyShow vr)
      b = fmap displayRaw (simpleParsec (prettyShow vr))
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp4 :: VersionRange -> Property
prop_parse_disp4 vr =
  let a = Just vr
      b = (simpleParsec (prettyShow vr))
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp5 :: VersionRange -> Property
prop_parse_disp5 vr =
  let a = Just vr
      b = simpleParsec (displayRaw vr)
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

displayRaw :: VersionRange -> String
displayRaw =
   Disp.render
 . cataVersionRange alg . normaliseVersionRange
  where

    -- precedence:
    -- All the same as the usual pretty printer, except for the parens
    alg (ThisVersionF v)                = Disp.text "==" <<>> pretty v
    alg (LaterVersionF v)               = Disp.char '>'  <<>> pretty v
    alg (EarlierVersionF v)             = Disp.char '<'  <<>> pretty v
    alg (OrLaterVersionF v)             = Disp.text ">=" <<>> pretty v
    alg (OrEarlierVersionF v)           = Disp.text "<=" <<>> pretty v
    alg (MajorBoundVersionF v)          = Disp.text "^>=" <<>> pretty v
    alg (UnionVersionRangesF r1 r2)     = r1 <+> Disp.text "||" <+> r2
    alg (IntersectVersionRangesF r1 r2) = r1 <+> Disp.text "&&" <+> r2

    dispWild v =
           Disp.hcat (Disp.punctuate (Disp.char '.')
                                     (map Disp.int (versionNumbers v)))
        <<>> Disp.text ".*"
