{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.Distribution.Client.Described where

import Distribution.Client.Compat.Prelude
import Prelude ()
import UnitTests.Distribution.Client.ArbitraryInstances ()

import Data.Typeable         (typeOf)
import Test.QuickCheck       (Arbitrary (..), Gen, Property, choose, counterexample)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Distribution.FieldGrammar.Described (Described (..), Regex (..), reComma, reSpacedComma, reSpacedList)
import Distribution.Parsec                 (eitherParsec)
import Distribution.Pretty                 (prettyShow)

import qualified Distribution.Utils.CharSet as CS

import Distribution.Client.IndexUtils.Timestamp (IndexState, Timestamp)
import Distribution.Client.Types                (RepoName)

import qualified RERE         as RE
import qualified RERE.CharSet as RE

-- instances
import Test.QuickCheck.Instances.Cabal ()

tests :: TestTree
tests = testGroup "Described"
    [ testDescribed (Proxy :: Proxy Timestamp)
    , testDescribed (Proxy :: Proxy IndexState)
    , testDescribed (Proxy :: Proxy RepoName)
    ]

-------------------------------------------------------------------------------
-- Described/Pretty/Parsec tests
-------------------------------------------------------------------------------

testDescribed
    :: forall a. (Arbitrary a, Described a, Typeable a, Eq a, Show a)
    => Proxy a
    -> TestTree
testDescribed _ = testGroup name
    [ testProperty "parsec" propParsec
    , testProperty "pretty" propPretty
    , testProperty "roundtrip" propRoundtrip
    ]
  where
    name = show (typeOf (undefined :: a))

    propParsec :: Ex a -> Property
    propParsec (Example str) = counterexample (show res) $ case res of
        Right _ -> True
        Left _  -> False
      where
        res :: Either String a
        res = eitherParsec str

    rr :: RE.RE Void
    rr = convert $ describe (Proxy :: Proxy a)

    propPretty :: a -> Property
    propPretty x = counterexample str $ RE.matchR rr str
      where
        str = prettyShow x

    propRoundtrip :: a -> Property
    propRoundtrip x = counterexample (show (res, str)) $ case res of

        Right y -> x == y
        Left _  -> False
      where
        str = prettyShow x
        res = eitherParsec str

newtype Ex a = Example String
  deriving (Show)

instance Described a => Arbitrary (Ex a) where
    arbitrary
        = fmap Example
        $ fromMaybe (return "")
        $ RE.generate 10 5
        $ convert $ describe (Proxy :: Proxy a)

    shrink (Example s)
        | '\n' `elem` s = [ Example $ map (\c -> if c == '\n' then ' ' else c) s ]
        | otherwise     = []

genInt :: Int -> Int -> Gen Int
genInt lo hi = choose (lo, hi)

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

convert :: Regex Void -> RE.RE Void
convert = go id . vacuous where
    go :: Ord b => (a -> b) -> Regex a -> RE.RE b
    go f (REAppend rs)      = foldr (\r acc -> go f r <> acc) RE.Eps rs
    go f (REUnion rs)       = foldr (\r acc -> go f r RE.\/ acc) RE.Null rs
    go _ (RECharSet cs)     = RE.Ch (convertCS cs)
    go _ (REString str)     = RE.string_ str

    go f (REMunch sep r)    = RE.Eps RE.\/ r' <> RE.star_ (sep' <> r') where
        sep' = go f sep
        r'   = go f r
    go f (REMunch1 sep r)   = r' <> RE.star_ (sep' <> r') where
        sep' = go f sep
        r'   = go f r
    go f (REMunchR n sep r)
        | n <= 0    = RE.Eps
        | otherwise = RE.Eps RE.\/ r' <> go' (pred n)
      where
        sep' = go f sep
        r'   = go f r

        go' m | m <= 0    = RE.Eps
              | otherwise = RE.Eps RE.\/ sep' <> r' <> go' (pred m)

    go f (REOpt r)          = RE.Eps RE.\/ go f r

    go f (REVar a)          = RE.Var (f a)
    go f (RENamed _ r)      = go f r
    go f (RERec n r)        = RE.fix_ (fromString n)
        (go (maybe RE.B (RE.F . f)) r)

    go _ RESpaces           = RE.Eps RE.\/ RE.ch_ ' ' RE.\/ "  " RE.\/ "\n"
    go _ RESpaces1          = RE.ch_ ' ' RE.\/ "  " RE.\/ "\n"

    go f (RECommaList r)    = go f (expandedCommaList r)
    go f (REOptCommaList r) = go f (expandedOptCommaList r)

    go _ RETodo             = RE.Null

expandedCommaList :: Regex a -> Regex a
expandedCommaList = REUnion . expandedCommaList'

expandedCommaList' :: Regex a -> [Regex a]
expandedCommaList' r =
    [ REMunch reSpacedComma r
    , reComma <> RESpaces <> REMunch1 reSpacedComma r
    , REMunch1 reSpacedComma r <> RESpaces <> reComma
    ]

expandedOptCommaList :: Regex a -> Regex a
expandedOptCommaList r = REUnion $ reSpacedList r : expandedCommaList' r

convertCS :: CS.CharSet -> RE.CharSet
convertCS = RE.fromIntervalList . CS.toIntervalList
