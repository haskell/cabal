{-# LANGUAGE CPP #-}
-- isAlpha and isAlphaNum definitions change from base to base
#if MIN_VERSION_base(4,12,0) && !MIN_VERSION_base(4,13,0)
#define HAS_TESTS
#endif
module UnitTests.Distribution.Utils.CharSet where

import Data.Char        (isAlpha, isAlphaNum)
import Data.List        (foldl')
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Distribution.Utils.CharSet as CS

tests :: TestTree
tests = testGroup "Distribution.Utils.CharSet"
    [ testCase "alphanum" $
        CS.alphanum @?= foldl' (flip CS.insert) CS.empty
            [ c | c <- [ minBound .. maxBound ], isAlphaNum c ]

    , testCase "alpha" $
        CS.alpha @?= foldl' (flip CS.insert) CS.empty
            [ c | c <- [ minBound .. maxBound ], isAlpha c ]

    , testCase "alpha is subset of alphanum" $
        CS.union CS.alpha CS.alphanum @?= CS.alphanum
    ]
