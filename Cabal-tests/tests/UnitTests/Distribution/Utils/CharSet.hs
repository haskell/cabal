module UnitTests.Distribution.Utils.CharSet where

import Prelude hiding (Foldable(..))
import Data.Char        (isAlpha, isAlphaNum)
import Data.Foldable    (foldl')
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
