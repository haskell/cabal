module UnitTests.Distribution.Parsec (tests) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Parsec
  ( explicitEitherParsec'
  , parsecOptCommaList
  , parsecToken
  )

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "parsecOptCommaList"
        [ testCase "trailing comma" $
            parse "a, b," @?= Right ["a", "b"]
        , testCase "trailing comma without spaces" $
            parse "a,b," @?= Right ["a", "b"]
        , testCase "no commas" $
            parse "a b" @?= Right ["a", "b"]
        , testCase "mixed commas" $
            parse "a, b c" @?= Right ["a", "b", "c"]
        , testCase "single item with trailing comma" $
            parse "a," @?= Right ["a"]
        , testCase "empty" $
            parse "" @?= Right []
        , testCase "leading comma is rejected" $
            case parse ", a" of
              Left _ -> pure ()
              Right xs -> assertFailure $ "unexpectedly parsed: " ++ show xs
        ]
    ]
  where
    parse :: String -> Either String [String]
    parse = explicitEitherParsec' CabalSpecV2_4 (parsecOptCommaList parsecToken)
