module UnitTests.Distribution.Simple.Compiler
    ( tests
    ) where

import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Compiler
  ( DebugInfoLevel (..)
  , OptimisationLevel (..)
  , flagToDebugInfoLevel
  , flagToOptimisationLevel
  )

import qualified Control.Exception as C
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "flagToDebugInfoLevel" flagToDebugInfoLevelTests
  , testGroup "flagToOptimisationLevel" flagToOptimisationLevelTests
  , testGroup "DebugInfoLevel Parsec" debugInfoLevelParsecTests
  , testGroup "OptimisationLevel Parsec" optimisationLevelParsecTests
  ]

flagToDebugInfoLevelTests :: [TestTree]
flagToDebugInfoLevelTests =
  [ testCase "defaults to NormalDebugInfo" $
      flagToDebugInfoLevel Nothing @?= NormalDebugInfo
  , testCase "--enable-debug-info=True" $
      flagToDebugInfoLevel (Just "True") @?= NormalDebugInfo
  , testCase "--enable-debug-info=False" $
      flagToDebugInfoLevel (Just "False") @?= NoDebugInfo
  , testCase "--enable-debug-info=0" $
      flagToDebugInfoLevel (Just "0") @?= NoDebugInfo
  , testCase "--enable-debug-info=1" $
      flagToDebugInfoLevel (Just "1") @?= MinimalDebugInfo
  , testCase "--enable-debug-info=2" $
      flagToDebugInfoLevel (Just "2") @?= NormalDebugInfo
  , testCase "--enable-debug-info=3" $
      flagToDebugInfoLevel (Just "3") @?= MaximalDebugInfo
  , testCase "--enable-debug-info=4 is out of range" $
      assertError "expected an error for out-of-range debug info level" $
        flagToDebugInfoLevel (Just "4")
  , testCase "--enable-debug-info=foo is unparsable" $
      assertError "expected an error for unparsable debug info level" $
        flagToDebugInfoLevel (Just "foo")
  ]

flagToOptimisationLevelTests :: [TestTree]
flagToOptimisationLevelTests =
  [ testCase "defaults to NormalOptimisation" $
      flagToOptimisationLevel Nothing @?= NormalOptimisation
  , testCase "--enable-optimization=True" $
      flagToOptimisationLevel (Just "True") @?= NormalOptimisation
  , testCase "--enable-optimization=False" $
      flagToOptimisationLevel (Just "False") @?= NoOptimisation
  , testCase "--enable-optimization=0" $
      flagToOptimisationLevel (Just "0") @?= NoOptimisation
  , testCase "--enable-optimization=1" $
      flagToOptimisationLevel (Just "1") @?= NormalOptimisation
  , testCase "--enable-optimization=2" $
      flagToOptimisationLevel (Just "2") @?= MaximumOptimisation
  , testCase "--enable-optimization=3 is out of range" $
      assertError "expected an error for out-of-range optimisation level" $
        flagToOptimisationLevel (Just "3")
  , testCase "--enable-optimization=foo is unparsable" $
      assertError "expected an error for unparsable optimisation level" $
        flagToOptimisationLevel (Just "foo")
  ]

debugInfoLevelParsecTests :: [TestTree]
debugInfoLevelParsecTests =
  [ testCase "parses True" $
      eitherParsec "True" @?= (Right NormalDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses False" $
      eitherParsec "False" @?= (Right NoDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses 0" $
      eitherParsec "0" @?= (Right NoDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses 1" $
      eitherParsec "1" @?= (Right MinimalDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses 2" $
      eitherParsec "2" @?= (Right NormalDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses 3" $
      eitherParsec "3" @?= (Right MaximalDebugInfo :: Either String DebugInfoLevel)
  ]

optimisationLevelParsecTests :: [TestTree]
optimisationLevelParsecTests =
  [ testCase "parses True" $
      eitherParsec "True" @?= (Right NormalOptimisation :: Either String OptimisationLevel)
  , testCase "parses False" $
      eitherParsec "False" @?= (Right NoOptimisation :: Either String OptimisationLevel)
  , testCase "parses 0" $
      eitherParsec "0" @?= (Right NoOptimisation :: Either String OptimisationLevel)
  , testCase "parses 1" $
      eitherParsec "1" @?= (Right NormalOptimisation :: Either String OptimisationLevel)
  , testCase "parses 2" $
      eitherParsec "2" @?= (Right MaximumOptimisation :: Either String OptimisationLevel)
  ]

assertError :: String -> a -> Assertion
assertError msg x =
  C.catch
    (C.evaluate x >> assertFailure msg)
    (\(C.ErrorCall _) -> return ())
