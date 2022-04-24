module UnitTests.Distribution.Solver.Modular.MessageUtils ( tests ) where

import Distribution.Solver.Modular.MessageUtils
    (allKnownExtensions, cutoffRange, withinRange, mostSimilarElement)
import Language.Haskell.Extension (knownLanguages)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests = testProperty "the equal string is always the closest" propEqualStringClosest : assertionTests

-- The equal string will always be the most similar element
propEqualStringClosest :: String -> Bool
propEqualStringClosest str = mostSimilarElement str [str] == str

assertionTests :: [TestTree]
assertionTests = map (testCase "assert equals") (extensionAssertions ++ languageAssertions) ++ map (testCase "assert truthy") rangeAssertions

extensionAssertions :: [Assertion]
extensionAssertions = map (`testClosest` extensionStrings) shouldSuggestExtension

languageAssertions :: [Assertion]
languageAssertions = map (`testClosest` languageStrings) shouldSuggestLanguage

testClosest :: (String, String) -> [String] -> Assertion
testClosest (misspelled, closestMatch) elems = assertEqual "Strings should match" closestMatch (mostSimilarElement misspelled elems)

extensionStrings :: [String]
extensionStrings = allKnownExtensions

languageStrings :: [String]
languageStrings = show <$> knownLanguages

-- Given x misspelled extension should suggest y extension
shouldSuggestExtension :: [(String, String)]
shouldSuggestExtension =
  [ ("FlexibleConstraints", "FlexibleContexts")
  , ("FlexibleInstantiation", "FlexibleInstances")
  , ("GATs", "GADTs")
  , ("MultiTypeClass", "MultiParamTypeClasses")
  , ("NoMonoLoclBinds", "NoMonoLocalBinds")
  , ("NoLamdaCase", "NoLambdaCase")
  ]

-- Given x misspelled language should suggest y language
shouldSuggestLanguage :: [(String, String)]
shouldSuggestLanguage =
  [ ("GHC2020", "GHC2021")
  , ("Haskell2011", "Haskell2010")
  , ("Hugs98", "Haskell98")
  ]

rangeAssertions :: [Assertion]
rangeAssertions = map (testRange cutoffRange extensionStrings) outOfBounds

isOutOfBounds :: Int -> String -> String -> Bool
isOutOfBounds range a b = not $ withinRange range a b

testRange :: Int -> [String] -> String ->  Assertion
testRange range elems erronousElement = assertBool "String should be out of bounds to make a spelling suggestion" (isOutOfBounds range erronousElement suggestion)
  where
    suggestion = mostSimilarElement erronousElement elems

outOfBounds :: [String]
outOfBounds =
  [ "HopefullyThisExtensionWontOccur"
  , "ThisIsNotEvenRemotelyAnExtension"
  , "IsThisMaybeAnExtension"
  ]
