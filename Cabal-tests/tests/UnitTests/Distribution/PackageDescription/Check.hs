-- For the deprecated import of Distribution.Compat.Prelude.Internal
{-# OPTIONS_GHC -Wwarn=deprecations #-}

module UnitTests.Distribution.PackageDescription.Check (tests) where

import Distribution.Compat.Prelude.Internal
import Prelude ()

import Distribution.PackageDescription.Check

import Test.Tasty
import Test.Tasty.HUnit

-- instances
import Test.QuickCheck.Instances.Cabal ()


tests :: [TestTree]
tests =
    [ testCase "Unique ignore strings" (uniqueNames @?= True)
    , testCase "Short ignore identifiers" (longerThan @?= [])
    , testCase "Parsimonious '-' use" (usingTooManyDashes @?= [])
    ]
  where
    allExplanationIdStrings :: [CheckExplanationIDString]
    allExplanationIdStrings = map ppCheckExplanationId [minBound..maxBound]

    uniqueNames :: Bool
    uniqueNames = length allExplanationIdStrings == length (nub allExplanationIdStrings)

    longerThan :: [CheckExplanationIDString]
    longerThan = filter ((>25). length) allExplanationIdStrings

    usingTooManyDashes :: [CheckExplanationIDString]
    usingTooManyDashes = filter ((>2) . length . filter (=='-'))
                           allExplanationIdStrings

