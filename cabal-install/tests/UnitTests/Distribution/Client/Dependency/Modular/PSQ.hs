module UnitTests.Distribution.Client.Dependency.Modular.PSQ (
  tests
  ) where

import Distribution.Client.Dependency.Modular.PSQ

import Test.Framework                  as TF (Test)
import Test.Framework.Providers.HUnit  (testCase)
import Test.HUnit                      (Assertion, assertEqual)

tests :: [TF.Test]
tests = [ testCase "splitsAltImplementation" splitsTest
        ]

-- | Original splits implementation
splits' :: PSQ k a -> PSQ k (a, PSQ k a)
splits' xs =
  casePSQ xs
    (PSQ [])
    (\ k v ys -> cons k (v, ys) (fmap (\ (w, zs) -> (w, cons k v zs)) (splits' ys)))

splitsTest :: Assertion
splitsTest = do
  assertEqual "" (splits' psq1) (splits psq1)
  assertEqual "" (splits' psq2) (splits psq2)
  assertEqual "" (splits' psq3) (splits psq3)
  where
    psq1 = PSQ [ (1,2), (3,4), (5,6), (7,8) ] :: PSQ Int Int
    psq2 = PSQ [ (1,2) ] :: PSQ Int Int
    psq3 = PSQ [] :: PSQ Int Int
