module UnitTests.Distribution.Client.Dependency.Modular.PSQ (
  tests
  ) where

import Distribution.Client.Dependency.Modular.PSQ

import Test.Framework as TF (Test)
import Test.Framework.Providers.QuickCheck2

tests :: [TF.Test]
tests = [ testProperty "splitsAltImplementation" splitsTest
        ]

-- | Original splits implementation
splits' :: PSQ k a -> PSQ k (a, PSQ k a)
splits' xs =
  casePSQ xs
    (PSQ [])
    (\ k v ys -> cons k (v, ys) (fmap (\ (w, zs) -> (w, cons k v zs)) (splits' ys)))

splitsTest :: [(Int, Int)] -> Bool
splitsTest psq = splits' (PSQ psq) == splits (PSQ psq)
