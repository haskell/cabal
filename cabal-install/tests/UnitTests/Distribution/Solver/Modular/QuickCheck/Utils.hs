module UnitTests.Distribution.Solver.Modular.QuickCheck.Utils
  ( testPropertyWithSeed
  ) where

import Data.Tagged (Tagged, retag)
import System.Random (getStdRandom, random)

import Test.Tasty (TestTree)
import Test.Tasty.Options (OptionDescription, lookupOption, setOption)
import Test.Tasty.Providers (IsTest (..), singleTest)
import Test.Tasty.QuickCheck
  ( QC (..)
  , QuickCheckReplay (..)
  , Testable
  , property
  )

import Distribution.Simple.Utils
import Distribution.Verbosity

-- | Create a QuickCheck test that prints the seed before testing the property.
-- The seed can be useful for debugging non-terminating test cases. This is
-- related to https://github.com/feuerbach/tasty/issues/86.
testPropertyWithSeed :: Testable a => String -> a -> TestTree
testPropertyWithSeed name = singleTest name . QCWithSeed . QC . property

newtype QCWithSeed = QCWithSeed QC

instance IsTest QCWithSeed where
  testOptions = retag (testOptions :: Tagged QC [OptionDescription])

  run options (QCWithSeed test) progress = do
    replay <- case lookupOption options of
      QuickCheckReplay (Just override) -> return override
      QuickCheckReplay Nothing -> getStdRandom random
    notice normal $ "Using --quickcheck-replay=" ++ show replay
    run (setOption (QuickCheckReplay (Just replay)) options) test progress
