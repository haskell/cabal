module UnitTests.Distribution.Client.BuildReport
  ( tests
  ) where

import Distribution.Client.Compat.Prelude
import UnitTests.Distribution.Client.ArbitraryInstances ()
import UnitTests.Distribution.Client.TreeDiffInstances ()
import Prelude ()

import Data.TreeDiff.QuickCheck (ediffEq)
import Test.QuickCheck (Property, counterexample)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

import Distribution.Client.BuildReports.Anonymous (BuildReport, parseBuildReport, showBuildReport)
import Distribution.Simple.Utils (toUTF8BS)

-- instances
import Test.QuickCheck.Instances.Cabal ()

tests :: [TestTree]
tests =
  [ testProperty "test" roundtrip
  ]

roundtrip :: BuildReport -> Property
roundtrip br =
  counterexample str $
    Right br `ediffEq` parseBuildReport (toUTF8BS str)
  where
    str :: String
    str = showBuildReport br
