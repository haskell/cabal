module UnitTests.Distribution.Client.BuildReport (
    tests,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()
import UnitTests.Distribution.Client.ArbitraryInstances ()
import UnitTests.Distribution.Client.TreeDiffInstances ()

import Data.TreeDiff.QuickCheck (ediffEq)
import Test.QuickCheck          (Property, counterexample)
import Test.Tasty               (TestTree, testGroup)
import Test.Tasty.QuickCheck    (testProperty)

import Distribution.Client.BuildReports.Anonymous (BuildReport, parseBuildReport, showBuildReport)
import Distribution.Simple.Utils                  (toUTF8BS)

-- instances
import Test.QuickCheck.Instances.Cabal ()

tests :: TestTree
tests = testGroup "BuildReport"
    [ testProperty "test" roundtrip
    ]

roundtrip :: BuildReport -> Property
roundtrip br =
    counterexample str $
    Right br `ediffEq` parseBuildReport (toUTF8BS str)
  where
    str :: String
    str = showBuildReport br
