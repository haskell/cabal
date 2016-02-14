module Main
    ( main
    ) where

import Test.Tasty

import qualified UnitTests.Distribution.Compat.CreatePipe
import qualified UnitTests.Distribution.Compat.ReadP
import qualified UnitTests.Distribution.Simple.Program.Internal
import qualified UnitTests.Distribution.Simple.Utils
import qualified UnitTests.Distribution.System
import qualified UnitTests.Distribution.Utils.NubList
import qualified UnitTests.Distribution.Version (versionTests)

tests :: TestTree
tests = testGroup "Unit Tests" $
    [ testGroup "Distribution.Compat.CreatePipe"
        UnitTests.Distribution.Compat.CreatePipe.tests
    , testGroup "Distribution.Compat.ReadP"
        UnitTests.Distribution.Compat.ReadP.tests
    , testGroup "Distribution.Simple.Program.Internal"
        UnitTests.Distribution.Simple.Program.Internal.tests
    , testGroup "Distribution.Simple.Utils"
        UnitTests.Distribution.Simple.Utils.tests
    , testGroup "Distribution.Utils.NubList"
        UnitTests.Distribution.Utils.NubList.tests
    , testGroup "Distribution.System"
        UnitTests.Distribution.System.tests
    , testGroup "Distribution.Version"
        UnitTests.Distribution.Version.versionTests
    ]

main :: IO ()
main = defaultMain tests
