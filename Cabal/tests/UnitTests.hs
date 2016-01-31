module Main
    ( main
    ) where

import Test.Tasty

import qualified UnitTests.Distribution.Compat.CreatePipe
import qualified UnitTests.Distribution.Compat.ReadP
import qualified UnitTests.Distribution.Simple.Program.Internal
import qualified UnitTests.Distribution.Utils.NubList
import qualified UnitTests.Distribution.System
import qualified UnitTests.Distribution.Version (versionTests, parseTests)

tests :: TestTree
tests = testGroup "Unit Tests" $
    [ testGroup "Distribution.Compat.ReadP"
        UnitTests.Distribution.Compat.ReadP.tests
    , testGroup "Distribution.Compat.CreatePipe"
        UnitTests.Distribution.Compat.CreatePipe.tests
    , testGroup "Distribution.Simple.Program.Internal"
        UnitTests.Distribution.Simple.Program.Internal.tests
    , testGroup "Distribution.Utils.NubList"
        UnitTests.Distribution.Utils.NubList.tests
    , testGroup "Distribution.System"
        UnitTests.Distribution.System.tests
    , UnitTests.Distribution.Version.versionTests
    , UnitTests.Distribution.Version.parseTests
    ]

main :: IO ()
main = defaultMain tests
