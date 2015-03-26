module Main
    ( main
    ) where

import Test.Tasty

import qualified UnitTests.Distribution.Compat.CreatePipe
import qualified UnitTests.Distribution.Compat.ReadP
import qualified UnitTests.Distribution.Simple.Program.Internal
import qualified UnitTests.Distribution.Utils.NubList

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
    ]

main :: IO ()
main = defaultMain tests
