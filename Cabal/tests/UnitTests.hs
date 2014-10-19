module Main
    ( main
    ) where

import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Test.Framework

import qualified UnitTests.Distribution.Compat.CreatePipe
import qualified UnitTests.Distribution.Compat.ReadP
import qualified UnitTests.Distribution.Utils.NubList

tests :: [Test]
tests =
    [ testGroup "Distribution.Compat.ReadP"
        UnitTests.Distribution.Compat.ReadP.tests
    , testGroup "Distribution.Compat.CreatePipe"
        UnitTests.Distribution.Compat.CreatePipe.tests
    , testGroup "Distribution.Utils.NubList"
        UnitTests.Distribution.Utils.NubList.tests
    ]

main :: IO ()
main = do
    -- WORKAROUND: disable buffering on stdout to get streaming test logs
    -- test providers _should_ do this themselves
    hSetBuffering stdout NoBuffering
    defaultMain tests
