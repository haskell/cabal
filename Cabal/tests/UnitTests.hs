module Main
    ( main
    ) where

import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Test.Framework

import qualified UnitTests.Distribution.Compat.ReadP

tests :: [Test]
tests = [
    testGroup "Distribution.Compat.ReadP"
        UnitTests.Distribution.Compat.ReadP.tests
    ]

main :: IO ()
main = do
    -- WORKAROUND: disable buffering on stdout to get streaming test logs
    -- test providers _should_ do this themselves
    hSetBuffering stdout NoBuffering
    defaultMain tests
