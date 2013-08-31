module Main
    ( main
    ) where

import Test.Framework

import qualified UnitTests.Distribution.Compat.ReadP
import qualified UnitTests.Distribution.Simple.InstallDirs

tests :: [Test]
tests = [
    testGroup "Distribution.Compat.ReadP"
        UnitTests.Distribution.Compat.ReadP.tests,
    testGroup "Distribution.Simple.InstallDirs"
        UnitTests.Distribution.Simple.InstallDirs.tests
    ]

main :: IO ()
main = defaultMain tests
