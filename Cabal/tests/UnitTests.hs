module Main
    ( main
    ) where

import Test.Framework

import qualified UnitTests.Distribution.Compat.ReadP

tests :: [Test]
tests = [
    testGroup "Distribution.Compat.ReadP"
        UnitTests.Distribution.Compat.ReadP.tests
    ]

main :: IO ()
main = defaultMain tests
