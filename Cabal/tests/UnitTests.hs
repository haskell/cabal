module Main
    ( main
    ) where

import Test.Framework

import qualified UnitTests.Distribution.Compat.ReadP
import qualified UnitTests.Distribution.Simple.JobControl

tests :: [Test]
tests =
    [ testGroup "Distribution.Compat.ReadP"
        UnitTests.Distribution.Compat.ReadP.tests
    , testGroup "Distribution.Simple.JobControl"
        UnitTests.Distribution.Simple.JobControl.tests
    ]

main :: IO ()
main = defaultMain tests
