module Main
       where

import Test.Framework

import qualified UnitTests.Distribution.Client.Sandbox
import qualified UnitTests.Distribution.Client.Targets

tests :: [Test]
tests = [
   testGroup "Distribution.Client.Sandbox"
       UnitTests.Distribution.Client.Sandbox.tests
  ,testGroup "Distribution.Client.Targets"
       UnitTests.Distribution.Client.Targets.tests
  ]

main :: IO ()
main = defaultMain tests
