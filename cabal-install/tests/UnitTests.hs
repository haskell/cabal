module Main
       where

import Test.Framework

import qualified UnitTests.Distribution.Client.Sandbox
import qualified UnitTests.Distribution.Client.Targets
import qualified UnitTests.Distribution.Client.Dependency.Modular.PSQ

tests :: [Test]
tests = [
   testGroup "Distribution.Client.Sandbox"
       UnitTests.Distribution.Client.Sandbox.tests
  ,testGroup "Distribution.Client.Targets"
       UnitTests.Distribution.Client.Targets.tests
  ,testGroup "UnitTests.Distribution.Client.Dependency.Modular.PSQ"
        UnitTests.Distribution.Client.Dependency.Modular.PSQ.tests
  ]

main :: IO ()
main = defaultMain tests
