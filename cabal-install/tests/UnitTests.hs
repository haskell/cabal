module Main
       where

import Test.Tasty
import Test.Tasty.Options

import qualified UnitTests.Distribution.Client.Compat.Time
import qualified UnitTests.Distribution.Client.Dependency.Modular.PSQ
import qualified UnitTests.Distribution.Client.Dependency.Modular.Solver
import qualified UnitTests.Distribution.Client.FileMonitor
import qualified UnitTests.Distribution.Client.GZipUtils
import qualified UnitTests.Distribution.Client.Sandbox
import qualified UnitTests.Distribution.Client.Sandbox.Timestamp
import qualified UnitTests.Distribution.Client.Tar
import qualified UnitTests.Distribution.Client.Targets
import qualified UnitTests.Distribution.Client.UserConfig

tests :: TestTree
tests = testGroup "Unit Tests"
  [ testGroup "UnitTests.Distribution.Client.Compat.Time"
        UnitTests.Distribution.Client.Compat.Time.tests
  , testGroup "UnitTests.Distribution.Client.Dependency.Modular.PSQ"
        UnitTests.Distribution.Client.Dependency.Modular.PSQ.tests
  , testGroup "UnitTests.Distribution.Client.Dependency.Modular.Solver"
        UnitTests.Distribution.Client.Dependency.Modular.Solver.tests
  , testGroup "UnitTests.Distribution.Client.FileMonitor"
        UnitTests.Distribution.Client.FileMonitor.tests
  , testGroup "Distribution.Client.GZipUtils"
       UnitTests.Distribution.Client.GZipUtils.tests
  , testGroup "Distribution.Client.Sandbox"
       UnitTests.Distribution.Client.Sandbox.tests
  , testGroup "Distribution.Client.Sandbox.Timestamp"
       UnitTests.Distribution.Client.Sandbox.Timestamp.tests
  , testGroup "Distribution.Client.Tar"
       UnitTests.Distribution.Client.Tar.tests
  , testGroup "Distribution.Client.Targets"
       UnitTests.Distribution.Client.Targets.tests
  , testGroup "UnitTests.Distribution.Client.UserConfig"
       UnitTests.Distribution.Client.UserConfig.tests
  ]

-- Extra options for running the test suite
extraOptions :: [OptionDescription]
extraOptions = concat [
    UnitTests.Distribution.Client.Dependency.Modular.Solver.options
  ]

main :: IO ()
main = defaultMainWithIngredients
         (includingOptions extraOptions : defaultIngredients)
         tests
