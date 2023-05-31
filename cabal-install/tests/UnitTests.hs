module Main (main) where

import Test.Tasty

import qualified UnitTests.Distribution.Client.BuildReport
import qualified UnitTests.Distribution.Client.Configure
import qualified UnitTests.Distribution.Client.FetchUtils
import qualified UnitTests.Distribution.Client.GZipUtils
import qualified UnitTests.Distribution.Client.Get
import qualified UnitTests.Distribution.Client.Glob
import qualified UnitTests.Distribution.Client.IndexUtils
import qualified UnitTests.Distribution.Client.IndexUtils.Timestamp
import qualified UnitTests.Distribution.Client.Init
import qualified UnitTests.Distribution.Client.InstallPlan
import qualified UnitTests.Distribution.Client.JobControl
import qualified UnitTests.Distribution.Client.ProjectConfig
import qualified UnitTests.Distribution.Client.ProjectPlanning
import qualified UnitTests.Distribution.Client.Store
import qualified UnitTests.Distribution.Client.Tar
import qualified UnitTests.Distribution.Client.Targets
import qualified UnitTests.Distribution.Client.UserConfig
import qualified UnitTests.Distribution.Solver.Modular.Builder
import qualified UnitTests.Distribution.Solver.Modular.RetryLog
import qualified UnitTests.Distribution.Solver.Modular.Solver
import qualified UnitTests.Distribution.Solver.Modular.WeightedPSQ
import qualified UnitTests.Distribution.Solver.Types.OptionalStanza

main :: IO ()
main = do
  initTests <- UnitTests.Distribution.Client.Init.tests
  defaultMain $
    testGroup
      "Unit Tests"
      [ testGroup
          "UnitTests.Distribution.Client.BuildReport"
          UnitTests.Distribution.Client.BuildReport.tests
      , testGroup
          "UnitTests.Distribution.Client.Configure"
          UnitTests.Distribution.Client.Configure.tests
      , testGroup
          "UnitTests.Distribution.Client.FetchUtils"
          UnitTests.Distribution.Client.FetchUtils.tests
      , testGroup
          "UnitTests.Distribution.Client.Get"
          UnitTests.Distribution.Client.Get.tests
      , testGroup
          "UnitTests.Distribution.Client.Glob"
          UnitTests.Distribution.Client.Glob.tests
      , testGroup
          "Distribution.Client.GZipUtils"
          UnitTests.Distribution.Client.GZipUtils.tests
      , testGroup
          "UnitTests.Distribution.Client.IndexUtils"
          UnitTests.Distribution.Client.IndexUtils.tests
      , testGroup
          "UnitTests.Distribution.Client.IndexUtils.Timestamp"
          UnitTests.Distribution.Client.IndexUtils.Timestamp.tests
      , testGroup
          "Distribution.Client.Init"
          initTests
      , testGroup
          "UnitTests.Distribution.Client.InstallPlan"
          UnitTests.Distribution.Client.InstallPlan.tests
      , testGroup
          "UnitTests.Distribution.Client.JobControl"
          UnitTests.Distribution.Client.JobControl.tests
      , testGroup
          "UnitTests.Distribution.Client.ProjectConfig"
          UnitTests.Distribution.Client.ProjectConfig.tests
      , testGroup
          "UnitTests.Distribution.Client.ProjectPlanning"
          UnitTests.Distribution.Client.ProjectPlanning.tests
      , testGroup
          "Distribution.Client.Store"
          UnitTests.Distribution.Client.Store.tests
      , testGroup
          "Distribution.Client.Tar"
          UnitTests.Distribution.Client.Tar.tests
      , testGroup
          "Distribution.Client.Targets"
          UnitTests.Distribution.Client.Targets.tests
      , testGroup
          "UnitTests.Distribution.Client.UserConfig"
          UnitTests.Distribution.Client.UserConfig.tests
      , testGroup
          "UnitTests.Distribution.Solver.Modular.Builder"
          UnitTests.Distribution.Solver.Modular.Builder.tests
      , testGroup
          "UnitTests.Distribution.Solver.Modular.RetryLog"
          UnitTests.Distribution.Solver.Modular.RetryLog.tests
      , testGroup
          "UnitTests.Distribution.Solver.Modular.Solver"
          UnitTests.Distribution.Solver.Modular.Solver.tests
      , testGroup
          "UnitTests.Distribution.Solver.Modular.WeightedPSQ"
          UnitTests.Distribution.Solver.Modular.WeightedPSQ.tests
      , testGroup
          "UnitTests.Distribution.Solver.Types.OptionalStanza"
          UnitTests.Distribution.Solver.Types.OptionalStanza.tests
      ]
