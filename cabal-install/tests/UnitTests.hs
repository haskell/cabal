module Main (main) where


import Test.Tasty

import qualified UnitTests.Distribution.Solver.Modular.Builder
import qualified UnitTests.Distribution.Solver.Modular.WeightedPSQ
import qualified UnitTests.Distribution.Solver.Modular.Solver
import qualified UnitTests.Distribution.Solver.Modular.RetryLog
import qualified UnitTests.Distribution.Solver.Types.OptionalStanza
import qualified UnitTests.Distribution.Client.BuildReport
import qualified UnitTests.Distribution.Client.Glob
import qualified UnitTests.Distribution.Client.GZipUtils
import qualified UnitTests.Distribution.Client.Store
import qualified UnitTests.Distribution.Client.Tar
import qualified UnitTests.Distribution.Client.Targets
import qualified UnitTests.Distribution.Client.UserConfig
import qualified UnitTests.Distribution.Client.ProjectConfig
import qualified UnitTests.Distribution.Client.JobControl
import qualified UnitTests.Distribution.Client.IndexUtils.Timestamp
import qualified UnitTests.Distribution.Client.Init
import qualified UnitTests.Distribution.Client.InstallPlan
import qualified UnitTests.Distribution.Client.Get


main :: IO ()
main =
  defaultMain $ testGroup "Unit Tests"
    [ testGroup "UnitTests.Distribution.Solver.Modular.Builder"
          UnitTests.Distribution.Solver.Modular.Builder.tests
    , testGroup "UnitTests.Distribution.Solver.Modular.WeightedPSQ"
          UnitTests.Distribution.Solver.Modular.WeightedPSQ.tests
    , testGroup "UnitTests.Distribution.Solver.Modular.Solver"
          UnitTests.Distribution.Solver.Modular.Solver.tests
    , testGroup "UnitTests.Distribution.Solver.Modular.RetryLog"
          UnitTests.Distribution.Solver.Modular.RetryLog.tests
    , UnitTests.Distribution.Solver.Types.OptionalStanza.tests
    , testGroup "UnitTests.Distribution.Client.Glob"
          UnitTests.Distribution.Client.Glob.tests
    , testGroup "Distribution.Client.GZipUtils"
        UnitTests.Distribution.Client.GZipUtils.tests
    , testGroup "Distribution.Client.Init"
        UnitTests.Distribution.Client.Init.tests
    , testGroup "Distribution.Client.Store"
        UnitTests.Distribution.Client.Store.tests
    , testGroup "Distribution.Client.Tar"
        UnitTests.Distribution.Client.Tar.tests
    , testGroup "Distribution.Client.Targets"
        UnitTests.Distribution.Client.Targets.tests
    , testGroup "UnitTests.Distribution.Client.UserConfig"
        UnitTests.Distribution.Client.UserConfig.tests
    , testGroup "UnitTests.Distribution.Client.ProjectConfig"
        UnitTests.Distribution.Client.ProjectConfig.tests
    , testGroup "UnitTests.Distribution.Client.JobControl"
        UnitTests.Distribution.Client.JobControl.tests
    , testGroup "UnitTests.Distribution.Client.IndexUtils.Timestamp"
        UnitTests.Distribution.Client.IndexUtils.Timestamp.tests
    , testGroup "UnitTests.Distribution.Client.InstallPlan"
        UnitTests.Distribution.Client.InstallPlan.tests
    , testGroup "UnitTests.Distribution.Client.Get"
        UnitTests.Distribution.Client.Get.tests
    , UnitTests.Distribution.Client.BuildReport.tests

    ]
