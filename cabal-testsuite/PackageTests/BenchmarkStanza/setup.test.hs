import Test.Cabal.Prelude

import Control.Monad.IO.Class
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path
import Distribution.Version

main = setupAndCabalTest $ do
  assertOutputDoesNotContain "unknown section type"
    =<< setup' "configure" ["--enable-benchmarks"]
  lbi <- getLocalBuildInfoM
  let gotBenchmark = head $ benchmarks (localPkgDescr lbi)
  assertEqual
    "benchmarkName"
    (mkUnqualComponentName "dummy")
    (benchmarkName gotBenchmark)
  assertEqual
    "benchmarkInterface"
    (BenchmarkExeV10 (mkVersion [1, 0]) "dummy.hs")
    (benchmarkInterface gotBenchmark)
  -- NB: Not testing targetBuildDepends (benchmarkBuildInfo gotBenchmark),
  -- as the dependency varies with cabal-install
  assertEqual
    "benchmarkBuildInfo/hsSourceDirs"
    [sameDirectory]
    (hsSourceDirs (benchmarkBuildInfo gotBenchmark))
  return ()
