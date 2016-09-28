module PackageTests.BenchmarkStanza.Check where

import PackageTests.PackageTester

import Distribution.Version
import Distribution.Simple.LocalBuildInfo
import Distribution.Package
import Distribution.PackageDescription

suite :: TestM ()
suite = do
    assertOutputDoesNotContain "unknown section type"
        =<< cabal' "configure" []
    dist_dir <- distDir
    lbi <- liftIO $ getPersistBuildConfig dist_dir
    let anticipatedBenchmark = emptyBenchmark
            { benchmarkName = "dummy"
            , benchmarkInterface = BenchmarkExeV10 (mkVersion [1,0])
                                                   "dummy.hs"
            , benchmarkBuildInfo = emptyBuildInfo
                    { targetBuildDepends =
                            [ Dependency (mkPackageName "base") anyVersion ]
                    , hsSourceDirs = ["."]
                    }
            }
        gotBenchmark = head $ benchmarks (localPkgDescr lbi)
    assertEqual "parsed benchmark stanza does not match anticipated"
                            anticipatedBenchmark gotBenchmark
    return ()
