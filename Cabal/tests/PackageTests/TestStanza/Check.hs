module PackageTests.TestStanza.Check where

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
    let anticipatedTestSuite = emptyTestSuite
            { testName = "dummy"
            , testInterface = TestSuiteExeV10 (Version [1,0] []) "dummy.hs"
            , testBuildInfo = emptyBuildInfo
                    { targetBuildDepends =
                            [ Dependency (PackageName "base") anyVersion ]
                    , hsSourceDirs = ["."]
                    }
            , testEnabled = False
            }
        gotTestSuite = head $ testSuites (localPkgDescr lbi)
    assertEqual "parsed test-suite stanza does not match anticipated"
                            anticipatedTestSuite gotTestSuite
    return ()
