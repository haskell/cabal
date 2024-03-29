{-# LANGUAGE CPP #-}
import Test.Cabal.Prelude

import Distribution.Version
import Distribution.Simple.LocalBuildInfo
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Types.UnqualComponentName
import Control.Monad.IO.Class
import Distribution.Simple.Configure
import Distribution.Utils.Path

main = setupAndCabalTest $ do
    assertOutputDoesNotContain "unknown section type"
        =<< setup' "configure" ["--enable-tests"]
    lbi <- getLocalBuildInfoM
    let gotTestSuite = head $ testSuites (localPkgDescr lbi)
    assertEqual "testName"  (mkUnqualComponentName "dummy")
                            (testName gotTestSuite)
    assertEqual "testInterface" (TestSuiteExeV10 (mkVersion [1,0])
#if MIN_VERSION_Cabal(3,11,0)
                                  $ makeRelativePathEx
#endif
                                  "dummy.hs")
                                (testInterface gotTestSuite)
    -- NB: Not testing targetBuildDepends (testBuildInfo gotTestSuite)
    -- as dependency varies with cabal-install
    assertEqual
        "testBuildInfo/hsSourceDirs"
        [sameDirectory]
        (hsSourceDirs (testBuildInfo gotTestSuite))
    return ()
