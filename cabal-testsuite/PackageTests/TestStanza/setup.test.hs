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
    =<< setup' "configure" ["--enable-tests"]
  lbi <- getLocalBuildInfoM
  let gotTestSuite = head $ testSuites (localPkgDescr lbi)
  assertEqual
    "testName"
    (mkUnqualComponentName "dummy")
    (testName gotTestSuite)
  assertEqual
    "testInterface"
    (TestSuiteExeV10 (mkVersion [1, 0]) "dummy.hs")
    (testInterface gotTestSuite)
  -- NB: Not testing targetBuildDepends (testBuildInfo gotTestSuite)
  -- as dependency varies with cabal-install
  assertEqual
    "testBuildInfo/hsSourceDirs"
    [sameDirectory]
    (hsSourceDirs (testBuildInfo gotTestSuite))
  return ()
