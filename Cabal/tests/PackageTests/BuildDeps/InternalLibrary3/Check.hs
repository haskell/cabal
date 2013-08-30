module PackageTests.BuildDeps.InternalLibrary3.Check where

import qualified Data.ByteString.Char8 as C
import PackageTests.PackageTester
import System.FilePath
import Test.HUnit


suite :: FilePath -> FilePath -> Test
suite ghcPath ghcPkgPath = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary3") []
    let specTI = PackageSpec (directory spec </> "to-install") []

    unregister "InternalLibrary3" ghcPkgPath
    iResult <- cabal_install specTI ghcPath
    assertInstallSucceeded iResult
    bResult <- cabal_build spec ghcPath
    assertBuildSucceeded bResult
    unregister "InternalLibrary3"ghcPkgPath

    (_, _, output) <- run (Just $ directory spec) (directory spec </> "dist" </> "build" </> "lemon" </> "lemon") []
    C.appendFile (directory spec </> "test-log.txt") (C.pack $ "\ndist/build/lemon/lemon\n"++output)
    assertEqual "executable should have linked with the internal library" "myLibFunc internal" (concat $ lines output)

