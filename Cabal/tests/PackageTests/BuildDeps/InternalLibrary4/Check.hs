module PackageTests.BuildDeps.InternalLibrary4.Check where

import qualified Data.ByteString.Char8 as C
import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit


suite :: SuiteConfig -> Assertion
suite config = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BuildDeps" </> "InternalLibrary4"
            , configOpts = []
            , distPref = Nothing
            }
    let specTI = PackageSpec
            { directory = directory spec </> "to-install"
            , configOpts = []
            , distPref = Nothing
            }

    unregister config "InternalLibrary4"
    iResult <- cabal_install config specTI
    assertInstallSucceeded iResult
    bResult <- cabal_build config spec
    assertBuildSucceeded bResult
    unregister config "InternalLibrary4"

    (_, _, output) <- run (Just $ directory spec) (directory spec </> "dist" </> "build" </> "lemon" </> "lemon") [] []
    C.appendFile (directory spec </> "test-log.txt") (C.pack $ "\ndist/build/lemon/lemon\n"++output)
    assertEqual "executable should have linked with the installed library" "myLibFunc installed" (concat $ lines output)
