module PackageTests.BuildDeps.InternalLibrary3.Check where

import qualified Data.ByteString.Char8 as C
import PackageTests.PackageTester
import Prelude hiding (catch)
import System.FilePath
import Test.HUnit


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary3") []
    let specTI = PackageSpec (directory spec </> "to-install") []

    unregister "InternalLibrary3"
    iResult <- cabal_install specTI                     
    assertInstallSucceeded iResult
    bResult <- cabal_build spec
    assertBuildSucceeded bResult
    unregister "InternalLibrary3"

    (_, _, output) <- run (Just $ directory spec) "dist/build/lemon/lemon" []
    C.appendFile (directory spec </> "test-log.txt") (C.pack $ "\ndist/build/lemon/lemon\n"++output)
    assertEqual "executable should have linked with the internal library" "myLibFunc internal" (concat $ lines output)

