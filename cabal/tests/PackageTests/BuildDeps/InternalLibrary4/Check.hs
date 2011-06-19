module PackageTests.BuildDeps.InternalLibrary4.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath
import qualified Data.ByteString.Char8 as C


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary4") []
    let specTI = PackageSpec (directory spec </> "to-install") []

    unregister "InternalLibrary4"
    iResult <- cabal_install specTI                     
    assertEqual "cabal install should succeed - see to-install/test-log.txt" True (successful iResult)
    bResult <- cabal_build spec
    assertEqual "cabal build should succeed - see test-log.txt" True (successful bResult)
    unregister "InternalLibrary4"

    (_, _, output) <- run (Just $ directory spec) "dist/build/lemon/lemon" []
    C.appendFile (directory spec </> "test-log.txt") (C.pack $ "\ndist/build/lemon/lemon\n"++output)
    assertEqual "executable should have linked with the installed library" "myLibFunc installed" (concat $ lines output)

