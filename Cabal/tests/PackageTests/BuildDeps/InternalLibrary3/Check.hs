module PackageTests.BuildDeps.InternalLibrary3.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath
import qualified Data.ByteString.Char8 as C
import Control.Exception
import Prelude hiding (catch)


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary3") []
    let specTI = PackageSpec (directory spec </> "to-install") []

    unregister "InternalLibrary3"
    iResult <- cabal_install specTI                     
    do
        assertEqual "cabal install should succeed - see to-install/test-log.txt" True (successful iResult)
      `catch` \exc -> do
        putStrLn $ "Cabal result was "++show iResult
        throwIO (exc :: SomeException)
    bResult <- cabal_build spec
    do
        assertEqual "cabal build should succeed - see test-log.txt" True (successful bResult)
      `catch` \exc -> do
        putStrLn $ "Cabal result was "++show bResult
        throwIO (exc :: SomeException)
    unregister "InternalLibrary3"

    (_, _, output) <- run (Just $ directory spec) "dist/build/lemon/lemon" []
    C.appendFile (directory spec </> "test-log.txt") (C.pack $ "\ndist/build/lemon/lemon\n"++output)
    assertEqual "executable should have linked with the internal library" "myLibFunc internal" (concat $ lines output)

