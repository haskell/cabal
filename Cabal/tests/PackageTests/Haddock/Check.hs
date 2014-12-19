module PackageTests.Haddock.Check (suite) where

import Control.Monad (unless, when)
import Data.List (isInfixOf)

import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Test.HUnit (Assertion, Test (TestCase), assertFailure)

import Distribution.Simple.Utils (withFileContents)
import PackageTests.PackageTester
    (PackageSpec(..), assertHaddockSucceeded, cabal_haddock)

this :: String
this = "Haddock"

suite :: FilePath -> Test
suite ghcPath = TestCase $ do
    let dir = "PackageTests" </> this
        haddocksDir = dir </> "dist" </> "doc" </> "html" </> "Haddock"
        spec = PackageSpec
            { directory = dir
            , configOpts = []
            , distPref = Nothing
            }

    haddocksDirExists <- doesDirectoryExist haddocksDir
    when haddocksDirExists (removeDirectoryRecursive haddocksDir)
    hResult <- cabal_haddock spec [] ghcPath
    assertHaddockSucceeded hResult

    let docFiles = map (haddocksDir </>)
                       ["CPP.html", "Literate.html", "NoCPP.html", "Simple.html"]
    mapM_ (assertFindInFile "For hiding needles.") docFiles

assertFindInFile :: String -> FilePath -> Assertion
assertFindInFile needle path =
    withFileContents path
                     (\contents ->
                      unless (needle `isInfixOf` contents)
                             (assertFailure ("expected: " ++ needle ++ "\n" ++
                                             " in file: " ++ path)))
