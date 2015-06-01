module UnitTests.Distribution.Client.SourceTrees (
  tests
  ) where

import Data.Version (makeVersion)
import Distribution.Verbosity (silent)
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath ((</>))

import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Client.SourceTrees as SourceTree
import qualified Distribution.Package as Package
import qualified Distribution.Simple.Utils as Utils
import qualified System.Directory as Directory

tests :: [TestTree]
tests = [ testCase "readSourcePackagesInDir" readSourcePackagesInDirTest
        ]

readSourcePackagesInDirTest :: Assertion
readSourcePackagesInDirTest = do
  sysTmpDir <- Directory.getTemporaryDirectory
  Utils.withTempDirectory silent sysTmpDir "cabal-test" $ \ tmpDir -> do
    let pkgDir = tmpDir </> "a"
    Directory.createDirectory pkgDir
    -- Write a trivial cabal file.
    writeFile (pkgDir </> "a.cabal") $ unlines [
      "name: a",
      "version: 1.2.3.4"
      ]
    index <- SourceTree.readSourcePackagesInDir silent tmpDir
    let name  = Package.PackageName "a"
        ver   = makeVersion [1,2,3,4]
        pkgid = Package.PackageIdentifier name ver
    assertBool "Package should exist in index." $
      PackageIndex.elemByPackageId index pkgid
