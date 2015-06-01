{-# LANGUAGE DisambiguateRecordFields #-}
module UnitTests.Distribution.Client.SourceTrees (
  tests
  ) where

import Distribution.Client.Types (SourcePackage(..))
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.Verbosity (silent)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Version as Version
import qualified Distribution.Client.Dependency.Types as Dependency.Types
import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Client.SourceTrees as SourceTree
import qualified Distribution.Client.Types as Types
import qualified Distribution.Package as Package
import qualified Distribution.PackageDescription as PackageDescription
import qualified Distribution.Simple.Utils as Utils
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

tests :: [TestTree]
tests = [
    testCase "readSourcePackagesInDir" readSourcePackagesInDirTest
  , testCase "constrainDepsToSourcePackages" constrainDepsToSourcePackagesTest
  ]

-- Test source package
name :: Package.PackageName
name = Package.PackageName "a"
ver :: Version.Version
ver = Version.Version { versionBranch = [1,2,3,4], versionTags = [] }
pkgid :: Package.PackageIdentifier
pkgid = Package.PackageIdentifier name ver

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
    assertBool "Package should exist in index." $
      PackageIndex.elemByPackageId index pkgid

constrainDepsToSourcePackagesTest :: Assertion
constrainDepsToSourcePackagesTest = do
  let path = "a/a.cabal"
      pkg =
        GenericPackageDescription {
          packageDescription = PackageDescription.emptyPackageDescription {
            package = pkgid
          },
          genPackageFlags = [],
          condLibrary = Nothing,
          condExecutables = [],
          condTestSuites = [],
          condBenchmarks = []
         }
      srcpkg =
        SourcePackage {
          packageInfoId = Package.packageId pkg,
          packageDescription = pkg,
          packageSource = Types.LocalUnpackedPackage
                          (FilePath.takeDirectory path),
          packageDescrOverride = Nothing
        }
      index    = PackageIndex.fromList [srcpkg]
      expected = [Dependency.Types.PackageConstraintSource name]
      actual   = SourceTree.constrainDepsToSourcePackages index
  assertEqual "Should constrain to source version." expected actual
