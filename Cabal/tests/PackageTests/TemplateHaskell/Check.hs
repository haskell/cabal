module PackageTests.TemplateHaskell.Check where

import Test.HUnit
import System.FilePath
import PackageTests.PackageTester
import Data.List (isInfixOf, intercalate)
import Distribution.Version
import Distribution.PackageDescription.Parse
        ( readPackageDescription )
import Distribution.PackageDescription.Configuration
        ( finalizePackageDescription )
import Distribution.Package
        ( PackageIdentifier(..), PackageName(..), Dependency(..) )
import Distribution.PackageDescription
        ( PackageDescription(..), BuildInfo(..), TestSuite(..), Library(..)
        , TestSuiteInterface(..)
        , TestType(..), emptyPackageDescription, emptyBuildInfo, emptyLibrary
        , emptyTestSuite, BuildType(..) )
import Distribution.Verbosity (silent)
import Distribution.License (License(..))
import Distribution.ModuleName (fromString)
import Distribution.System (buildPlatform)
import Distribution.Compiler
        ( CompilerId(..), CompilerFlavor(..) )
import Distribution.Text

profiling :: Test
profiling = TestCase $ do
   let flags = ["--disable-library-vanilla"
               ,"--enable-library-profiling"
               ,"--enable-executable-profiling"]
       spec = PackageSpec ("PackageTests" </> "TemplateHaskell" </> "profiling") flags
   result <- cabal_build spec
   assertEqual "cabal build should succeed - see test-log.txt" True (successful result)

dynamic :: Test
dynamic = TestCase $ do
    let flags = ["--disable-library-vanilla"
                ,"--enable-shared"
                ,"--enable-executable-dynamic"]
        spec = PackageSpec ("PackageTests" </> "TemplateHaskell" </> "dynamic") flags
    result <- cabal_build spec
    assertEqual "cabal build should succeed - see test-log.txt" True (successful result)

