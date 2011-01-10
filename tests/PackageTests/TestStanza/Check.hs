module PackageTests.TestStanza.Check where

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

suite :: Version -> Test
suite cabalVersion = TestCase $ do
    let directory = "PackageTests" </> "TestStanza"
        pdFile = directory </> "my" <.> "cabal"
        spec = PackageSpec directory []
    result <- cabal_configure spec
    let message = "cabal configure should recognize test section"
        test = "unknown section type"
               `isInfixOf`
               (intercalate " " $ lines $ outputText result)
    assertEqual message False test
    genPD <- readPackageDescription silent pdFile
    let compiler = CompilerId GHC $ Version [6, 12, 2] []
        anyV = intersectVersionRanges anyVersion anyVersion
        anticipatedTestSuite = emptyTestSuite
            { testName = "dummy"
            , testInterface = TestSuiteExeV10 (Version [1,0] []) "dummy.hs"
            , testBuildInfo = emptyBuildInfo
                    { targetBuildDepends =
                            [ Dependency (PackageName "base") anyVersion ]
                    , hsSourceDirs = ["."]
                    }
            , testEnabled = False
            }
    case finalizePackageDescription [] (const True) buildPlatform compiler [] genPD of
        Left xs -> let depMessage = "should not have missing dependencies:\n" ++
                                    (unlines $ map (show . disp) xs)
                   in assertEqual depMessage True False
        Right (f, _) -> let gotTest = head $ testSuites f
                        in assertEqual "parsed test-suite stanza does not match anticipated"
                                gotTest anticipatedTestSuite
