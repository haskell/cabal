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
        anticipatedFinalPD = emptyPackageDescription
                { package = PackageIdentifier
                        { pkgName = PackageName "TestStanza"
                        , pkgVersion = Version [0, 1] []
                        }
                , license = BSD3
                , author = "Thomas Tuegel"
                , stability = "stable"
                , description = "Check that Cabal recognizes the Test stanza defined below."
                , category = "PackageTests"
                , descCabalVersion = anyVersion
                , buildType = Just Simple
                , buildDepends =
                    [ Dependency (PackageName "base") anyV ]
                , library = Just emptyLibrary
                        { exposedModules = [fromString "MyLibrary"]
                        , libBuildInfo = emptyBuildInfo
                                { targetBuildDepends =
                                        [ Dependency (PackageName "base") anyVersion ]
                                , hsSourceDirs = ["."]
                                }
                        }
                , testSuites = [ emptyTestSuite
                        { testName = "dummy"
                        , mainIs = Just "dummy.hs"
                        , testType = ExeTest $ Version [1,0] []
                        , testModule = Nothing
                        , testBuildInfo = emptyBuildInfo
                                { targetBuildDepends =
                                        [ Dependency (PackageName "base") anyVersion ]
                                , hsSourceDirs = ["."]
                                }
                        }
                                ]
                }
    case finalizePackageDescription [] (const True) buildPlatform compiler [] genPD of
        Left xs -> let depMessage = "should not have missing dependencies:\n" ++
                                    (unlines $ map (show . disp) xs)
                   in assertEqual depMessage True False
        Right (f, _) -> assertEqual "parsed package description does not match anticipated"
                                f anticipatedFinalPD