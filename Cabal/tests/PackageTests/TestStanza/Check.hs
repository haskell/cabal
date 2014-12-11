module PackageTests.TestStanza.Check where

import Test.HUnit
import System.FilePath
import qualified  Data.Map as Map
import PackageTests.PackageTester
import Distribution.Version
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration
    (finalizePackageDescription)
import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.PackageDescription
    ( PackageDescription(..), BuildInfo(..), TestSuite(..)
    , TestSuiteInterface(..), emptyBuildInfo, emptyTestSuite
    , defaultRenaming)
import Distribution.Verbosity (silent)
import Distribution.System (buildPlatform)
import Distribution.Compiler
    ( CompilerId(..), CompilerFlavor(..), unknownCompilerInfo, AbiTag(..) )
import Distribution.Text

suite :: FilePath -> Test
suite ghcPath = TestCase $ do
    let dir = "PackageTests" </> "TestStanza"
        pdFile = dir </> "my" <.> "cabal"
        spec = PackageSpec dir []
    result <- cabal_configure spec ghcPath
    assertOutputDoesNotContain "unknown section type" result
    genPD <- readPackageDescription silent pdFile
    let compiler = unknownCompilerInfo (CompilerId GHC $ Version [6, 12, 2] []) NoAbiTag
        anticipatedTestSuite = emptyTestSuite
            { testName = "dummy"
            , testInterface = TestSuiteExeV10 (Version [1,0] []) "dummy.hs"
            , testBuildInfo = emptyBuildInfo
                    { targetBuildDepends =
                            [ Dependency (PackageName "base") anyVersion ]
                    , targetBuildRenaming =
                            Map.singleton (PackageName "base") defaultRenaming
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
