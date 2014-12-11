module PackageTests.BenchmarkStanza.Check where

import Test.HUnit
import System.FilePath
import qualified Data.Map as Map
import PackageTests.PackageTester
import Distribution.Version
import Distribution.PackageDescription.Parse
        ( readPackageDescription )
import Distribution.PackageDescription.Configuration
        ( finalizePackageDescription )
import Distribution.Package
        ( PackageName(..), Dependency(..) )
import Distribution.PackageDescription
        ( PackageDescription(..), BuildInfo(..), Benchmark(..)
        , BenchmarkInterface(..)
        , emptyBuildInfo
        , emptyBenchmark, defaultRenaming )
import Distribution.Verbosity (silent)
import Distribution.System (buildPlatform)
import Distribution.Compiler
        ( CompilerId(..), CompilerFlavor(..), unknownCompilerInfo, AbiTag(..) )
import Distribution.Text

suite :: FilePath -> Test
suite ghcPath = TestCase $ do
    let dir = "PackageTests" </> "BenchmarkStanza"
        pdFile = dir </> "my" <.> "cabal"
        spec = PackageSpec dir []
    result <- cabal_configure spec ghcPath
    assertOutputDoesNotContain "unknown section type" result
    genPD <- readPackageDescription silent pdFile
    let compiler = unknownCompilerInfo (CompilerId GHC $ Version [6, 12, 2] []) NoAbiTag
        anticipatedBenchmark = emptyBenchmark
            { benchmarkName = "dummy"
            , benchmarkInterface = BenchmarkExeV10 (Version [1,0] []) "dummy.hs"
            , benchmarkBuildInfo = emptyBuildInfo
                    { targetBuildDepends =
                            [ Dependency (PackageName "base") anyVersion ]
                    , targetBuildRenaming =
                            Map.singleton (PackageName "base") defaultRenaming
                    , hsSourceDirs = ["."]
                    }
            , benchmarkEnabled = False
            }
    case finalizePackageDescription [] (const True) buildPlatform compiler [] genPD of
        Left xs -> let depMessage = "should not have missing dependencies:\n" ++
                                    (unlines $ map (show . disp) xs)
                   in assertEqual depMessage True False
        Right (f, _) -> let gotBenchmark = head $ benchmarks f
                        in assertEqual "parsed benchmark stanza does not match anticipated"
                                gotBenchmark anticipatedBenchmark
