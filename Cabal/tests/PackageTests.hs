-- The intention is that this will be the new unit test framework.
-- Please add any working tests here.  This file should do nothing
-- but import tests from other modules.
--
-- Stephen Blackheath, 2009

module Main where

import Data.Version (Version(Version))
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Text (display)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import Test.Framework (Test, TestName, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified Test.HUnit as HUnit

import PackageTests.BenchmarkExeV10.Check
import PackageTests.BenchmarkOptions.Check
import PackageTests.BenchmarkStanza.Check
-- import PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check
-- import PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check
import PackageTests.BuildDeps.InternalLibrary0.Check
import PackageTests.BuildDeps.InternalLibrary1.Check
import PackageTests.BuildDeps.InternalLibrary2.Check
import PackageTests.BuildDeps.InternalLibrary3.Check
import PackageTests.BuildDeps.InternalLibrary4.Check
import PackageTests.BuildDeps.SameDepsAllRound.Check
import PackageTests.BuildDeps.TargetSpecificDeps1.Check
import PackageTests.BuildDeps.TargetSpecificDeps2.Check
import PackageTests.BuildDeps.TargetSpecificDeps3.Check
import PackageTests.BuildTestSuiteDetailedV09.Check
import PackageTests.PackageTester (PackageSpec(..), compileSetup)
import PackageTests.PathsModule.Executable.Check
import PackageTests.PathsModule.Library.Check
import PackageTests.PreProcess.Check
import PackageTests.TemplateHaskell.Check
import PackageTests.CMain.Check
import PackageTests.EmptyLib.Check
import PackageTests.TestOptions.Check
import PackageTests.TestStanza.Check
import PackageTests.TestSuiteExeV10.Check
import PackageTests.OrderFlags.Check

hunit :: TestName -> HUnit.Test -> Test
hunit name test = testGroup name $ hUnitTestToTests test

tests :: Version -> PackageSpec -> [Test]
tests version inplaceSpec =
    [ hunit "BuildDeps/SameDepsAllRound"
      PackageTests.BuildDeps.SameDepsAllRound.Check.suite
      -- The two following tests were disabled by Johan Tibell as
      -- they have been failing for a long time:
      -- , hunit "BuildDeps/GlobalBuildDepsNotAdditive1/"
      --   PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check.suite
      -- , hunit "BuildDeps/GlobalBuildDepsNotAdditive2/"
      --   PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check.suite
    , hunit "BuildDeps/InternalLibrary0"
      (PackageTests.BuildDeps.InternalLibrary0.Check.suite version)
    , hunit "PreProcess" PackageTests.PreProcess.Check.suite
    , hunit "TestStanza" PackageTests.TestStanza.Check.suite
      -- ^ The Test stanza test will eventually be required
      -- only for higher versions.
    , hunit "TestSuiteExeV10/Test" PackageTests.TestSuiteExeV10.Check.checkTest
    , hunit "TestSuiteExeV10/TestWithHpc"
      PackageTests.TestSuiteExeV10.Check.checkTestWithHpc
    , hunit "TestOptions" PackageTests.TestOptions.Check.suite
    , hunit "BenchmarkStanza" PackageTests.BenchmarkStanza.Check.suite
      -- ^ The benchmark stanza test will eventually be required
      -- only for higher versions.
    , hunit "BenchmarkExeV10/Test"
      PackageTests.BenchmarkExeV10.Check.checkBenchmark
    , hunit "BenchmarkOptions" PackageTests.BenchmarkOptions.Check.suite
    , hunit "TemplateHaskell/vanilla"
      PackageTests.TemplateHaskell.Check.vanilla
    , hunit "TemplateHaskell/profiling"
      PackageTests.TemplateHaskell.Check.profiling
    , hunit "TemplateHaskell/dynamic"
      PackageTests.TemplateHaskell.Check.dynamic
    , hunit "PathsModule/Executable"
      PackageTests.PathsModule.Executable.Check.suite
    , hunit "PathsModule/Library" PackageTests.PathsModule.Library.Check.suite
    , hunit "EmptyLib/emptyLib"
      PackageTests.EmptyLib.Check.emptyLib
    , hunit "BuildTestSuiteDetailedV09"
      $ PackageTests.BuildTestSuiteDetailedV09.Check.suite inplaceSpec
    , hunit "OrderFlags"
      PackageTests.OrderFlags.Check.suite
    ] ++
    -- These tests are only required to pass on cabal version >= 1.7
    (if version >= Version [1, 7] []
     then [ hunit "BuildDeps/TargetSpecificDeps1"
            PackageTests.BuildDeps.TargetSpecificDeps1.Check.suite
          , hunit "BuildDeps/TargetSpecificDeps2"
            PackageTests.BuildDeps.TargetSpecificDeps2.Check.suite
          , hunit "BuildDeps/TargetSpecificDeps3"
            PackageTests.BuildDeps.TargetSpecificDeps3.Check.suite
          , hunit "BuildDeps/InternalLibrary1"
            PackageTests.BuildDeps.InternalLibrary1.Check.suite
          , hunit "BuildDeps/InternalLibrary2"
            PackageTests.BuildDeps.InternalLibrary2.Check.suite
          , hunit "BuildDeps/InternalLibrary3"
            PackageTests.BuildDeps.InternalLibrary3.Check.suite
          , hunit "BuildDeps/InternalLibrary4"
            PackageTests.BuildDeps.InternalLibrary4.Check.suite
          , hunit "PackageTests/CMain"
            PackageTests.CMain.Check.checkBuild
          ]
     else [])

main :: IO ()
main = do
    wd <- getCurrentDirectory
    let dbFile = wd </> "dist/package.conf.inplace"
        inplaceSpec = PackageSpec
            { directory = []
            , configOpts = [ "--package-db=" ++ dbFile
                           , "--constraint=Cabal == " ++ display cabalVersion
                           ]
            }
    putStrLn $ "Cabal test suite - testing cabal version " ++
        display cabalVersion
    setCurrentDirectory "tests"
    -- Create a shared Setup executable to speed up Simple tests
    compileSetup "."
    defaultMain (tests cabalVersion inplaceSpec)
