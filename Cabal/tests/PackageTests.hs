-- The intention is that this will be the new unit test framework.
-- Please add any working tests here.  This file should do nothing
-- but import tests from other modules.
--
-- Stephen Blackheath, 2009

module Main where

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
import PackageTests.DeterministicAr.Check
import PackageTests.EmptyLib.Check
import PackageTests.TestOptions.Check
import PackageTests.TestStanza.Check
import PackageTests.TestSuiteExeV10.Check
import PackageTests.OrderFlags.Check

import Distribution.Compat.Exception (catchIO)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.Types (programPath)
import Distribution.Simple.Program.Builtin (ghcProgram, ghcPkgProgram)
import Distribution.Simple.Program.Db (requireProgram)
import Distribution.Simple.Utils (cabalVersion, die, withFileContents)
import Distribution.Text (display)
import Distribution.Verbosity (normal)
import Distribution.Version (Version(Version))

import Data.Maybe (isJust)
import System.Directory (doesFileExist, getCurrentDirectory,
                         setCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Test.Framework (Test, TestName, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified Test.HUnit as HUnit


hunit :: TestName -> HUnit.Test -> Test
hunit name test = testGroup name $ hUnitTestToTests test

tests :: Version -> PackageSpec -> FilePath -> FilePath -> [Test]
tests version inplaceSpec ghcPath ghcPkgPath =
    [ hunit "BuildDeps/SameDepsAllRound"
      (PackageTests.BuildDeps.SameDepsAllRound.Check.suite ghcPath)
      -- The two following tests were disabled by Johan Tibell as
      -- they have been failing for a long time:
      -- , hunit "BuildDeps/GlobalBuildDepsNotAdditive1/"
      --   (PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check.suite ghcPath)
      -- , hunit "BuildDeps/GlobalBuildDepsNotAdditive2/"
      --   (PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check.suite ghcPath)
    , hunit "BuildDeps/InternalLibrary0"
      (PackageTests.BuildDeps.InternalLibrary0.Check.suite version ghcPath)
    , hunit "PreProcess" (PackageTests.PreProcess.Check.suite ghcPath)
    , hunit "TestStanza" (PackageTests.TestStanza.Check.suite ghcPath)
      -- ^ The Test stanza test will eventually be required
      -- only for higher versions.
    , hunit "TestSuiteExeV10/Test" (PackageTests.TestSuiteExeV10.Check.checkTest ghcPath)
    , hunit "TestSuiteExeV10/TestWithHpc"
      (PackageTests.TestSuiteExeV10.Check.checkTestWithHpc ghcPath)
    , hunit "TestOptions" (PackageTests.TestOptions.Check.suite ghcPath)
    , hunit "BenchmarkStanza" (PackageTests.BenchmarkStanza.Check.suite ghcPath)
      -- ^ The benchmark stanza test will eventually be required
      -- only for higher versions.
    , hunit "BenchmarkExeV10/Test"
      (PackageTests.BenchmarkExeV10.Check.checkBenchmark ghcPath)
    , hunit "BenchmarkOptions" (PackageTests.BenchmarkOptions.Check.suite ghcPath)
    , hunit "TemplateHaskell/vanilla"
      (PackageTests.TemplateHaskell.Check.vanilla ghcPath)
    , hunit "TemplateHaskell/profiling"
      (PackageTests.TemplateHaskell.Check.profiling ghcPath)
    , hunit "PathsModule/Executable"
      (PackageTests.PathsModule.Executable.Check.suite ghcPath)
    , hunit "PathsModule/Library" (PackageTests.PathsModule.Library.Check.suite ghcPath)
    , hunit "DeterministicAr"
        (PackageTests.DeterministicAr.Check.suite ghcPath ghcPkgPath)
    , hunit "EmptyLib/emptyLib"
      (PackageTests.EmptyLib.Check.emptyLib ghcPath)
    , hunit "BuildTestSuiteDetailedV09"
      (PackageTests.BuildTestSuiteDetailedV09.Check.suite inplaceSpec ghcPath)
    , hunit "OrderFlags"
      (PackageTests.OrderFlags.Check.suite ghcPath)
    , hunit "TemplateHaskell/dynamic"
      (PackageTests.TemplateHaskell.Check.dynamic ghcPath)
    ] ++
    -- These tests are only required to pass on cabal version >= 1.7
    (if version >= Version [1, 7] []
     then [ hunit "BuildDeps/TargetSpecificDeps1"
            (PackageTests.BuildDeps.TargetSpecificDeps1.Check.suite ghcPath)
          , hunit "BuildDeps/TargetSpecificDeps2"
            (PackageTests.BuildDeps.TargetSpecificDeps2.Check.suite ghcPath)
          , hunit "BuildDeps/TargetSpecificDeps3"
            (PackageTests.BuildDeps.TargetSpecificDeps3.Check.suite ghcPath)
          , hunit "BuildDeps/InternalLibrary1"
            (PackageTests.BuildDeps.InternalLibrary1.Check.suite ghcPath)
          , hunit "BuildDeps/InternalLibrary2"
            (PackageTests.BuildDeps.InternalLibrary2.Check.suite ghcPath ghcPkgPath)
          , hunit "BuildDeps/InternalLibrary3"
            (PackageTests.BuildDeps.InternalLibrary3.Check.suite ghcPath ghcPkgPath)
          , hunit "BuildDeps/InternalLibrary4"
            (PackageTests.BuildDeps.InternalLibrary4.Check.suite ghcPath ghcPkgPath)
          , hunit "PackageTests/CMain"
            (PackageTests.CMain.Check.checkBuild ghcPath)
          ]
     else [])

main :: IO ()
main = do
    -- WORKAROUND: disable buffering on stdout to get streaming test logs
    -- test providers _should_ do this themselves
    hSetBuffering stdout NoBuffering

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
    lbi <- getPersistBuildConfig_ ("dist" </> "setup-config")
    (ghc, _) <- requireProgram normal ghcProgram (withPrograms lbi)
    (ghcPkg, _) <- requireProgram normal ghcPkgProgram (withPrograms lbi)
    let ghcPath = programPath ghc
        ghcPkgPath = programPath ghcPkg
    putStrLn $ "Using ghc: " ++ ghcPath
    putStrLn $ "Using ghc-pkg: " ++ ghcPkgPath
    setCurrentDirectory "tests"
    -- Create a shared Setup executable to speed up Simple tests
    compileSetup "." ghcPath
    defaultMain (tests cabalVersion inplaceSpec ghcPath ghcPkgPath)

-- Like Distribution.Simple.Configure.getPersistBuildConfig but
-- doesn't check that the Cabal version matches, which it doesn't when
-- we run Cabal's own test suite, due to bootstrapping issues.
getPersistBuildConfig_ :: FilePath -> IO LocalBuildInfo
getPersistBuildConfig_ filename = do
  exists <- doesFileExist filename
  if not exists
    then die missing
    else withFileContents filename $ \str ->
      case lines str of
        [_header, rest] -> case reads rest of
          [(bi,_)] -> return bi
          _        -> die cantParse
        _            -> die cantParse
  where
    missing   = "Run the 'configure' command first."
    cantParse = "Saved package config file seems to be corrupt. "
             ++ "Try re-running the 'configure' command."
