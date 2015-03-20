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
import PackageTests.Haddock.Check
import PackageTests.TestOptions.Check
import PackageTests.TestStanza.Check
import PackageTests.TestSuiteExeV10.Check
import PackageTests.OrderFlags.Check
import PackageTests.ReexportedModules.Check

import Distribution.Simple.Configure
    ( ConfigStateFileError(..), getConfigStateFile )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.Types (programPath)
import Distribution.Simple.Program.Builtin
    ( ghcProgram, ghcPkgProgram, haddockProgram )
import Distribution.Simple.Program.Db (requireProgram)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Text (display)
import Distribution.Verbosity (normal)
import Distribution.Version (Version(Version))

import Control.Exception (try, throw)
import System.Directory
    ( getCurrentDirectory, setCurrentDirectory )
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit


tests :: Version -> PackageSpec -> FilePath -> FilePath -> [TestTree]
tests version inplaceSpec ghcPath ghcPkgPath =
    [ testCase "BuildDeps/SameDepsAllRound"
      (PackageTests.BuildDeps.SameDepsAllRound.Check.suite ghcPath)
      -- The two following tests were disabled by Johan Tibell as
      -- they have been failing for a long time:
      -- , testCase "BuildDeps/GlobalBuildDepsNotAdditive1/"
      --   (PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check.suite ghcPath)
      -- , testCase "BuildDeps/GlobalBuildDepsNotAdditive2/"
      --   (PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check.suite ghcPath)
    , testCase "BuildDeps/InternalLibrary0"
      (PackageTests.BuildDeps.InternalLibrary0.Check.suite version ghcPath)
    , testCase "PreProcess" (PackageTests.PreProcess.Check.suite ghcPath)
    , testCase "TestStanza" (PackageTests.TestStanza.Check.suite ghcPath)
      -- ^ The Test stanza test will eventually be required
      -- only for higher versions.
    , testGroup "TestSuiteExeV10" (PackageTests.TestSuiteExeV10.Check.checks ghcPath)
    , testCase "TestOptions" (PackageTests.TestOptions.Check.suite ghcPath)
    , testCase "BenchmarkStanza" (PackageTests.BenchmarkStanza.Check.suite ghcPath)
      -- ^ The benchmark stanza test will eventually be required
      -- only for higher versions.
    , testCase "BenchmarkExeV10/Test"
      (PackageTests.BenchmarkExeV10.Check.checkBenchmark ghcPath)
    , testCase "BenchmarkOptions" (PackageTests.BenchmarkOptions.Check.suite ghcPath)
    , testCase "TemplateHaskell/vanilla"
      (PackageTests.TemplateHaskell.Check.vanilla ghcPath)
    , testCase "TemplateHaskell/profiling"
      (PackageTests.TemplateHaskell.Check.profiling ghcPath)
    , testCase "PathsModule/Executable"
      (PackageTests.PathsModule.Executable.Check.suite ghcPath)
    , testCase "PathsModule/Library" (PackageTests.PathsModule.Library.Check.suite ghcPath)
    , testCase "DeterministicAr"
        (PackageTests.DeterministicAr.Check.suite ghcPath ghcPkgPath)
    , testCase "EmptyLib/emptyLib"
      (PackageTests.EmptyLib.Check.emptyLib ghcPath)
    , testCase "Haddock" (PackageTests.Haddock.Check.suite ghcPath)
    , testCase "BuildTestSuiteDetailedV09"
      (PackageTests.BuildTestSuiteDetailedV09.Check.suite inplaceSpec ghcPath)
    , testCase "OrderFlags"
      (PackageTests.OrderFlags.Check.suite ghcPath)
    , testCase "TemplateHaskell/dynamic"
      (PackageTests.TemplateHaskell.Check.dynamic ghcPath)
    , testCase "ReexportedModules"
      (PackageTests.ReexportedModules.Check.suite ghcPath)
    ] ++
    -- These tests are only required to pass on cabal version >= 1.7
    (if version >= Version [1, 7] []
     then [ testCase "BuildDeps/TargetSpecificDeps1"
            (PackageTests.BuildDeps.TargetSpecificDeps1.Check.suite ghcPath)
          , testCase "BuildDeps/TargetSpecificDeps2"
            (PackageTests.BuildDeps.TargetSpecificDeps2.Check.suite ghcPath)
          , testCase "BuildDeps/TargetSpecificDeps3"
            (PackageTests.BuildDeps.TargetSpecificDeps3.Check.suite ghcPath)
          , testCase "BuildDeps/InternalLibrary1"
            (PackageTests.BuildDeps.InternalLibrary1.Check.suite ghcPath)
          , testCase "BuildDeps/InternalLibrary2"
            (PackageTests.BuildDeps.InternalLibrary2.Check.suite ghcPath ghcPkgPath)
          , testCase "BuildDeps/InternalLibrary3"
            (PackageTests.BuildDeps.InternalLibrary3.Check.suite ghcPath ghcPkgPath)
          , testCase "BuildDeps/InternalLibrary4"
            (PackageTests.BuildDeps.InternalLibrary4.Check.suite ghcPath ghcPkgPath)
          , testCase "PackageTests/CMain"
            (PackageTests.CMain.Check.checkBuild ghcPath)
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
            , distPref = Nothing
            }
    putStrLn $ "Cabal test suite - testing cabal version " ++
        display cabalVersion
    lbi <- getPersistBuildConfig_ ("dist" </> "setup-config")
    (ghc, _) <- requireProgram normal ghcProgram (withPrograms lbi)
    (ghcPkg, _) <- requireProgram normal ghcPkgProgram (withPrograms lbi)
    (haddock, _) <- requireProgram normal haddockProgram (withPrograms lbi)
    let ghcPath = programPath ghc
        ghcPkgPath = programPath ghcPkg
        haddockPath = programPath haddock
    putStrLn $ "Using ghc: " ++ ghcPath
    putStrLn $ "Using ghc-pkg: " ++ ghcPkgPath
    putStrLn $ "Using haddock: " ++ haddockPath
    setCurrentDirectory "tests"
    -- Create a shared Setup executable to speed up Simple tests
    compileSetup "." ghcPath
    defaultMain $ testGroup "Package Tests"
      (tests cabalVersion inplaceSpec ghcPath ghcPkgPath)

-- Like Distribution.Simple.Configure.getPersistBuildConfig but
-- doesn't check that the Cabal version matches, which it doesn't when
-- we run Cabal's own test suite, due to bootstrapping issues.
getPersistBuildConfig_ :: FilePath -> IO LocalBuildInfo
getPersistBuildConfig_ filename = do
    eLBI <- try $ getConfigStateFile filename
    case eLBI of
      Left (ConfigStateFileBadVersion _ _ (Right lbi)) -> return lbi
      Left (ConfigStateFileBadVersion _ _ (Left err)) -> throw err
      Left err -> throw err
      Right lbi -> return lbi
