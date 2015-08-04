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
import PackageTests.PackageTester (PackageSpec(..), SuiteConfig(..), compileSetup)
import PackageTests.PathsModule.Executable.Check
import PackageTests.PathsModule.Library.Check
import PackageTests.PreProcess.Check
import PackageTests.PreProcessExtraSources.Check
import PackageTests.TemplateHaskell.Check
import PackageTests.CMain.Check
import PackageTests.DeterministicAr.Check
import PackageTests.EmptyLib.Check
import PackageTests.Haddock.Check
import PackageTests.TestOptions.Check
import PackageTests.TestStanza.Check
import PackageTests.TestSuiteTests.ExeV10.Check
import PackageTests.TestSuiteTests.LibV09.Check
import PackageTests.OrderFlags.Check
import PackageTests.ReexportedModules.Check
import PackageTests.UniqueIPID.Check

import Distribution.Simple.Configure
    ( ConfigStateFileError(..), findDistPrefOrDefault, getConfigStateFile )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.Types (programPath)
import Distribution.Simple.Program.Builtin
    ( ghcProgram, ghcPkgProgram, haddockProgram )
import Distribution.Simple.Program.Db (requireProgram)
import Distribution.Simple.Setup (Flag(..))
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Text (display)
import Distribution.Verbosity (normal)
import Distribution.Version (Version(Version))

import Control.Exception (try, throw)
import Distribution.Compat.Environment ( setEnv )
import System.Directory
    ( canonicalizePath, setCurrentDirectory )
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit


tests :: SuiteConfig -> Version -> [TestTree]
tests config version =
    [ testCase "BuildDeps/SameDepsAllRound"
      (PackageTests.BuildDeps.SameDepsAllRound.Check.suite config)
      -- The two following tests were disabled by Johan Tibell as
      -- they have been failing for a long time:
      -- , testCase "BuildDeps/GlobalBuildDepsNotAdditive1/"
      --   (PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check.suite ghcPath)
      -- , testCase "BuildDeps/GlobalBuildDepsNotAdditive2/"
      --   (PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check.suite ghcPath)
    , testCase "BuildDeps/InternalLibrary0"
      (PackageTests.BuildDeps.InternalLibrary0.Check.suite config version)
    , testCase "PreProcess" (PackageTests.PreProcess.Check.suite config)
    , testCase "PreProcessExtraSources"
      (PackageTests.PreProcessExtraSources.Check.suite config)
    , testCase "TestStanza" (PackageTests.TestStanza.Check.suite config)
      -- ^ The Test stanza test will eventually be required
      -- only for higher versions.
    , testGroup "TestSuiteTests"
      [ testGroup "ExeV10"
        (PackageTests.TestSuiteTests.ExeV10.Check.checks config)
      , testGroup "LibV09"
        (PackageTests.TestSuiteTests.LibV09.Check.checks config)
      ]
    , testCase "TestOptions" (PackageTests.TestOptions.Check.suite config)
    , testCase "BenchmarkStanza" (PackageTests.BenchmarkStanza.Check.suite config)
      -- ^ The benchmark stanza test will eventually be required
      -- only for higher versions.
    , testCase "BenchmarkExeV10/Test"
      (PackageTests.BenchmarkExeV10.Check.checkBenchmark config)
    , testCase "BenchmarkOptions" (PackageTests.BenchmarkOptions.Check.suite config)
    , testCase "TemplateHaskell/vanilla"
      (PackageTests.TemplateHaskell.Check.vanilla config)
    , testCase "TemplateHaskell/profiling"
      (PackageTests.TemplateHaskell.Check.profiling config)
    , testCase "PathsModule/Executable"
      (PackageTests.PathsModule.Executable.Check.suite config)
    , testCase "PathsModule/Library"
      (PackageTests.PathsModule.Library.Check.suite config)
    , testCase "DeterministicAr"
      (PackageTests.DeterministicAr.Check.suite config)
    , testCase "EmptyLib/emptyLib"
      (PackageTests.EmptyLib.Check.emptyLib config)
    , testCase "Haddock" (PackageTests.Haddock.Check.suite config)
    , testCase "OrderFlags"
      (PackageTests.OrderFlags.Check.suite config)
    , testCase "TemplateHaskell/dynamic"
      (PackageTests.TemplateHaskell.Check.dynamic config)
    , testCase "ReexportedModules"
      (PackageTests.ReexportedModules.Check.suite config)
    , testCase "UniqueIPID"
      (PackageTests.UniqueIPID.Check.suite config)
    ] ++
    -- These tests are only required to pass on cabal version >= 1.7
    (if version >= Version [1, 7] []
     then [ testCase "BuildDeps/TargetSpecificDeps1"
            (PackageTests.BuildDeps.TargetSpecificDeps1.Check.suite config)
          , testCase "BuildDeps/TargetSpecificDeps2"
            (PackageTests.BuildDeps.TargetSpecificDeps2.Check.suite config)
          , testCase "BuildDeps/TargetSpecificDeps3"
            (PackageTests.BuildDeps.TargetSpecificDeps3.Check.suite config)
          , testCase "BuildDeps/InternalLibrary1"
            (PackageTests.BuildDeps.InternalLibrary1.Check.suite config)
          , testCase "BuildDeps/InternalLibrary2"
            (PackageTests.BuildDeps.InternalLibrary2.Check.suite config)
          , testCase "BuildDeps/InternalLibrary3"
            (PackageTests.BuildDeps.InternalLibrary3.Check.suite config)
          , testCase "BuildDeps/InternalLibrary4"
            (PackageTests.BuildDeps.InternalLibrary4.Check.suite config)
          , testCase "PackageTests/CMain"
            (PackageTests.CMain.Check.checkBuild config)
          ]
     else [])

main :: IO ()
main = do
    -- Find the builddir used to build Cabal
    distPref_ <- findDistPrefOrDefault NoFlag >>= canonicalizePath
    -- Use the default builddir for all of the subsequent package tests
    setEnv "CABAL_BUILDDIR" "dist"
    lbi <- getPersistBuildConfig_ (distPref_ </> "setup-config")
    (ghc, _) <- requireProgram normal ghcProgram (withPrograms lbi)
    (ghcPkg, _) <- requireProgram normal ghcPkgProgram (withPrograms lbi)
    (haddock, _) <- requireProgram normal haddockProgram (withPrograms lbi)
    let haddockPath = programPath haddock
        dbFile = distPref_ </> "package.conf.inplace"
        config = SuiteConfig
                 { cabalDistPref = distPref_
                 , ghcPath = programPath ghc
                 , ghcPkgPath = programPath ghcPkg
                 , inplaceSpec = PackageSpec
                   { directory = []
                   , configOpts =
                     [ "--package-db=" ++ dbFile
                     , "--constraint=Cabal == " ++ display cabalVersion
                     ]
                   , distPref = Nothing
                   }
                 }
    putStrLn $ "Cabal test suite - testing cabal version " ++ display cabalVersion
    putStrLn $ "Using ghc: " ++ ghcPath config
    putStrLn $ "Using ghc-pkg: " ++ ghcPkgPath config
    putStrLn $ "Using haddock: " ++ haddockPath
    setCurrentDirectory "tests"
    -- Create a shared Setup executable to speed up Simple tests
    compileSetup config "."
    defaultMain $ testGroup "Package Tests"
      (tests config cabalVersion)

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
