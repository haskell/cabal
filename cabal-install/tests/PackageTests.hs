-- | Groups black-box tests of cabal-install and configures them to test
-- the correct binary.
--
-- This file should do nothing but import tests from other modules and run
-- them with the path to the correct cabal-install binary.
module Main
       where

-- Modules from Cabal.
import Distribution.Simple.Program.Builtin (ghcPkgProgram)
import Distribution.Simple.Program.Db
        (defaultProgramDb, requireProgram, setProgramSearchPath)
import Distribution.Simple.Program.Find
        (ProgramSearchPathEntry(ProgramSearchPathDir), defaultProgramSearchPath)
import Distribution.Simple.Program.Types
        ( Program(..), simpleProgram, programPath)
import Distribution.Simple.Utils ( findProgramVersion )
import Distribution.Verbosity (normal)

-- Third party modules.
import qualified Control.Exception.Extensible as E
import System.Directory
        ( canonicalizePath, getCurrentDirectory, setCurrentDirectory
        , removeFile, doesFileExist )
import System.FilePath ((</>))
import Test.Framework (Test, defaultMain, testGroup)
import Control.Monad ( when )

-- Module containing common test code.

import PackageTests.PackageTester ( TestsPaths(..)
                                  , packageTestsDirectory
                                  , packageTestsConfigFile )

-- Modules containing the tests.
import qualified PackageTests.Exec.Check
import qualified PackageTests.Freeze.Check
import qualified PackageTests.MultipleSource.Check

-- List of tests to run. Each test will be called with the path to the
-- cabal binary to use.
tests :: PackageTests.PackageTester.TestsPaths -> [Test]
tests paths =
    [ testGroup "Freeze"         $ PackageTests.Freeze.Check.tests         paths
    , testGroup "Exec"           $ PackageTests.Exec.Check.tests           paths
    , testGroup "MultipleSource" $ PackageTests.MultipleSource.Check.tests paths
    ]

cabalProgram :: Program
cabalProgram = (simpleProgram "cabal") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }

main :: IO ()
main = do
    buildDir <- canonicalizePath "dist/build/cabal"
    let programSearchPath = ProgramSearchPathDir buildDir : defaultProgramSearchPath
    (cabal, _) <- requireProgram normal cabalProgram
                      (setProgramSearchPath programSearchPath defaultProgramDb)
    (ghcPkg, _) <- requireProgram normal ghcPkgProgram defaultProgramDb
    canonicalConfigPath <- canonicalizePath $ "tests" </> packageTestsDirectory

    let testsPaths = TestsPaths {
          cabalPath = programPath cabal,
          ghcPkgPath = programPath ghcPkg,
          configPath = canonicalConfigPath </> packageTestsConfigFile
        }

    putStrLn $ "Using cabal: "   ++ cabalPath  testsPaths
    putStrLn $ "Using ghc-pkg: " ++ ghcPkgPath testsPaths

    cwd <- getCurrentDirectory
    let confFile = packageTestsDirectory </> "cabal-config"
        removeConf = do
          b <- doesFileExist confFile
          when b $ removeFile confFile
    let runTests = do
          setCurrentDirectory "tests"
          removeConf -- assert that there is no existing config file
                     -- (we want deterministic testing with the default
                     --  config values)
          defaultMain $ tests testsPaths
    runTests `E.finally` do
        -- remove the default config file that got created by the tests
        removeConf
        -- Change back to the old working directory so that the tests can be
        -- repeatedly run in `cabal repl` via `:main`.
        setCurrentDirectory cwd
