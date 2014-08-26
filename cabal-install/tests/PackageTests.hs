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
        (canonicalizePath, getCurrentDirectory, setCurrentDirectory)
import Test.Framework (Test, defaultMain, testGroup)

-- Modules containing the tests.
import qualified PackageTests.Exec.Check
import qualified PackageTests.Freeze.Check
import qualified PackageTests.MultipleSource.Check

-- List of tests to run. Each test will be called with the path to the
-- cabal binary to use.
tests :: FilePath -> FilePath -> [Test]
tests cabalPath ghcPkgPath =
    [ testGroup "Freeze" $ PackageTests.Freeze.Check.tests cabalPath
    , testGroup "Exec"   $ PackageTests.Exec.Check.tests cabalPath ghcPkgPath
    , testGroup "MultipleSource" $ PackageTests.MultipleSource.Check.tests cabalPath
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
    let cabalPath = programPath cabal

    (ghcPkg, _) <- requireProgram normal ghcPkgProgram defaultProgramDb
    let ghcPkgPath = programPath ghcPkg
    putStrLn $ "Using cabal: " ++ cabalPath
    putStrLn $ "Using ghc-pkg: " ++ ghcPkgPath
    cwd <- getCurrentDirectory
    let runTests = do
        setCurrentDirectory "tests"
        defaultMain $ tests cabalPath ghcPkgPath
    -- Change back to the old working directory so that the tests can be
    -- repeatedly run in `cabal repl` via `:main`.
    runTests `E.finally` setCurrentDirectory cwd
