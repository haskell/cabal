-- | Groups black-box tests of cabal-install and configures them to test
-- the correct binary.
--
-- This file should do nothing but import tests from other modules and run
-- them with the path to the correct cabal-install binary.
module Main
       where

-- Modules from Cabal.
import Distribution.Simple.Program.Db (defaultProgramDb, requireProgram)
import Distribution.Simple.Program.Types
        ( Program(..), simpleProgram, programPath)
import Distribution.Simple.Utils ( findProgramVersion )
import Distribution.Verbosity (normal)

-- Third party modules.
import qualified Control.Exception.Extensible as E
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Test.Framework (Test, defaultMain, testGroup)

-- Modules containing the tests.
import qualified PackageTests.Freeze.Check

-- List of tests to run. Each test will be called with the path to the
-- cabal binary to use.
tests :: [FilePath -> Test]
tests =
    [ testGroup "Freeze" . PackageTests.Freeze.Check.tests
    ]

cabalProgram :: Program
cabalProgram = (simpleProgram "cabal") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }

main :: IO ()
main = do
    (cabal, _) <- requireProgram normal cabalProgram defaultProgramDb
    let cabalPath = programPath cabal
    putStrLn $ "Using cabal: " ++ cabalPath
    cwd <- getCurrentDirectory
    let runTests = do
        setCurrentDirectory "tests"
        defaultMain (map ($ cabalPath) tests)
    -- Change back to the old working directory so that the tests can be
    -- repeatedly run in `cabal repl` via `:main`.
    runTests `E.finally` setCurrentDirectory cwd
