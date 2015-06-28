-- | Groups black-box tests of cabal-install and configures them to test
-- the correct binary.
--
-- This file should do nothing but import tests from other modules and run
-- them with the path to the correct cabal-install binary.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import System.Directory
        ( canonicalizePath, getCurrentDirectory, setCurrentDirectory
        , removeFile, doesFileExist )
import System.FilePath ((</>))
import Test.Tasty
        ( defaultMainWithIngredients, defaultIngredients
        , includingOptions, askOption
        , TestTree, testGroup, withResource
        )
import Test.Tasty.Options ( OptionDescription(..), IsOption(..) )
import Data.Proxy ( Proxy(..) )
import Data.Typeable ( Typeable )
import Data.Tagged ( Tagged(..) )
import Control.Monad ( when )
import System.SetEnv (unsetEnv)

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
tests :: IO TestsPaths -> TestTree
tests paths = testGroup "Package Tests" $
    [ testGroup "Freeze"         $ PackageTests.Freeze.Check.tests         paths
    , testGroup "Exec"           $ PackageTests.Exec.Check.tests           paths
    , testGroup "MultipleSource" $ PackageTests.MultipleSource.Check.tests paths
    ]

cabalProgram :: Program
cabalProgram = (simpleProgram "cabal") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }

-- | System initialization before the tests can be run
initTests :: FilePath -> IO TestsPaths
initTests dist = do
    buildDir <- canonicalizePath $ dist </> "build" </> "cabal"
    let programSearchPath = ProgramSearchPathDir buildDir : defaultProgramSearchPath
    (cabal, _) <- requireProgram normal cabalProgram
                      (setProgramSearchPath programSearchPath defaultProgramDb)
    (ghcPkg, _) <- requireProgram normal ghcPkgProgram defaultProgramDb
    canonicalConfigPath <- canonicalizePath $ "tests" </> packageTestsDirectory
    cwd <- getCurrentDirectory

    let testsPaths = TestsPaths {
          cabalPath  = programPath cabal,
          ghcPkgPath = programPath ghcPkg,
          configPath = canonicalConfigPath </> packageTestsConfigFile,
          oldCwd     = cwd
        }

    -- Unset CABAL_SANDBOX_CONFIG (if set)
    unsetEnv "CABAL_SANDBOX_CONFIG"

    -- TODO: Using putStrLn kind of messes with tasty's output. Not sure what
    -- the right approach is here though.
    putStrLn $ "Using cabal: "   ++ cabalPath  testsPaths
    putStrLn $ "Using ghc-pkg: " ++ ghcPkgPath testsPaths

    setCurrentDirectory "tests"
    removeConf -- assert that there is no existing config file
               -- (we want deterministic testing with the default
               --  config values)
    return testsPaths

-- | Restore system state after tests are complete
freeTests :: TestsPaths -> IO ()
freeTests TestsPaths{..} = do
    removeConf
    -- Change back to the old working directory so that the tests can be
    -- repeatedly run in `cabal repl` via `:main`.
    setCurrentDirectory oldCwd

-- | Remove the default config file that got created by the tests
removeConf :: IO ()
removeConf = do
    b <- doesFileExist confFile
    when b $ removeFile confFile
  where
    confFile = packageTestsDirectory </> "cabal-config"

main :: IO ()
main =
     defaultMainWithIngredients ingredients $
       askOption $ \(BuildDirOption dist) ->
         withResource (initTests dist) freeTests tests
  where
    ingredients = includingOptions options : defaultIngredients
    options     = [ Option (Proxy :: Proxy BuildDirOption)
                  ]

{-------------------------------------------------------------------------------
  Command line options
-------------------------------------------------------------------------------}

newtype BuildDirOption = BuildDirOption FilePath
  deriving (Typeable)

-- | The equivalent of cabal's --builddir option
instance IsOption BuildDirOption where
  defaultValue   = BuildDirOption "dist"
  parseValue     = Just . BuildDirOption
  optionName     = Tagged "builddir"
  optionHelp     = Tagged "The directory where Cabal puts generated build files"
