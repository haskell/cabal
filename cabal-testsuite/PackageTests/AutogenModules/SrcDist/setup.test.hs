{-# LANGUAGE CPP #-}

import Test.Cabal.Prelude

import Control.Monad.IO.Class
import Distribution.ModuleName hiding (main)
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

-- Test that setup parses and uses 'autogen-modules' fields correctly
main = setupAndCabalTest $ do
  dist_dir <- fmap testDistDir getTestEnv

  -- Calling sdist without running configure first makes test fail with:
  -- "Exception: Run the 'configure' command first."
  -- This is because we are calling getPersistBuildConfig

  configureResult <- setup' "configure" []
  sdistResult <- setup' "sdist" []

  -- Now check that all the correct modules were parsed.
  lbi <- getLocalBuildInfoM
  let Just gotLibrary = library (localPkgDescr lbi)
  let gotExecutable = head $ executables (localPkgDescr lbi)
  let gotTestSuite = head $ testSuites (localPkgDescr lbi)
  let gotBenchmark = head $ benchmarks (localPkgDescr lbi)
  assertEqual
    "library 'autogen-modules' field does not match expected"
    [fromString "PackageInfo_AutogenModules", fromString "Paths_AutogenModules", fromString "MyLibHelperModule"]
    (libModulesAutogen gotLibrary)
  assertEqual
    "executable 'autogen-modules' field does not match expected"
    [fromString "PackageInfo_AutogenModules", fromString "Paths_AutogenModules", fromString "MyExeHelperModule"]
    (exeModulesAutogen gotExecutable)
  assertEqual
    "test-suite 'autogen-modules' field does not match expected"
    [fromString "PackageInfo_AutogenModules", fromString "Paths_AutogenModules", fromString "MyTestHelperModule"]
    (testModulesAutogen gotTestSuite)
  assertEqual
    "benchmark 'autogen-modules' field does not match expected"
    [fromString "PackageInfo_AutogenModules", fromString "Paths_AutogenModules", fromString "MyBenchHelperModule"]
    (benchmarkModulesAutogen gotBenchmark)

  -- Package check messages.
  let libAutogenMsg =
        "An 'autogen-module' is neither on 'exposed-modules' or "
          ++ "'other-modules'"
  let exeAutogenMsg =
        "On executable 'Exe' an 'autogen-module' is not on "
          ++ "'other-modules'"
  let testAutogenMsg =
        "On test suite 'Test' an 'autogen-module' is not on "
          ++ "'other-modules'"
  let benchAutogenMsg =
        "On benchmark 'Bench' an 'autogen-module' is not on "
          ++ "'other-modules'"
  let pathsAutogenMsg =
        "Packages using 'cabal-version: 2.0' and the autogenerated"

  -- Asserts for the undesired check messages after configure.
  assertOutputDoesNotContain libAutogenMsg configureResult
  assertOutputDoesNotContain exeAutogenMsg configureResult
  assertOutputDoesNotContain testAutogenMsg configureResult
  assertOutputDoesNotContain benchAutogenMsg configureResult
  assertOutputDoesNotContain pathsAutogenMsg configureResult

  -- Asserts for the undesired check messages after sdist.
  assertOutputDoesNotContain "Distribution quality errors:" sdistResult
  assertOutputDoesNotContain libAutogenMsg sdistResult
  assertOutputDoesNotContain exeAutogenMsg sdistResult
  assertOutputDoesNotContain testAutogenMsg sdistResult
  assertOutputDoesNotContain benchAutogenMsg sdistResult
  assertOutputDoesNotContain "Distribution quality warnings:" sdistResult
  assertOutputDoesNotContain pathsAutogenMsg sdistResult

-- Assert sdist --list-sources output.
-- If called before configure fails, dist directory is not created.
{- FOURMOLU_DISABLE -}
  let listSourcesFileGot = dist_dir ++ "/" ++ "list-sources.txt"
  setup "sdist" ["--list-sources=" ++ listSourcesFileGot]
  let listSourcesStrExpected =
#if defined(mingw32_HOST_OS)
        ".\\MyLibrary.hs\n"
          ++ ".\\MyLibModule.hs\n"
          ++ ".\\Dummy.hs\n"
          ++ ".\\MyExeModule.hs\n"
          ++ ".\\Dummy.hs\n"
          ++ ".\\MyTestModule.hs\n"
          ++ ".\\Dummy.hs\n"
          ++ ".\\MyBenchModule.hs\n"
          ++ "LICENSE\n"
          ++ ".\\AutogenModules.cabal\n"
#else
            "./MyLibrary.hs\n"
          ++ "./MyLibModule.hs\n"
          ++ "./Dummy.hs\n"
          ++ "./MyExeModule.hs\n"
          ++ "./Dummy.hs\n"
          ++ "./MyTestModule.hs\n"
          ++ "./Dummy.hs\n"
          ++ "./MyBenchModule.hs\n"
          ++ "LICENSE\n"
          ++ "./AutogenModules.cabal\n"
#endif
  listSourcesStrGot <- liftIO $ readFile listSourcesFileGot
  assertEqual
    "sdist --list-sources does not match the expected files"
    listSourcesStrExpected
    listSourcesStrGot

  return ()
