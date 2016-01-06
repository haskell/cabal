module PackageTests.Tests(tests) where

import PackageTests.PackageTester

import qualified PackageTests.BenchmarkStanza.Check
import qualified PackageTests.TestStanza.Check
import qualified PackageTests.DeterministicAr.Check
import qualified PackageTests.TestSuiteTests.ExeV10.Check

import Control.Monad

import Data.Version
import Test.Tasty (TestTree, testGroup, mkTimeout, localOption)
import Test.Tasty.HUnit (testCase)

-- TODO: turn this into a "test-defining writer monad".
-- This will let us handle scoping gracefully.
tests :: SuiteConfig -> [TestTree]
tests config =
  tail [ undefined

  ---------------------------------------------------------------------
  -- * External tests

  -- Test that Cabal parses 'benchmark' sections correctly
  , tc "BenchmarkStanza"  PackageTests.BenchmarkStanza.Check.suite

  -- Test that Cabal parses 'test' sections correctly
  , tc "TestStanza"       PackageTests.TestStanza.Check.suite

  -- Test that Cabal determinstically generates object archives
  , tc "DeterministicAr"  PackageTests.DeterministicAr.Check.suite

  ---------------------------------------------------------------------
  -- * Test suite tests

  , testGroup "TestSuiteTests"

    -- Test exitcode-stdio-1.0 test suites (and HPC)
    [ testGroup "ExeV10"
      (PackageTests.TestSuiteTests.ExeV10.Check.tests config)

    -- Test detailed-0.9 test suites
    , testGroup "LibV09" $
      let
        tcs :: FilePath -> TestM a -> TestTree
        tcs name m
            = testCase name (runTestM config ("TestSuiteTests/LibV09")
                                             (Just name) m)
      in -- Test if detailed-0.9 builds correctly
         [ tcs "Build" $ cabal_build ["--enable-tests"]

         -- Tests for #2489, stdio deadlock
         , localOption (mkTimeout $ 10 ^ (8 :: Int))
         . tcs "Deadlock" $ do
            cabal_build ["--enable-tests"]
            shouldFail $ cabal "test" []
         ]
    ]

  ---------------------------------------------------------------------
  -- * Inline tests

  -- Test if exitcode-stdio-1.0 benchmark builds correctly
  , tc "BenchmarkExeV10" $ cabal_build ["--enable-benchmarks"]

  -- Test --benchmark-option(s) flags on ./Setup bench
  , tc "BenchmarkOptions" $ do
      cabal_build ["--enable-benchmarks"]
      cabal "bench" [ "--benchmark-options=1 2 3" ]
      cabal "bench" [ "--benchmark-option=1"
                    , "--benchmark-option=2"
                    , "--benchmark-option=3"
                    ]

  -- Test --test-option(s) flags on ./Setup test
  , tc "TestOptions" $ do
      cabal_build ["--enable-tests"]
      cabal "test" ["--test-options=1 2 3"]
      cabal "test" [ "--test-option=1"
                   , "--test-option=2"
                   , "--test-option=3"
                   ]

  -- Test attempt to have executable depend on internal
  -- library, but cabal-version is too old.
  , tc "BuildDeps/InternalLibrary0" $ do
      r <- shouldFail $ cabal' "configure" []
      -- Should tell you how to enable the desired behavior
      let sb = "library which is defined within the same package."
      assertOutputContains sb r

  -- Test executable depends on internal library.
  , tc "BuildDeps/InternalLibrary1" $ cabal_build []

  -- Test that internal library is preferred to an installed on
  -- with the same name and version
  , tc "BuildDeps/InternalLibrary2" $ internal_lib_test "internal"

  -- Test that internal library is preferred to an installed on
  -- with the same name and LATER version
  , tc "BuildDeps/InternalLibrary3" $ internal_lib_test "internal"

  -- Test that an explicit dependency constraint which doesn't
  -- match the internal library causes us to use external library
  , tc "BuildDeps/InternalLibrary4" $ internal_lib_test "installed"

  -- Test "old build-dep behavior", where we should get the
  -- same package dependencies on all targets if cabal-version
  -- is sufficiently old.
  , tc "BuildDeps/SameDepsAllRound" $ cabal_build []

  -- Test "new build-dep behavior", where each target gets
  -- separate dependencies.  This tests that an executable
  -- dep does not leak into the library.
  , tc "BuildDeps/TargetSpecificDeps1" $ do
      cabal "configure" []
      r <- shouldFail $ cabal' "build" []
      assertBool "error should be in MyLibrary.hs" $
          resultOutput r =~ "^MyLibrary.hs:"
      assertBool "error should be \"Could not find module `System.Time\"" $
          resultOutput r =~ "Could not find module.*System.Time"

  -- This is a control on TargetSpecificDeps1; it should
  -- succeed.
  , tc "BuildDeps/TargetSpecificDeps2" $ cabal_build []

  -- Test "new build-dep behavior", where each target gets
  -- separate dependencies.  This tests that an library
  -- dep does not leak into the executable.
  , tc "BuildDeps/TargetSpecificDeps3" $ do
      cabal "configure" []
      r <- shouldFail $ cabal' "build" []
      assertBool "error should be in lemon.hs" $
          resultOutput r =~ "^lemon.hs:"
      assertBool "error should be \"Could not find module `System.Time\"" $
          resultOutput r =~ "Could not find module.*System.Time"

  -- Test that Paths module is generated and available for executables.
  , tc "PathsModule/Executable" $ cabal_build []

  -- Test that Paths module is generated and available for libraries.
  , tc "PathsModule/Library" $ cabal_build []

  -- Check that preprocessors (hsc2hs) are run
  , tc "PreProcess" $ cabal_build ["--enable-tests", "--enable-benchmarks"]

  -- Check that preprocessors that generate extra C sources are handled
  , tc "PreProcessExtraSources" $ cabal_build ["--enable-tests", "--enable-benchmarks"]

  -- Test building a vanilla library/executable which uses Template Haskell
  , tc "TemplateHaskell/vanilla" $ cabal_build []

  -- Test building a profiled library/executable which uses Template Haskell
  -- (Cabal has to build the non-profiled version first)
  , tc "TemplateHaskell/profiling" $ cabal_build ["--enable-library-profiling", "--enable-profiling"]

  -- Test building a dynamic library/executable which uses Template
  -- Haskell
  , tc "TemplateHaskell/dynamic" $ cabal_build ["--enable-shared", "--enable-executable-dynamic"]

  -- Test building an executable whose main() function is defined in a C
  -- file
  , tc "CMain" $ cabal_build []

  -- Test build when the library is empty, for #1241
  , tc "EmptyLib" $
      withPackage "empty" $ cabal_build []

  -- Test that "./Setup haddock" works correctly
  , tc "Haddock" $ do
      dist_dir <- distDir
      let haddocksDir = dist_dir </> "doc" </> "html" </> "Haddock"
      cabal "configure" []
      cabal "haddock" []
      let docFiles
              = map (haddocksDir </>)
                    ["CPP.html", "Literate.html", "NoCPP.html", "Simple.html"]
      mapM_ (assertFindInFile "For hiding needles.") docFiles

  -- Test that Haddock with a newline in synopsis works correctly, #3004
  , tc "HaddockNewline" $ do
        cabal "configure" []
        cabal "haddock" []

  -- Test that Cabal properly orders GHC flags passed to GHC (when
  -- there are multiple ghc-options fields.)
  , tc "OrderFlags" $ cabal_build []

  -- Test that reexported modules build correctly
  -- TODO: should also test that they import OK!
  , tc "ReexportedModules" $ do
      whenGhcVersion (>= Version [7,9] []) $ cabal_build []

  -- Test that Cabal computes different IPIDs when the source changes.
  , tc "UniqueIPID" . withPackageDb $ do
      withPackage "P1" $ cabal "configure" []
      withPackage "P2" $ cabal "configure" []
      withPackage "P1" $ cabal "build" []
      withPackage "P1" $ cabal "build" [] -- rebuild should work
      r1 <- withPackage "P1" $ cabal' "register" ["--print-ipid", "--inplace"]
      withPackage "P2" $ cabal "build" []
      r2 <- withPackage "P2" $ cabal' "register" ["--print-ipid", "--inplace"]
      let exIPID s = takeWhile (/= '\n') $
               head . filter (isPrefixOf $ "UniqueIPID-0.1-") $ (tails s)
      when ((exIPID $ resultOutput r1) == (exIPID $ resultOutput r2)) $
        assertFailure $ "cabal has not calculated different Installed " ++
          "package ID when source is changed."

  ]
  where
    -- Shared test function for BuildDeps/InternalLibrary* tests.
    internal_lib_test expect = withPackageDb $ do
        withPackage "to-install" $ cabal_install []
        cabal_build []
        r <- runExe' "lemon" []
        assertEqual
            ("executable should have linked with the " ++ expect ++ " library")
            ("myLibFunc " ++ expect)
            (concat $ lines (resultOutput r))

    tc :: FilePath -> TestM a -> TestTree
    tc name m
        = testCase name (runTestM config name Nothing m)
