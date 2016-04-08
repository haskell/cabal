module PackageTests.Tests(tests) where

import PackageTests.PackageTester

import qualified PackageTests.BenchmarkStanza.Check
import qualified PackageTests.TestStanza.Check
import qualified PackageTests.DeterministicAr.Check
import qualified PackageTests.TestSuiteTests.ExeV10.Check

import Control.Monad

import Data.Version
import Test.Tasty (mkTimeout, localOption)
import Test.Tasty.HUnit (testCase)

tests :: SuiteConfig -> TestTreeM ()
tests config = do

  ---------------------------------------------------------------------
  -- * External tests

  -- Test that Cabal parses 'benchmark' sections correctly
  tc "BenchmarkStanza"  PackageTests.BenchmarkStanza.Check.suite

  -- Test that Cabal parses 'test' sections correctly
  tc "TestStanza"       PackageTests.TestStanza.Check.suite

  -- Test that Cabal determinstically generates object archives
  tc "DeterministicAr"  PackageTests.DeterministicAr.Check.suite

  ---------------------------------------------------------------------
  -- * Test suite tests

  groupTests "TestSuiteTests" $ do

    -- Test exitcode-stdio-1.0 test suites (and HPC)
    groupTests "ExeV10"
      (PackageTests.TestSuiteTests.ExeV10.Check.tests config)

    -- Test detailed-0.9 test suites
    groupTests "LibV09" $
      let
        tcs :: FilePath -> TestM a -> TestTreeM ()
        tcs name m
            = testTree' $ testCase name (runTestM config
                                         "TestSuiteTests/LibV09" (Just name) m)
      in do
          -- Test if detailed-0.9 builds correctly
          tcs "Build" $ cabal_build ["--enable-tests"]

          -- Tests for #2489, stdio deadlock
          mapTestTrees (localOption (mkTimeout $ 10 ^ (8 :: Int)))
            . tcs "Deadlock" $ do
              cabal_build ["--enable-tests"]
              shouldFail $ cabal "test" []

  ---------------------------------------------------------------------
  -- * Inline tests

  -- Test if exitcode-stdio-1.0 benchmark builds correctly
  tc "BenchmarkExeV10" $ cabal_build ["--enable-benchmarks"]

  -- Test --benchmark-option(s) flags on ./Setup bench
  tc "BenchmarkOptions" $ do
      cabal_build ["--enable-benchmarks"]
      cabal "bench" [ "--benchmark-options=1 2 3" ]
      cabal "bench" [ "--benchmark-option=1"
                    , "--benchmark-option=2"
                    , "--benchmark-option=3"
                    ]

  -- Test --test-option(s) flags on ./Setup test
  tc "TestOptions" $ do
      cabal_build ["--enable-tests"]
      cabal "test" ["--test-options=1 2 3"]
      cabal "test" [ "--test-option=1"
                   , "--test-option=2"
                   , "--test-option=3"
                   ]

  -- Test attempt to have executable depend on internal
  -- library, but cabal-version is too old.
  tc "BuildDeps/InternalLibrary0" $ do
      r <- shouldFail $ cabal' "configure" []
      -- Should tell you how to enable the desired behavior
      let sb = "library which is defined within the same package."
      assertOutputContains sb r

  -- Test executable depends on internal library.
  tc "BuildDeps/InternalLibrary1" $ cabal_build []

  -- Test that internal library is preferred to an installed on
  -- with the same name and version
  tc "BuildDeps/InternalLibrary2" $ internal_lib_test "internal"

  -- Test that internal library is preferred to an installed on
  -- with the same name and LATER version
  tc "BuildDeps/InternalLibrary3" $ internal_lib_test "internal"

  -- Test that an explicit dependency constraint which doesn't
  -- match the internal library causes us to use external library
  tc "BuildDeps/InternalLibrary4" $ internal_lib_test "installed"

  -- Test "old build-dep behavior", where we should get the
  -- same package dependencies on all targets if cabal-version
  -- is sufficiently old.
  tc "BuildDeps/SameDepsAllRound" $ cabal_build []

  -- Test "new build-dep behavior", where each target gets
  -- separate dependencies.  This tests that an executable
  -- dep does not leak into the library.
  tc "BuildDeps/TargetSpecificDeps1" $ do
      cabal "configure" []
      r <- shouldFail $ cabal' "build" []
      assertRegex "error should be in MyLibrary.hs" "^MyLibrary.hs:" r
      assertRegex
        "error should be \"Could not find module `Text\\.PrettyPrint\""
        "(Could not find module|Failed to load interface for).*Text\\.PrettyPrint" r

  -- This is a control on TargetSpecificDeps1; it should
  -- succeed.
  tc "BuildDeps/TargetSpecificDeps2" $ cabal_build []

  -- Test "new build-dep behavior", where each target gets
  -- separate dependencies.  This tests that an library
  -- dep does not leak into the executable.
  tc "BuildDeps/TargetSpecificDeps3" $ do
      cabal "configure" []
      r <- shouldFail $ cabal' "build" []
      assertRegex "error should be in lemon.hs" "^lemon.hs:" r
      assertRegex
        "error should be \"Could not find module `Text\\.PrettyPrint\""
        "(Could not find module|Failed to load interface for).*Text\\.PrettyPrint" r

  -- Test that Paths module is generated and available for executables.
  tc "PathsModule/Executable" $ cabal_build []

  -- Test that Paths module is generated and available for libraries.
  tc "PathsModule/Library" $ cabal_build []

  -- Check that preprocessors (hsc2hs) are run
  tc "PreProcess" $ cabal_build ["--enable-tests", "--enable-benchmarks"]

  -- Check that preprocessors that generate extra C sources are handled
  tc "PreProcessExtraSources" $ cabal_build ["--enable-tests",
                                             "--enable-benchmarks"]

  -- Test building a vanilla library/executable which uses Template Haskell
  tc "TemplateHaskell/vanilla" $ cabal_build []

  -- Test building a profiled library/executable which uses Template Haskell
  -- (Cabal has to build the non-profiled version first)
  tc "TemplateHaskell/profiling" $ cabal_build ["--enable-library-profiling",
                                                "--enable-profiling"]

  -- Test building a dynamic library/executable which uses Template
  -- Haskell
  testWhen (hasSharedLibraries config) $
    tc "TemplateHaskell/dynamic" $ cabal_build ["--enable-shared",
                                                "--enable-executable-dynamic"]

  -- Test building an executable whose main() function is defined in a C
  -- file
  tc "CMain" $ cabal_build []

  -- Test build when the library is empty, for #1241
  tc "EmptyLib" $
      withPackage "empty" $ cabal_build []

  -- Test that "./Setup haddock" works correctly
  tc "Haddock" $ do
      dist_dir <- distDir
      let haddocksDir = dist_dir </> "doc" </> "html" </> "Haddock"
      cabal "configure" []
      cabal "haddock" []
      let docFiles
              = map (haddocksDir </>)
                    ["CPP.html", "Literate.html", "NoCPP.html", "Simple.html"]
      mapM_ (assertFindInFile "For hiding needles.") docFiles

  -- Test that Haddock with a newline in synopsis works correctly, #3004
  tc "HaddockNewline" $ do
        cabal "configure" []
        cabal "haddock" []

  -- Test that Cabal properly orders GHC flags passed to GHC (when
  -- there are multiple ghc-options fields.)
  tc "OrderFlags" $ cabal_build []

  -- Test that reexported modules build correctly
  -- TODO: should also test that they import OK!
  tc "ReexportedModules" $ do
      whenGhcVersion (>= Version [7,9] []) $ cabal_build []

  -- Test that Cabal computes different IPIDs when the source changes.
  tc "UniqueIPID" . withPackageDb $ do
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

  tc "DuplicateModuleName" $ do
      cabal_build ["--enable-tests"]
      r1 <- shouldFail $ cabal' "test" ["foo"]
      assertOutputContains "test B" r1
      assertOutputContains "test A" r1
      r2 <- shouldFail $ cabal' "test" ["foo2"]
      assertOutputContains "test C" r2
      assertOutputContains "test A" r2

  tc "TestNameCollision" $ do
        withPackageDb $ do
          withPackage "parent" $ cabal_install []
          withPackage "child" $ do
            cabal_build ["--enable-tests"]
            cabal "test" []

  -- Test that '--allow-newer' works via the 'Setup.hs configure' interface.
  tc "AllowNewer" $ do
        shouldFail $ cabal "configure" []
        cabal "configure" ["--allow-newer"]
        shouldFail $ cabal "configure" ["--allow-newer=baz,quux"]
        cabal "configure" ["--allow-newer=base", "--allow-newer=baz,quux"]
        cabal "configure" ["--allow-newer=bar", "--allow-newer=base,baz"
                          ,"--allow-newer=quux"]
        shouldFail $ cabal "configure" ["--enable-tests"]
        cabal "configure" ["--enable-tests", "--allow-newer"]
        shouldFail $ cabal "configure" ["--enable-benchmarks"]
        cabal "configure" ["--enable-benchmarks", "--allow-newer"]
        shouldFail $ cabal "configure" ["--enable-benchmarks", "--enable-tests"]
        cabal "configure" ["--enable-benchmarks", "--enable-tests"
                          ,"--allow-newer"]
        shouldFail $ cabal "configure" ["--allow-newer=Foo:base"]
        shouldFail $ cabal "configure" ["--allow-newer=Foo:base"
                                       ,"--enable-tests", "--enable-benchmarks"]
        cabal "configure" ["--allow-newer=AllowNewer:base"]
        cabal "configure" ["--allow-newer=AllowNewer:base"
                          ,"--allow-newer=Foo:base"]
        cabal "configure" ["--allow-newer=AllowNewer:base"
                          ,"--allow-newer=Foo:base"
                          ,"--enable-tests", "--enable-benchmarks"]

  -- Test that Cabal can choose flags to disable building a component when that
  -- component's dependencies are unavailable. The build should succeed without
  -- requiring the component's dependencies or imports.
  tc "BuildableField" $ do
      r <- cabal' "configure" ["-v"]
      assertOutputContains "Flags chosen: build-exe=False" r
      cabal "build" []

  -- TODO: Enable these tests on Windows
  unlessWindows $ do
      tc "GhcPkgGuess/SameDirectory" $ ghc_pkg_guess "ghc"
      tc "GhcPkgGuess/SameDirectoryVersion" $ ghc_pkg_guess "ghc-7.10"
      tc "GhcPkgGuess/SameDirectoryGhcVersion" $ ghc_pkg_guess "ghc-7.10"

  unlessWindows $ do
      tc "GhcPkgGuess/Symlink" $ do
        -- We don't want to distribute a tarball with symlinks. See #3190.
        withSymlink "bin/ghc"
                    "tests/PackageTests/GhcPkgGuess/Symlink/ghc" $
                    ghc_pkg_guess "ghc"

      tc "GhcPkgGuess/SymlinkVersion" $ do
        withSymlink "bin/ghc-7.10"
                    "tests/PackageTests/GhcPkgGuess/SymlinkVersion/ghc" $
                    ghc_pkg_guess "ghc"

      tc "GhcPkgGuess/SymlinkGhcVersion" $ do
        withSymlink "bin/ghc-7.10"
                    "tests/PackageTests/GhcPkgGuess/SymlinkGhcVersion/ghc" $
                    ghc_pkg_guess "ghc"

  where
    ghc_pkg_guess bin_name = do
        cwd <- packageDir
        with_ghc <- getWithGhcPath
        r <- withEnv [("WITH_GHC", Just with_ghc)]
           . shouldFail $ cabal' "configure" ["-w", cwd </> bin_name]
        assertOutputContains "is version 9999999" r
        return ()

    -- Shared test function for BuildDeps/InternalLibrary* tests.
    internal_lib_test expect = withPackageDb $ do
        withPackage "to-install" $ cabal_install []
        cabal_build []
        r <- runExe' "lemon" []
        assertEqual
            ("executable should have linked with the " ++ expect ++ " library")
            ("foo foo myLibFunc " ++ expect)
            (concatOutput (resultOutput r))

    assertRegex :: String -> String -> Result -> TestM ()
    assertRegex msg regex r = let out = resultOutput r
                              in assertBool (msg ++ ",\nactual output:\n" ++ out)
                                 (out =~ regex)

    tc :: FilePath -> TestM a -> TestTreeM ()
    tc name = testTree config name Nothing
