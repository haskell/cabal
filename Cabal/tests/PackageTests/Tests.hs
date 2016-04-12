module PackageTests.Tests(tests) where

import PackageTests.PackageTester

import qualified PackageTests.BenchmarkStanza.Check
import qualified PackageTests.TestStanza.Check
import qualified PackageTests.DeterministicAr.Check
import qualified PackageTests.TestSuiteTests.ExeV10.Check

import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo(localPkgDescr, compiler), absoluteComponentInstallDirs
  , InstallDirs(libdir), maybeGetComponentLocalBuildInfo
  , ComponentLocalBuildInfo(componentUnitId), ComponentName(CLibName) )
import Distribution.Simple.InstallDirs ( CopyDest(NoCopyDest) )
import Distribution.Simple.BuildPaths  ( mkLibName, mkSharedLibName )
import Distribution.Simple.Compiler    ( compilerId )
import Distribution.System (buildOS, OS(Windows))

import Control.Monad

import System.Directory
import Data.Version
import Test.Tasty (mkTimeout, localOption)

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

  -- TODO: This shouldn't be necessary, but there seems to be some
  -- bug in the way the test is written.  Not going to lose sleep
  -- over this... but would be nice to fix.
  testWhen (hasCabalForGhc config)
   . groupTests "TestSuiteTests/ExeV10" $
      (PackageTests.TestSuiteTests.ExeV10.Check.tests config)

  -- Test if detailed-0.9 builds correctly
  testWhen (hasCabalForGhc config)
   . tcs "TestSuiteTests/LibV09" "Build" $ cabal_build ["--enable-tests"]

  -- Tests for #2489, stdio deadlock
  testWhen (hasCabalForGhc config)
   . mapTestTrees (localOption (mkTimeout $ 10 ^ (8 :: Int)))
   . tcs "TestSuiteTests/LibV09" "Deadlock" $ do
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

  -- Test that if two components have the same module name, they do not
  -- clobber each other.
  testWhen (hasCabalForGhc config) $ -- uses library test suite
    tc "DuplicateModuleName" $ do
      cabal_build ["--enable-tests"]
      r1 <- shouldFail $ cabal' "test" ["foo"]
      assertOutputContains "test B" r1
      assertOutputContains "test A" r1
      r2 <- shouldFail $ cabal' "test" ["foo2"]
      assertOutputContains "test C" r2
      assertOutputContains "test A" r2

  -- Test that if test suite has a name which conflicts with a package
  -- which is in the database, we can still use the test case (they
  -- should NOT shadow).
  testWhen (hasCabalForGhc config)
   . tc "TestNameCollision" $ do
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

  -- Basic test for internal libraries (in p); package q is to make
  -- sure that the internal library correctly is used, not the
  -- external library.
  tc "InternalLibraries" $ do
      withPackageDb $ do
          withPackage "q" $ cabal_install []
          withPackage "p" $ do
              cabal_install []
              cabal "clean" []
              r <- runInstalledExe' "foo" []
              assertOutputContains "I AM THE ONE" r

  -- Test to see if --gen-script works.
  tcs "InternalLibraries" "gen-script" $ do
    withPackageDb $ do
      withPackage "p" $ do
        cabal_build []
        cabal "copy" []
        cabal "register" ["--gen-script"]
        _ <- if buildOS == Windows
                then shell "cmd" ["/C", "register.bat"]
                else shell "./register.sh" []
        return ()
      -- Make sure we can see p
      withPackage "r" $ cabal_install []

  -- Test to see if --gen-pkg-config works.
  tcs "InternalLibraries" "gen-pkg-config" $ do
    withPackageDb $ do
      withPackage "p" $ do
        cabal_build []
        cabal "copy" []
        let dir = "pkg-config.bak"
        cabal "register" ["--gen-pkg-config=" ++ dir]
        -- Infelicity! Does not respect CWD.
        pkg_dir <- packageDir
        let notHidden = not . isHidden
            isHidden name = "." `isPrefixOf` name
        confs <- fmap (sort . filter notHidden)
               . liftIO $ getDirectoryContents (pkg_dir </> dir)
        forM_ confs $ \conf -> ghcPkg "register" [pkg_dir </> dir </> conf]

      -- Make sure we can see p
      withPackage "r" $ cabal_install []

  -- Internal libraries used by a statically linked executable:
  -- no libraries should get installed or registered.  (Note,
  -- this does build shared libraries just to make sure they
  -- don't get installed, so this test doesn't work on Windows.)
  testWhen (hasSharedLibraries config) $
    tcs "InternalLibraries/Executable" "Static" $
      multiple_libraries_executable False

  -- Internal libraries used by a dynamically linked executable:
  -- ONLY the dynamic library should be installed, no registration
  testWhen (hasSharedLibraries config) $
    tcs "InternalLibraries/Executable" "Dynamic" $
      multiple_libraries_executable True

  -- Internal library used by public library; it must be installed and
  -- registered.
  tc "InternalLibraries/Library" $ do
      withPackageDb $ do
          withPackage "foolib" $ cabal_install []
          withPackage "fooexe" $ do
              cabal_build []
              runExe' "fooexe" []
                  >>= assertOutputContains "25"

  -- Basic test to make sure we can build internal libraries manually.
  mtcs "InternalLibraries/Library" "AssumeDepsUpToDate" $ \step -> do
    withPackageDb $ do
      step "Building foolib"
      withPackage "foolib" $ do
        cabal "configure" []
        let upd = "--assume-deps-up-to-date"
        step "foolib: copying global data"
        cabal "copy" [upd]
        let mk_foolib_internal str = do
                step $ "foolib: building foolib-internal (" ++ str ++ ")"
                cabal "build" [upd, "foolib-internal"]
                cabal "copy" [upd, "foolib-internal"]
                cabal "register" [upd, "foolib-internal"]
            mk_foolib str = do
                step $ "foolib: building foolib (" ++ str ++ ")"
                cabal "build" [upd, "foolib"]
                cabal "copy" [upd, "foolib"]
                cabal "register" [upd, "foolib"]
        mk_foolib_internal "run 1"
        mk_foolib "run 1"
        mk_foolib_internal "run 2"
        mk_foolib "run 2"
      withPackage "fooexe" $ do
        cabal_build []
        runExe' "fooexe" [] >>= assertOutputContains "25"

  -- Test to ensure that cabal_macros.h are computed per-component.
  tc "Macros" $ do
      cabal_build []
      runExe' "macros-a" []
          >>= assertOutputContains "macros-a"
      runExe' "macros-b" []
          >>= assertOutputContains "macros-b"

  -- Test for 'build-type: Configure' example from the Cabal manual.
  -- Disabled on Windows since MingW doesn't ship with autoreconf by
  -- default.
  unlessWindows $ do
      tc "Configure" $ do
          _ <- shell "autoreconf" ["-i"]
          cabal_build []

  -- Test that per-component copy works, when only building library
  tc "CopyComponent/Lib" $
      withPackageDb $ do
          cabal "configure" []
          cabal "build" ["lib:p"]
          cabal "copy" ["lib:p"]

  -- Test that per-component copy works, when only building one executable
  tc "CopyComponent/Exe" $
      withPackageDb $ do
          cabal "configure" []
          cabal "build" ["myprog"]
          cabal "copy" ["myprog"]

  -- Test internal custom preprocessor
  tc "CustomPreProcess" $ do
      cabal_build []
      runExe' "hello-world" []
        >>= assertOutputContains "hello from A"

  -- Test that executable recompilation works
  -- https://github.com/haskell/cabal/issues/3294
  tc "Regression/T3294" $ do
    pkg_dir <- packageDir
    liftIO $ writeFile (pkg_dir </> "Main.hs") "main = putStrLn \"aaa\""
    cabal "configure" []
    cabal "build" []
    runExe' "T3294" [] >>= assertOutputContains "aaa"
    ghcFileModDelay
    liftIO $ writeFile (pkg_dir </> "Main.hs") "main = putStrLn \"bbb\""
    cabal "build" []
    runExe' "T3294" [] >>= assertOutputContains "bbb"

  -- Test build --assume-deps-up-to-date
  mtc "BuildAssumeDepsUpToDate" $ \step -> do
    step "Initial build"
    pkg_dir <- packageDir
    liftIO $ writeFile (pkg_dir </> "A.hs") "module A where\na = \"a1\""
    liftIO $ writeFile (pkg_dir </> "myprog/Main.hs") "import A\nmain = print (a ++ \" b1\")"
    cabal "configure" []
    cabal "build" ["--assume-deps-up-to-date", "BuildAssumeDepsUpToDate"]
    cabal "build" ["--assume-deps-up-to-date", "myprog"]
    runExe' "myprog" []
        >>= assertOutputContains "a1 b1"

    step "Rebuild executable only"
    ghcFileModDelay
    liftIO $ writeFile (pkg_dir </> "A.hs") "module A where\na = \"a2\""
    liftIO $ writeFile (pkg_dir </> "myprog/Main.hs") "import A\nmain = print (a ++ \" b2\")"
    cabal "build" ["--assume-deps-up-to-date", "myprog"]
    runExe' "myprog" []
        >>= assertOutputContains "a1 b2"

  -- Test copy --assume-deps-up-to-date
  -- NB: This test has a HORRIBLE HORRIBLE hack to ensure that
  -- on Windows, we don't try to make a prefix relative package;
  -- specifically, we give the package under testing a library
  -- so that we don't attempt to make it prefix relative.
  mtc "CopyAssumeDepsUpToDate" $ \step -> do
    withPackageDb $ do
      step "Initial build"
      cabal_build []

      step "Executable cannot find data file"
      pkg_dir <- packageDir
      shouldFail (runExe' "myprog" [])
        >>= assertOutputContains "does not exist"
      prefix_dir <- prefixDir
      shouldNotExist $ prefix_dir </> "bin" </> "myprog"

      step "Install data file"
      liftIO $ writeFile (pkg_dir </> "data") "aaa"
      cabal "copy" ["--assume-deps-up-to-date"]
      shouldNotExist $ prefix_dir </> "bin" </> "myprog"
      runExe' "myprog" []
        >>= assertOutputContains "aaa"

      step "Install executable"
      liftIO $ writeFile (pkg_dir </> "data") "bbb"
      cabal "copy" ["--assume-deps-up-to-date", "exe:myprog"]
      runInstalledExe' "myprog" []
        >>= assertOutputContains "aaa"

  -- Test register --assume-deps-up-to-date
  mtc "RegisterAssumeDepsUpToDate" $ \step -> do
    withPackageDb $ do
      -- We'll test this by generating registration files and verifying
      -- that they are indeed files (and not directories)
      step "Initial build and copy"
      cabal_build []
      cabal "copy" []

      step "Register q"
      let q_reg = "pkg-config-q"
      cabal "register" ["--assume-deps-up-to-date", "q", "--gen-pkg-config=" ++ q_reg]
      pkg_dir <- packageDir
      ghcPkg "register" [pkg_dir </> q_reg]

      step "Register p"
      let main_reg = "pkg-config-p"
      cabal "register" ["--assume-deps-up-to-date", "RegisterAssumeDepsUpToDate", "--gen-pkg-config=" ++ main_reg]
      ghcPkg "register" [pkg_dir </> main_reg]

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

    multiple_libraries_executable is_dynamic =
        withPackageDb $ do
            cabal_install $ [ if is_dynamic then "--enable-executable-dynamic"
                                            else "--disable-executable-dynamic"
                            , "--enable-shared"]
            dist_dir <- distDir
            lbi <- liftIO $ getPersistBuildConfig dist_dir
            let pkg_descr = localPkgDescr lbi
                compiler_id = compilerId (compiler lbi)
                cname = (CLibName "foo-internal")
                Just clbi = maybeGetComponentLocalBuildInfo lbi cname
                uid = componentUnitId clbi
                dir = libdir (absoluteComponentInstallDirs pkg_descr lbi uid
                              NoCopyDest)
            assertBool "interface files should NOT be installed" . not
                =<< liftIO (doesFileExist (dir </> "Foo.hi"))
            assertBool "static library should NOT be installed" . not
                =<< liftIO (doesFileExist (dir </> mkLibName uid))
            if is_dynamic
              then
                assertBool "dynamic library MUST be installed"
                    =<< liftIO (doesFileExist (dir </> mkSharedLibName
                                               compiler_id uid))
              else
                assertBool "dynamic library should NOT be installed" . not
                    =<< liftIO (doesFileExist (dir </> mkSharedLibName
                                               compiler_id uid))
            shouldFail $ ghcPkg "describe" ["foo"]
            -- clean away the dist directory so that we catch accidental
            -- dependence on the inplace files
            cabal "clean" []
            runInstalledExe' "foo" [] >>= assertOutputContains "46"

    tc :: FilePath -> TestM a -> TestTreeM ()
    tc name = testTree config name

    mtc :: FilePath -> ((String -> TestM ()) -> TestM a) -> TestTreeM ()
    mtc name = testTreeSteps config name

    tcs :: FilePath -> FilePath -> TestM a -> TestTreeM ()
    tcs name sub_name m
        = testTreeSub config name sub_name m

    mtcs :: FilePath -> FilePath -> ((String -> TestM ()) -> TestM a) -> TestTreeM ()
    mtcs name sub_name = testTreeSubSteps config name sub_name
