import Test.Cabal.Prelude

-- `cabal buck2` generates buck2 build files for every local package,
-- without needing a real buck2 binary or a real haskell-buck2 checkout
-- (the fixture's own `buck2/` dir and `.buckconfig`/`PACKAGE` only need
-- to exist - see Distribution.Client.Buck2.Setup.checkBuck2Prelude).
-- This exercises the main mapping rules end to end: a plain library
-- (lib-pkg), a second package (exe-pkg) whose library depends on it
-- (cross-package `deps`), an executable with `c-sources` (-> a separate
-- `cxx_library`), an exitcode-stdio-1.0 test-suite and a benchmark (only
-- generated because --enable-tests/--enable-benchmarks are passed - a
-- benchmark maps onto a plain haskell_binary(), same as an executable),
-- a detailed-0.9 test-suite (a self-generated stub Main driving
-- `Distribution.TestSuite`'s own API directly, not real Cabal's own
-- stdin-driven Setup.hs-generated one - see Generate.hs's own
-- `testSuite`/`writeDetailedTestStub` haddock), and a manual flag
-- gating `cpp-options`.
--
-- Doesn't cover `build-tool-depends:` GHC preprocessor support
-- (hspec-discover, markdown-unlit - see Generate.hs's own
-- `preprocessBuildTools` haddock): cabal-testsuite's own sandbox has no
-- remote repository configured at all, so a fixture here can't depend
-- on a real Hackage package the way `hspec-discover`\/`markdown-unlit`
-- would need - verified instead against a real, unrelated multi-package
-- project (`haskell-servant/servant`) that genuinely uses both - see
-- buck2.md's own DONE entry on this.
--
-- Runs with `recordMode DoNotRecord`: the per-component "Configuring
-- ... for ..." notices this prints come from multiple worker threads
-- configuring independent components concurrently (see buck2.md's own
-- DONE entry on parallelising this), so their relative order isn't
-- deterministic - only the *file content* assertions below are.
main = cabalTest $ do
    cwd <- fmap testCurrentDir getTestEnv
    recordMode DoNotRecord $ cabal "buck2" ["--enable-tests", "--enable-benchmarks", "-f+loud"]

    -- The generated file is a build spec - a plain dict describing each
    -- component as Cabal sees it - interpreted by buck2/cabal.bzl, which
    -- decides which rules, labels and flags that becomes.
    let libBzl = cwd </> "lib-pkg" </> "BUCK.cabal.bzl"
    assertFileDoesContain libBzl "local_build_spec"
    assertFileDoesContain libBzl "'kind': 'library'"
    assertFileDoesContain libBzl "'name': 'lib-pkg'"
    assertFileDoesNotContain libBzl "haskell_library("

    let exeBzl = cwd </> "exe-pkg" </> "BUCK.cabal.bzl"
    assertFileDoesContain exeBzl "'kind': 'library'"
    assertFileDoesContain exeBzl "'kind': 'executable'"
    assertFileDoesContain exeBzl "'kind': 'test-suite'"
    assertFileDoesContain exeBzl "'kind': 'benchmark'"
    assertFileDoesContain exeBzl "'name': 'exe-pkg-bench'"

    -- A dependency on another local package carries that package's
    -- directory (from which the target label is made).
    assertFileDoesContain exeBzl "'package': 'lib-pkg'"
    assertFileDoesContain exeBzl "'dir': 'lib-pkg'"

    -- C sources and include directories are recorded as written in the
    -- .cabal file; cabal.bzl turns them into a cxx_library() and makes
    -- the include paths relative to the project root.
    assertFileDoesContain exeBzl "'cbits/helper.c'"
    assertFileDoesContain exeBzl "'include_dirs'"
    assertFileDoesContain exeBzl "'-DLOUD'"

    -- `ghc-options:` and `test-options:` from cabal.project (not the
    -- .cabal file) reach the spec: the former once per package, the
    -- latter as `test_args` with template variables expanded per
    -- test-suite. cabal-install's own always-added `-hide-all-packages`
    -- (a workaround for custom Setup.hs scripts) is deliberately not
    -- copied over.
    assertFileDoesContain exeBzl "'ghc_options'"
    assertFileDoesContain exeBzl "'-fno-ignore-asserts'"
    assertFileDoesNotContain exeBzl "-hide-all-packages"
    assertFileDoesNotContain libBzl "'ghc_options'"
    assertFileDoesContain exeBzl "'test_args'"
    assertFileDoesContain exeBzl "'--opt-one'"
    assertFileDoesContain exeBzl "'--opt-two=exe-pkg-test'"
    assertFileDoesNotContain libBzl "'test_args'"
    assertFileDoesContain exeBzl "'exe-pkg-detailed-test'"

    -- Paths_<pkg>.hs and the detailed-0.9 stub Main both live under
    -- cabal-buck2/autogen/, which has its own BUCK file (see below):
    -- the spec names them, and cabal.bzl refers to them by that file's
    -- export_file() target.
    assertFileDoesContain exeBzl "'Paths_exe_pkg': {"
    assertFileDoesContain exeBzl "'autogen': 'Paths_exe_pkg'"
    assertFileDoesContain exeBzl "'autogen': 'exe-pkg-detailed-test-stub-main'"

    -- The hand-editable BUCK wrapper is created (only once) and loads
    -- the generated file, whose entry point passes customisation through
    -- to cabal.bzl's cabal_targets().
    assertFileDoesContain (cwd </> "exe-pkg" </> "BUCK") "generated_targets"
    assertFileDoesContain exeBzl "def generated_targets(**kwargs):"

    -- The detailed-0.9 test-suite's stub Main is our own generated
    -- driver (not real Cabal's stdin-driven one - see basic.test.hs's
    -- own module-level comment), importing the user's named test-module
    -- directly.
    assertFileDoesContain
        (cwd </> "exe-pkg" </> "cabal-buck2" </> "autogen" </> "exe-pkg-detailed-test" </> "Main.hs")
        "import qualified DetailedTests as CabalBuck2TestModule"

    -- cabal_macros.h / Paths_<pkg>.hs are generated via real Cabal's
    -- own generators (Distribution.Simple.Build.Macros/PathsModule),
    -- not hand-rolled stand-ins - see buck2.md's own DONE entry on this.
    assertFileDoesContain
        (cwd </> "exe-pkg" </> "cabal-buck2" </> "autogen" </> "exe-pkg" </> "cabal_macros.h")
        "CURRENT_PACKAGE_KEY"
    assertFileDoesContain
        (cwd </> "exe-pkg" </> "cabal-buck2" </> "autogen" </> "Paths_exe_pkg.hs")
        "version ="

    -- cabal-buck2/autogen/BUCK exports every autogen file (Paths_<pkg>,
    -- each component's own cabal_macros.h, the detailed-0.9 stub Main)
    -- as a real, addressable target via export_file() - both what makes
    -- the cabal_component kwarg's own $(location ...) reference above a
    -- real, buck2-tracked dependency edge (unlike the untracked raw path
    -- string this used to be), and what lets a hand-written BUCK rule
    -- elsewhere in the project reference e.g. Paths_<pkg> directly - see
    -- buck2.md's DONE entry on this.
    let autogenBuck = cwd </> "exe-pkg" </> "cabal-buck2" </> "autogen" </> "BUCK"
    assertFileDoesContain autogenBuck "name = 'exe-pkg-cabal-macros'"
    assertFileDoesContain autogenBuck "name = 'Paths_exe_pkg'"
    assertFileDoesContain autogenBuck "name = 'exe-pkg-detailed-test-stub-main'"

    -- Every export_file() must set `out` explicitly to the real file's
    -- own basename (Paths_exe_pkg.hs, cabal_macros.h, Main.hs) - without
    -- it, export_file()'s own default (the *rule's* name, e.g. plain
    -- `Paths_exe_pkg`, no extension - see prelude/export_file.bzl) makes
    -- the materialised artifact lose its extension, which a real `buck2
    -- build` doesn't error on but silently drops from the Haskell
    -- module list entirely (buck2/haskell.bzl's own `is_haskell_src()`
    -- checks the artifact's filename, not the target label) - caught
    -- the hard way against a real Glean checkout, not by this fixture,
    -- since asserting file *content* alone can't see a buck2 artifact's
    -- own output filename - see buck2.md's DONE entry on this.
    assertFileDoesContain autogenBuck "out = 'Paths_exe_pkg.hs'"
    assertFileDoesContain autogenBuck "out = 'cabal_macros.h'"
    assertFileDoesContain autogenBuck "out = 'Main.hs'"
