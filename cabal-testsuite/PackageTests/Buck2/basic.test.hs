import Test.Cabal.Prelude

-- `cabal buck2` generates buck2 build files for every local package,
-- without needing a real buck2 binary or a real haskell-buck2 checkout
-- (the fixture's own `buck2/` dir and `.buckconfig`/`PACKAGE` only need
-- to exist - see Distribution.Client.Buck2.Setup.checkBuck2Prelude).
-- This exercises the main mapping rules end to end: a plain library
-- (lib-pkg), a second package (exe-pkg) whose library depends on it
-- (cross-package `deps`), an executable with `c-sources` (-> a separate
-- `cxx_library`), a test-suite and a benchmark (only generated because
-- --enable-tests/--enable-benchmarks are passed - a benchmark maps onto
-- a plain haskell_binary(), same as an executable), and a manual flag
-- gating `cpp-options`.
--
-- Runs with `recordMode DoNotRecord`: the per-component "Configuring
-- ... for ..." notices this prints come from multiple worker threads
-- configuring independent components concurrently (see buck2.md's own
-- DONE entry on parallelising this), so their relative order isn't
-- deterministic - only the *file content* assertions below are.
main = cabalTest $ do
    cwd <- fmap testCurrentDir getTestEnv
    recordMode DoNotRecord $ cabal "buck2" ["--enable-tests", "--enable-benchmarks", "-f+loud"]

    let libBzl = cwd </> "lib-pkg" </> "BUCK.cabal.bzl"
    assertFileDoesContain libBzl "haskell_library"
    assertFileDoesContain libBzl "'lib-pkg'"

    let exeBzl = cwd </> "exe-pkg" </> "BUCK.cabal.bzl"
    assertFileDoesContain exeBzl "haskell_library"
    assertFileDoesContain exeBzl "haskell_binary"
    assertFileDoesContain exeBzl "haskell_test"
    assertFileDoesContain exeBzl "cxx_library"
    assertFileDoesContain exeBzl "//lib-pkg:lib-pkg"
    assertFileDoesContain exeBzl "-DLOUD"
    assertFileDoesContain exeBzl "'exe-pkg-bench'"

    -- The hand-editable BUCK wrapper is created (only once) and loads
    -- the generated file.
    assertFileDoesContain (cwd </> "exe-pkg" </> "BUCK") "generated_targets"

    -- cabal_macros.h / Paths_<pkg>.hs are generated via real Cabal's
    -- own generators (Distribution.Simple.Build.Macros/PathsModule),
    -- not hand-rolled stand-ins - see buck2.md's own DONE entry on this.
    assertFileDoesContain
        (cwd </> "exe-pkg" </> "cabal-buck2" </> "autogen" </> "exe-pkg" </> "cabal_macros.h")
        "CURRENT_PACKAGE_KEY"
    assertFileDoesContain
        (cwd </> "exe-pkg" </> "cabal-buck2" </> "autogen" </> "Paths_exe_pkg.hs")
        "version ="
