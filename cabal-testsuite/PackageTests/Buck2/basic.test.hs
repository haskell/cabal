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
-- stdin-driven Setup.hs-generated one - see CabalToBuck.hs's own
-- `testSuite`/`writeDetailedTestStub` haddock), and a manual flag
-- gating `cpp-options`.
--
-- Doesn't cover `build-tool-depends:` GHC preprocessor support
-- (hspec-discover, markdown-unlit - see CabalToBuck.hs's own
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

    let libBzl = cwd </> "lib-pkg" </> "BUCK.cabal.bzl"
    assertFileDoesContain libBzl "haskell_library"
    assertFileDoesContain libBzl "'lib-pkg'"

    let exeBzl = cwd </> "exe-pkg" </> "BUCK.cabal.bzl"
    assertFileDoesContain exeBzl "haskell_library"
    assertFileDoesContain exeBzl "haskell_binary"
    assertFileDoesContain exeBzl "haskell_test"
    assertFileDoesContain exeBzl "cxx_library"
    assertFileDoesContain exeBzl "//lib-pkg:lib-pkg"

    -- exported_preprocessor_flags (unlike srcs, a plain attrs.arg() -
    -- buck2 has no idea `-I...` even names a path) needs its
    -- `include-dirs:` entry prefixed with this package's own directory
    -- by hand - every cxx action always runs with the *project root* as
    -- its cwd, so a bare `-Icbits` resolves to the wrong place for any
    -- package that isn't at the project root itself. Caught for real
    -- against a non-fixture project (persistent-sqlite's bundled sqlite3
    -- amalgamation, `#include <sqlite3.h>`) - see buck2.md's DONE entry
    -- on this.
    assertFileDoesContain exeBzl "-Iexe-pkg/cbits"
    assertFileDoesContain exeBzl "-DLOUD"
    assertFileDoesContain exeBzl "'exe-pkg-bench'"
    assertFileDoesContain exeBzl "'exe-pkg-detailed-test'"

    -- Every generated rule gets a `cabal_component = (pkg, component)`
    -- kwarg (see buck2/haskell.bzl's own comment on it) instead of a
    -- plain, untracked `cabal_macros.h` path folded into compiler_flags
    -- - see buck2.md's DONE entry on this.
    assertFileDoesContain exeBzl "cabal_component = ('exe-pkg', 'exe-pkg')"
    assertFileDoesContain exeBzl "cabal_component = ('exe-pkg', 'exe-pkg-exe')"

    -- Paths_<pkg>.hs and the detailed-0.9 stub Main both live under
    -- cabal-buck2/autogen/, which now has its own BUCK file (see below)
    -- - so once generated they're referenced from srcs by that file's
    -- own export_file() target label, not a same-package-relative path.
    assertFileDoesContain exeBzl "'Paths_exe_pkg.hs': '//exe-pkg/cabal-buck2/autogen:Paths_exe_pkg'"
    assertFileDoesContain exeBzl "'Main.hs': '//exe-pkg/cabal-buck2/autogen:exe-pkg-detailed-test-stub-main'"

    -- The hand-editable BUCK wrapper is created (only once) and loads
    -- the generated file.
    assertFileDoesContain (cwd </> "exe-pkg" </> "BUCK") "generated_targets"

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
