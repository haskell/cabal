import Test.Cabal.Prelude

-- A plain `cabal buck2` (no --enable-tests/--enable-benchmarks) must
-- succeed even though the package has test-suites and a benchmark. The
-- benchmark here depends on `stm`, which nothing else in the fixture
-- uses: since its stanza isn't enabled, `stm` is (correctly) absent from
-- the pruned dependency plan `cabal buck2` builds its installed-package
-- index from - so if the benchmark were still configured, Cabal would
-- fail with "the given installed package instance does not exist"
-- (Cabal-5000). Caught for real against `persistent` (criterion, in its
-- benchmark) - see buck2.md's DONE entry on this. (Uses a boot package
-- rather than a Hackage one because cabal-testsuite's sandbox has no
-- remote repository.)
--
-- The disabled components just get no rule.
-- `noCabalPackageDb`: `cabal buck2` finds installed packages only in GHC's
-- global package db and the cabal store. Without it the testsuite puts the
-- in-tree Cabal, an `-inplace` package in an extra package db, in front of
-- cabal, and the fixture's detailed-0.9 test-suite depends on Cabal.
main = cabalTest $ noCabalPackageDb $ do
    cwd <- fmap testCurrentDir getTestEnv
    recordMode DoNotRecord $ cabal "buck2" []

    let exeBzl = cwd </> "exe-pkg" </> "BUCK.cabal.bzl"
    assertFileDoesContain exeBzl "'kind': 'library'"
    assertFileDoesContain exeBzl "'kind': 'executable'"
    assertFileDoesNotContain exeBzl "'exe-pkg-bench'"
    assertFileDoesNotContain exeBzl "'kind': 'test-suite'"
    assertFileDoesNotContain exeBzl "'kind': 'benchmark'"
