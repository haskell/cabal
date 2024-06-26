import Test.Cabal.Prelude

-- Test that build-tool-depends executables are correctly provisioned
-- in all circumstances.
main = cabalTest $ do
  mpkgdb <- testPackageDbPath <$> getTestEnv
  case mpkgdb of
    -- This test can only be run using the in-tree Cabal library because
    -- Cabal-hooks isn't available in any package database (GHC isn't yet
    -- shipped with a version of Cabal with SetupHooks).
    -- Ever since 716b109c4ae908458b16af5d75c233c7d9fdfc06, we use --intree-cabal-lib in
    -- CI, so we should always take the "Just" case which actually runs the test.
    --
    -- NB: be sure to use v2 commands, as otherwise the testsuite driver will not
    -- pass --package-db flags.
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> recordMode DoNotRecord $ do
      -- At build-time:
      --
      --  - in a pre-build hook
      --  - in a Template Haskell splice
      cabal "v2-build" [ "all", "--enable-tests", "--enable-benchmarks"]
      -- At runtime of a test-suite
      cabal "v2-test" [ "pbts" ]
      -- At runtime of a benchmark
      cabal "v2-bench" [ "pbts" ]
      -- At runtime of an executable
      cabal "v2-run" [ "pbts-exe" ]
