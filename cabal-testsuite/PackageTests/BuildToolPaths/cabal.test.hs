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
    Nothing -> skip "Cabal-hooks library unavailable."
    Just pkgdb -> recordMode DoNotRecord $ do
      -- At build-time:
      --
      --  - in a pre-build hook
      --  - in a Template Haskell splice
      cabal "build" [ "all", "--enable-tests", "--enable-benchmarks", "--package-db=" ++ pkgdb ]
      -- At runtime of a test-suite
      cabal "test" [ "pbts", "--package-db=" ++ pkgdb ]
      -- At runtime of a benchmark
      cabal "bench" [ "pbts", "--package-db=" ++ pkgdb ]
      -- At runtime of an executable
      cabal "run" [ "pbts-exe", "--package-db=" ++ pkgdb ]
