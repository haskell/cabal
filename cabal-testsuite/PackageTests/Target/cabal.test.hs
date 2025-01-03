import Test.Cabal.Prelude

main = do
  cabalTest' "default-all" $ do
    cabal "v2-target" []

  cabalTest' "explicit-all" $ do
    cabal "v2-target" ["all"]

  cabalTest' "all-enable-tests" $ do
    cabal "v2-target" ["all", "--enable-tests"]

  cabalTest' "all-enable-benches" $ do
    cabal "v2-target" ["all", "--enable-benchmarks"]

  cabalTest' "everything" $ do
    cabal "v2-target" ["all", "--enable-tests", "--enable-benchmarks"]

  cabalTest' "all-exes" $ do
    cabal "v2-target" ["all:exes"]

  cabalTest' "all-tests" $ do
    cabal "v2-target" ["all:tests"]

  cabalTest' "all-benches" $ do
    cabal "v2-target" ["all:benches"]

  cabalTest' "package-target" $ do
    cabal "v2-target" ["a"]
    cabal "v2-target" ["a", "--enable-tests", "--enable-benchmarks"]

  cabalTest' "path-target" $ do
    cabal "v2-target" ["dir-a/"]
    cabal "v2-target" ["dir-a/", "--enable-tests", "--enable-benchmarks"]

  cabalTest' "component-target-lib" $ do
    cabal "v2-target" ["a:lib:a"]
    cabal "v2-target" ["lib:a"]
    cabal "v2-target" ["a:a"]

  cabalTest' "component-target-exe" $ do
    cabal "v2-target" ["a:exe:a-exe"]
    cabal "v2-target" ["exe:a-exe"]
    cabal "v2-target" ["a:a-exe"]

  cabalTest' "component-target-bench" $ do
    cabal "v2-target" ["a:bench:a-bench"]
    cabal "v2-target" ["bench:a-bench"]
    cabal "v2-target" ["a:a-bench"]

  cabalTest' "component-target-test" $ do
    cabal "v2-target" ["a:test:a-test"]
    cabal "v2-target" ["test:a-test"]
    cabal "v2-target" ["a:a-test"]

  cabalTest' "ctype-target" $ do
    cabal "v2-target" ["a:libs"]
    cabal "v2-target" ["a:exes"]
    cabal "v2-target" ["a:tests"]
    cabal "v2-target" ["a:benches"]

  cabalTest' "missing-target" $ do
    fails $ cabal "v2-target" ["c:exes"]
    fails $ cabal "v2-target" ["c:tests"]
    fails $ cabal "v2-target" ["c:benches"]
