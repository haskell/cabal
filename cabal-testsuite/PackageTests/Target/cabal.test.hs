import Test.Cabal.Prelude

main = do
  cabalTest' "default-all" $ do
    cabal "clean" []
    cabal "v2-target" []

  cabalTest' "explicit-all" $ do
    cabal "clean" []
    cabal "v2-target" ["all"]

  cabalTest' "all-enable-tests" $ do
    cabal "clean" []
    cabal "v2-target" ["all", "--enable-tests"]

  cabalTest' "all-enable-benches" $ do
    cabal "clean" []
    cabal "v2-target" ["all", "--enable-benchmarks"]

  cabalTest' "everything" $ do
    cabal "clean" []
    cabal "v2-target" ["all", "--enable-tests", "--enable-benchmarks"]

  cabalTest' "all-exes" $ do
    cabal "clean" []
    cabal "v2-target" ["all:exes"]

  cabalTest' "all-tests" $ do
    cabal "clean" []
    cabal "v2-target" ["all:tests"]

  cabalTest' "all-benches" $ do
    cabal "clean" []
    cabal "v2-target" ["all:benches"]

  cabalTest' "package-target" $ do
    cabal "clean" []
    cabal "v2-target" ["a"]
    cabal "clean" []
    cabal "v2-target" ["a", "--enable-tests", "--enable-benchmarks"]

  cabalTest' "path-target" $ do
    cabal "clean" []
    cabal "v2-target" ["dir-a/"]
    cabal "clean" []
    cabal "v2-target" ["dir-a/", "--enable-tests", "--enable-benchmarks"]

  cabalTest' "component-target-lib" $ do
    cabal "clean" []
    cabal "v2-target" ["a:lib:a"]
    cabal "clean" []
    cabal "v2-target" ["lib:a"]
    cabal "clean" []
    cabal "v2-target" ["a:a"]

  cabalTest' "component-target-exe" $ do
    cabal "clean" []
    cabal "v2-target" ["a:exe:a-exe"]
    cabal "clean" []
    cabal "v2-target" ["exe:a-exe"]
    cabal "clean" []
    cabal "v2-target" ["a:a-exe"]

  cabalTest' "component-target-bench" $ do
    cabal "clean" []
    cabal "v2-target" ["a:bench:a-bench"]
    cabal "clean" []
    cabal "v2-target" ["bench:a-bench"]
    cabal "clean" []
    cabal "v2-target" ["a:a-bench"]

  cabalTest' "component-target-test" $ do
    cabal "clean" []
    cabal "v2-target" ["a:test:a-test"]
    cabal "clean" []
    cabal "v2-target" ["test:a-test"]
    cabal "clean" []
    cabal "v2-target" ["a:a-test"]

  cabalTest' "ctype-target" $ do
    cabal "clean" []
    cabal "v2-target" ["a:libs"]
    cabal "clean" []
    cabal "v2-target" ["a:exes"]
    cabal "clean" []
    cabal "v2-target" ["a:tests"]
    cabal "clean" []
    cabal "v2-target" ["a:benches"]

  cabalTest' "missing-target" $ do
    cabal "clean" []
    fails $ cabal "v2-target" ["c:exes"]
    cabal "clean" []
    fails $ cabal "v2-target" ["c:tests"]
    cabal "clean" []
    fails $ cabal "v2-target" ["c:benches"]
