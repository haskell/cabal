import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "v2-build" ["--only-configure"]
    assertOutputContains       "Configuring library for" res
    assertOutputContains       "Configuring executable 'foo' for" res
    assertOutputDoesNotContain "Configuring test suite 'bar' for" res
    assertOutputDoesNotContain "Configuring benchmark 'baz' for" res
    assertOutputDoesNotContain "Building" res

    res <- cabal' "v2-build" ["--only-configure", "--enable-tests"]
    assertOutputContains       "Configuring library for" res
    assertOutputContains       "Configuring executable 'foo' for" res
    assertOutputContains       "Configuring test suite 'bar' for" res
    assertOutputDoesNotContain "Configuring benchmark 'baz' for" res
    assertOutputDoesNotContain "Building" res

    res <- cabal' "v2-build"
             [ "--only-configure", "--enable-tests", "--enable-benchmarks"]
    assertOutputContains       "Configuring library for" res
    assertOutputContains       "Configuring executable 'foo' for" res
    assertOutputContains       "Configuring test suite 'bar' for" res
    assertOutputContains       "Configuring benchmark 'baz' for" res
    assertOutputDoesNotContain "Building" res
