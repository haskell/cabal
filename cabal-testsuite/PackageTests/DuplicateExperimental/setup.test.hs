import Test.Cabal.Prelude
main = cabalTest $ do
    -- check output is summarized in -v1 (-v normal)
    res <- cabal' "build" ["--only-configure","duplicate","-vnormal"]
    assertOutputContains "(and 3 more occurrences)" res

    -- check output is _not_ summarized in -v2 (verbose)
    res <- cabal' "build" ["--only-configure","duplicate","-vverbose"]
    assertOutputDoesNotContain "(and 3 more occurrences)" res
