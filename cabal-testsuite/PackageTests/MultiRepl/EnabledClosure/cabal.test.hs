import Test.Cabal.Prelude

main = do
  cabalTest $ do
    skipUnlessGhcVersion ">= 9.4"
    -- Note: only the last package is interactive.
    -- this test should load pkg-b too.
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl","pkg-a", "pkg-c"] ""

    -- we should check that pkg-c is indeed loaded,
    -- but currently the unit order is non-deterministic
    -- Fix this when GHC has a way to change active unit.
    -- TODO: ask for pkg-c unit, print Quu.quu

    assertOutputContains "- pkg-b-0 (interactive)" res
    -- assertOutputContains "168" res
    return ()
