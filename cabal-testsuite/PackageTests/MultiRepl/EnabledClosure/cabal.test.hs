import Test.Cabal.Prelude

main = do
  cabalTest $ do
    skipUnlessGhcVersion ">= 9.4"
    -- Note: only the last package is interactive.
    -- this test should load pkg-b too.
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl","pkg-c", "pkg-a"] "Quu.quu"

    assertOutputContains "- pkg-b-0 (interactive)" res
    assertOutputContains "168" res
