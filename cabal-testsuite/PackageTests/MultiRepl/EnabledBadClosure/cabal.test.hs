import Test.Cabal.Prelude

main = do
  cabalTest $ do
    -- MP: TODO: This should query Cabal library version
    skipIfGhcVersion ">= 9.10"
    -- Note: only the last package is interactive.
    -- this test should load pkg-b too.
    res <- fails $ cabalWithStdin "v2-repl" ["--enable-multi-repl","pkg-c", "pkg-a"] "Quu.quu"

    assertOutputContains "constraint from --enable-multi-repl requires >=3.11" res
