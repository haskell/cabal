import Test.Cabal.Prelude

main = do
  cabalTest $ recordMode DoNotRecord $ do
    skipUnlessAnyCabalVersion "< 3.11"
    -- Note: only the last package is interactive.
    -- this test should load pkg-b too.
    res <- fails $ cabalWithStdin "v2-repl" ["--enable-multi-repl","pkg-c", "pkg-a"] "Quu.quu"

    assertOutputContains "constraint from --enable-multi-repl requires >=3.11" res
