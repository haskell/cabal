import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord . withRepo "repo" $ do
    -- For the multi-repl command
    skipUnlessGhcVersion ">= 9.4"
    skipUnlessAnyCabalVersion "< 3.11"
    res <- fails $ cabalWithStdin "v2-repl" ["--keep-temp-files","--enable-multi-repl","pkg-a", "pkg-b"] ""
    assertOutputContains "constraint from --enable-multi-repl requires >=3.11" res
