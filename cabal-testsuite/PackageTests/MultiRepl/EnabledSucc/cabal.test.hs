import Test.Cabal.Prelude

main = do
  cabalTest $ do
    skipUnlessGhcVersion ">= 9.4"
    skipIfWindows -- heisenbug, see #9103
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl","pkg-b", "pkg-a"] "Bar.bar"
    assertOutputContains "3735929054" res
