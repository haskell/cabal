import Test.Cabal.Prelude

main = do
  cabalTest' "multirepl-a" $ do
    skipUnlessGhcVersion ">= 9.4"
    res <- fails $ cabalWithStdin "v2-repl" ["--disable-multi-repl","pkg-a", "pkg-b"] "foo"
    assertOutputContains "Cannot open a repl for multiple components at once." res
    assertOutputContains "Your compiler supports a multiple component repl but support is not enabled." res

  cabalTest' "multirepl-b" $ do
    skipUnlessGhcVersion "< 9.4"
    res <- fails $ cabalWithStdin "v2-repl" ["--disable-multi-repl","pkg-a", "pkg-b"] "foo"
    assertOutputContains "Cannot open a repl for multiple components at once." res
    assertOutputContains "The reason for this limitation is that your version" res
