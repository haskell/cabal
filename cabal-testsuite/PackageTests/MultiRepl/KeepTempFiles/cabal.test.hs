import Test.Cabal.Prelude

main = do
  cabalTest' "yes" $ do
    skipUnlessGhcVersion ">= 9.4"
    cabal' "clean" []
    res <-
      cabalWithStdin
        "v2-repl"
        [ "--keep-temp-files"
        , "--enable-multi-repl"
        , "pkg-b"
        , "pkg-a"
        ]
        "Bar.bar"
    assertOutputContains "foo is 42" res
    void $ assertGlobMatchesTestDir testDistDir "multi-out*/"

  cabalTest' "no" $ do
    skipUnlessGhcVersion ">= 9.4"
    cabal' "clean" []
    res <-
      cabalWithStdin
        "v2-repl"
        [ "--enable-multi-repl"
        , "pkg-b"
        , "pkg-a"
        ]
        "Bar.bar"
    assertOutputContains "foo is 42" res
    void $ assertGlobDoesNotMatchTestDir testDistDir "multi-out*/"
