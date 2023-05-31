import Test.Cabal.Prelude

main = cabalTest $ withRepo "repo" $ do
    skipUnlessGhcVersion ">= 9.4"
    void $ fails $ cabalWithStdin "v2-repl" ["--keep-temp-files","--enable-multi-repl","pkg-a", "pkg-b"] ""
