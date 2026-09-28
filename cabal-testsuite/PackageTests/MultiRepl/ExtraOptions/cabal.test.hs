import Test.Cabal.Prelude

main = do
  cabalTest $ do
    skipIfCIAndWindows 11933
    skipUnlessGhcVersion ">= 9.4"
    void $ cabalWithStdin "v2-repl" ["--enable-multi-repl","all"] ""
