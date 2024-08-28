import Test.Cabal.Prelude

main = do
    skipUnlessWindows
    cabalTest $ fails $ cabal "build" ["--enable-executable-dynamic", "--disable-shared"]
