import Test.Cabal.Prelude

main = do
    skipUnlessWindows
    cabalTest $ fails $ cabal "build" ["--enable-relocatable"]
