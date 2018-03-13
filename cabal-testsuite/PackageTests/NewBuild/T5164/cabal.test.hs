import Test.Cabal.Prelude
main = cabalTest $ do
    cabal' "new-build" ["all"] >>= assertOutputContains "Example data file"
