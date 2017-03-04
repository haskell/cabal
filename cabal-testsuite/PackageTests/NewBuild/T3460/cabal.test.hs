import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "new-build" ["-j", "T3460"]
