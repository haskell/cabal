import Test.Cabal.Prelude
main = cabalTest $ do
    expectBroken 4720 $ cabal "new-build" ["test"]
