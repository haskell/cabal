import Test.Cabal.Prelude

main = cabalTest $ do
    build <- fails $ cabal' "build" []
    assertOutputContains "can't find source for MyDummy" build
    return ()
