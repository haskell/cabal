import Test.Cabal.Prelude

main = cabalTest $ do
    build <- fails $ cabal' "build" ["--enable-tests"]
    assertOutputContains "Could not find Haskell source file MyDummy.hs" build
    return ()
