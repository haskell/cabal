import Test.Cabal.Prelude

main = cabalTest $ do
    build <- fails $ cabal' "build" ["--enable-benchmarks"]
    assertOutputContains "MyDummy.hs doesn't exist" build
    return ()
