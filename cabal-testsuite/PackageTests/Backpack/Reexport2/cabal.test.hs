import Test.Cabal.Prelude
main = cabalTest $ do
    r <- fails $ cabal' "v2-build" []
    assertOutputContains "Asdf" r
    assertOutputContains "Reexport2" r
