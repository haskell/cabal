import Test.Cabal.Prelude

main = cabalTest $ do
    -- script is called "s.hs" to avoid Windows long path issue in CI
    res <- cabal' "v2-run" ["s.hs"]
    assertOutputContains "Hello World" res
