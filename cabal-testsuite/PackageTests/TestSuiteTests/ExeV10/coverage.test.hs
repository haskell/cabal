import Test.Cabal.Prelude

main = cabalTest $ do
    cabal "v2-test" ["--enable-coverage"]
