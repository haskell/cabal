import Test.Cabal.Prelude

main = cabalTest $ expectBroken 5541 $ do
    cabal "new-sdist" ["many-data-files"]
