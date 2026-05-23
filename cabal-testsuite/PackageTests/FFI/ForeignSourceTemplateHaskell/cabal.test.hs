import Test.Cabal.Prelude

main = cabalTest $ do
  cabal "v2-build" ["addForeignSourceC"]
  cabal "v2-build" ["addForeignSourceCxx"]
