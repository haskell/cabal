import Test.Cabal.Prelude

main = cabalTest $ do
  cabal "build" []
