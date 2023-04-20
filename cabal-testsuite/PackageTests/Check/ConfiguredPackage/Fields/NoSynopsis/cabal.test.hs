import Test.Cabal.Prelude

-- No synopsis.
main = cabalTest $
  cabal "check" []
