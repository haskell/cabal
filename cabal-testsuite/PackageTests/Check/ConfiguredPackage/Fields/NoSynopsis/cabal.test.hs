import Test.Cabal.Prelude

-- No synopsis.
main = cabalTest $
  fails $ cabal "check" []
