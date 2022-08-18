import Test.Cabal.Prelude

-- No description.
main = cabalTest $
  fails $ cabal "check" []
