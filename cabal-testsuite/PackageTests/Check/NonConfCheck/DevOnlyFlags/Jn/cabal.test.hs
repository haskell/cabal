import Test.Cabal.Prelude

-- j[n].
main = cabalTest $
  fails $ cabal "check" []
