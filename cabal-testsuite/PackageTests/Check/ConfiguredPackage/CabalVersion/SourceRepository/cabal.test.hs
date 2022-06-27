import Test.Cabal.Prelude

-- `source-repository` need ≥1.6.
main = cabalTest $
  fails $ cabal "check" []
