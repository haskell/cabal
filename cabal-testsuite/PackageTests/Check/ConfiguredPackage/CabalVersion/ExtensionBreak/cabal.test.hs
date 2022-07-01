import Test.Cabal.Prelude

-- Some extension need ≥1.2.
main = cabalTest $
  fails $ cabal "check" []
