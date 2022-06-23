import Test.Cabal.Prelude

-- No `location`.
main = cabalTest $
  fails $ cabal "check" []
