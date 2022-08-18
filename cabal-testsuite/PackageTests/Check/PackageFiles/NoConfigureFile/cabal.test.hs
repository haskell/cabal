import Test.Cabal.Prelude

-- No `configure` script.
main = cabalTest $
  fails $ cabal "check" []
