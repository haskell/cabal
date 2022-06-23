import Test.Cabal.Prelude

-- No `type`.
main = cabalTest $
  fails $ cabal "check" []
