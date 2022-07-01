import Test.Cabal.Prelude

-- `extra-doc-files` need ≥1.18.
main = cabalTest $
  fails $ cabal "check" []
