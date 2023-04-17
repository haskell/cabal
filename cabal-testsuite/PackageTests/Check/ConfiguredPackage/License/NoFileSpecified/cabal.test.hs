import Test.Cabal.Prelude

-- `licence-file` missing.
main = cabalTest $
  cabal "check" []
