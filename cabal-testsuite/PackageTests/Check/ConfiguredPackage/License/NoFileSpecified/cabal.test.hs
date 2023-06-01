import Test.Cabal.Prelude

-- `license-file` missing.
main = cabalTest $
  cabal "check" []
