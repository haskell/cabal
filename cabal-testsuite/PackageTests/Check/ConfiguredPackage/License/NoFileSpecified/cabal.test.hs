import Test.Cabal.Prelude

-- `license-file` missing.
main = cabalTest $
  fails $ cabal "check" []
