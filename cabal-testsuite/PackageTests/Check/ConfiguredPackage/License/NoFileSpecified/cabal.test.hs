import Test.Cabal.Prelude

-- `licence-file` missing.
main = cabalTest $
  fails $ cabal "check" []
