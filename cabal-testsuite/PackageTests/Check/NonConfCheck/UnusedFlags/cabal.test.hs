import Test.Cabal.Prelude

-- Unused flag.
main = cabalTest $
  fails $ cabal "check" []
