import Test.Cabal.Prelude

-- Absolute path.
main = cabalTest $
  fails $ cabal "check" []
