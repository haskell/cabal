import Test.Cabal.Prelude

-- Missing `main-is`.
main = cabalTest $
  fails $ cabal "check" []
