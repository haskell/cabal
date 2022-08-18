import Test.Cabal.Prelude

-- Tricky option in `ghc-options`.
main = cabalTest $
  fails $ cabal "check" []
