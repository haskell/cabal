import Test.Cabal.Prelude

-- Tricky option in `ghc-shared-options`.
main = cabalTest $
  fails $ cabal "check" []
