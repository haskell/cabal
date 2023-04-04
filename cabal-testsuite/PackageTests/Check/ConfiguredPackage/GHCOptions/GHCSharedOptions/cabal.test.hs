import Test.Cabal.Prelude

-- Tricky option in `ghc-shared-options`.
main = cabalTest $
  cabal "check" []
