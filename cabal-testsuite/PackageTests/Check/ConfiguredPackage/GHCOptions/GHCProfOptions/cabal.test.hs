import Test.Cabal.Prelude

-- Tricky option in `ghc-prof-options`.
main = cabalTest $
  fails $ cabal "check" []
