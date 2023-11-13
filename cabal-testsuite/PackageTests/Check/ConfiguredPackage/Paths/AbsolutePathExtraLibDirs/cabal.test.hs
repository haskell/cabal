import Test.Cabal.Prelude

-- Absolute paths can be used in `extra-lib-dirs`.
main = cabalTest $
  cabal "check" []
