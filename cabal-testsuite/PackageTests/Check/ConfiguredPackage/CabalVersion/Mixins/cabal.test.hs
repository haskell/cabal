import Test.Cabal.Prelude

-- `mixins` need ≥2.0.
main = cabalTest $
  fails $ cabal "check" []
