import Test.Cabal.Prelude

-- `virtual-modules` need ≥2.2.
main = cabalTest $
  fails $ cabal "check" []
