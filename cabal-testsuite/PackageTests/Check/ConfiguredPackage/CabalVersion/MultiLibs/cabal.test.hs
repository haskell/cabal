import Test.Cabal.Prelude

-- Multilibs or named libs need ≥2.0.
main = cabalTest $
  fails $ cabal "check" []
