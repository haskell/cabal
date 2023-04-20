import Test.Cabal.Prelude

-- `cmm-sources` and friends need ≥3.0.
main = cabalTest $
  cabal "check" []
