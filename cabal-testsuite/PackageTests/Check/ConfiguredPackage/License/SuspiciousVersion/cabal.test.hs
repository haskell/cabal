import Test.Cabal.Prelude

-- Suspicious license version.
main = cabalTest $
  cabal "check" []
