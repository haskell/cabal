import Test.Cabal.Prelude

-- Suspicious license BSD4.
main = cabalTest $
  cabal "check" []
