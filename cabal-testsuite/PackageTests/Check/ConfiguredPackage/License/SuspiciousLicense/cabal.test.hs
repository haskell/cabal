import Test.Cabal.Prelude

-- Suspicious license BSD4.
main = cabalTest $
  fails $ cabal "check" []
