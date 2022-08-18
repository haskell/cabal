import Test.Cabal.Prelude

-- Suspicious license version.
main = cabalTest $
  fails $ cabal "check" []
