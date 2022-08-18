import Test.Cabal.Prelude

-- Impossible version range for internal library.
main = cabalTest $
  fails $ cabal "check" []
