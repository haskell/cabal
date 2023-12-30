import Test.Cabal.Prelude

-- Impossible version range for sublibrary.
main = cabalTest $
  fails $ cabal "check" []
