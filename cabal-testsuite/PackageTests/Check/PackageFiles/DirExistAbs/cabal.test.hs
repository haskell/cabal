import Test.Cabal.Prelude

-- Do not warn on non-existent directory if it is absolute.
main = cabalTest $
  cabal "check" []
