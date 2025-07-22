import Test.Cabal.Prelude

-- Do not warn on non-existant directory if it is absolute.
main = cabalTest $
  cabal "check" []
