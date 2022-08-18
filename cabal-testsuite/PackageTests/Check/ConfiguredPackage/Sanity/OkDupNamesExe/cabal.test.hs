import Test.Cabal.Prelude

-- It is OK for executables to have the same name of the external library.
main = cabalTest $
  cabal "check" []
