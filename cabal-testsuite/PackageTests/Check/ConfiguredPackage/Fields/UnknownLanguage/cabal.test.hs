import Test.Cabal.Prelude

-- Uknown language.
main = cabalTest $
  fails $ cabal "check" []
