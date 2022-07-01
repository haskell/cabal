import Test.Cabal.Prelude

-- Uknown extension.
main = cabalTest $
  fails $ cabal "check" []
