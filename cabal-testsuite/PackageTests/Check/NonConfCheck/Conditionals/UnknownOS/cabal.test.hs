import Test.Cabal.Prelude

-- Uknown OS name.
main = cabalTest $
  fails $ cabal "check" []
