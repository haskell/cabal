import Test.Cabal.Prelude

-- Partial extension match & <2.4.
main = cabalTest $
  fails $ cabal "check" []
