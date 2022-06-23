import Test.Cabal.Prelude

-- Dubious AllRightsReserved.
main = cabalTest $
  fails $ cabal "check" []
