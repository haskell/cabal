import Test.Cabal.Prelude

-- Dubious AllRightsReserved.
main = cabalTest $
  cabal "check" []
