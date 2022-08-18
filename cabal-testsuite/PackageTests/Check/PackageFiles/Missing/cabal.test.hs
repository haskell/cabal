import Test.Cabal.Prelude

-- No .cabal file.
main = cabalTest $
  fails $ cabal "check" []
