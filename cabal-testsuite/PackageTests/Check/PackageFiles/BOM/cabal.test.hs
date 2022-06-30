import Test.Cabal.Prelude

-- BOM at top of .cabal file.
main = cabalTest $
  fails $ cabal "check" []
