import Test.Cabal.Prelude

-- Internal library / package name clash.
main = cabalTest $
  fails $ cabal "check" []
