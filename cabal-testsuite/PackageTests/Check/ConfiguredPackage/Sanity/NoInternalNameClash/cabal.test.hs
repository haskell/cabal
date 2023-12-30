import Test.Cabal.Prelude

-- Sublibrary / package name clash.
main = cabalTest $
  fails $ cabal "check" []
