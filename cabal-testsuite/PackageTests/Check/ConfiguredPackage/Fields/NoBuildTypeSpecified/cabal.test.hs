import Test.Cabal.Prelude

-- No build-type specified.
main = cabalTest $
  fails $ cabal "check" []
