import Test.Cabal.Prelude

-- No custom-setup with build-type: simple.
main = cabalTest $
  fails $ cabal "check" []
