import Test.Cabal.Prelude

-- SetupHooks.hs is a valid setup script.
main = cabalTest $
  cabal "check" []
