import Test.Cabal.Prelude

-- Missing VCS info.
main = cabalTest $
  cabal "check" []
