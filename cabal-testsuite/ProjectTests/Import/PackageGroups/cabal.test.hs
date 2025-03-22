import Test.Cabal.Prelude

main = cabalTest $ do
  -- Build from toplevel.
  cabal "build" ["all"]
  cabal "clean" []
  cabal "build" ["dep1"]
  cabal "build" ["dep2"]
