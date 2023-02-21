import Test.Cabal.Prelude

main = cabalTest $ do
  -- Build from toplevel.
  cabal "build" ["all"]
  cabal "clean" []
  cabal "build" ["dep1"]
  cabal "build" ["dep2"]
  cabal "clean" []
  cabal "build" []
  withDirectory "dep2" $ do
    -- Build from one of the subdirectories that uses toplevel cabal.project.
    cabal "clean" []
    cabal "build" []
