import Test.Cabal.Prelude

main = cabalTest $ do
  -- Build from toplevel.
  cabal "build" []
  withDirectory "dep2" $ do
    -- Build from one of the subdirectories that uses toplevel cabal.project.
    cabal "build" []
