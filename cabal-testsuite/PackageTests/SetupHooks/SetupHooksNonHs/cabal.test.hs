import Test.Cabal.Prelude
main = cabalTest $ do
  cabal "run" []
  withDirectory "inner" $
    cabal "run" ["exe:NonHs"]
