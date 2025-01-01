import Test.Cabal.Prelude

main = do
  cabalTest' "default-all" $ do
    cabal "clean" []
    cabal "v2-target" []

  cabalTest' "explicit-all" $ do
    cabal "clean" []
    cabal "v2-target" ["all"]
