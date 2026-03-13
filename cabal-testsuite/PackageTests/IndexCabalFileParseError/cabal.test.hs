import Test.Cabal.Prelude

main = cabalTest $ withRepoNoUpdate "repo" $ do
    fails $ cabal "v2-update" []
