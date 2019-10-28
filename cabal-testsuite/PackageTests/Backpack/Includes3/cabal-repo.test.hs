import Test.Cabal.Prelude
main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $ do
    withProjectFile "cabal.repo.project" $ do
      withRepo "repo" $ do
        cabalG ["--store-dir=" ++ storeDir] "v2-build" ["exe"]
