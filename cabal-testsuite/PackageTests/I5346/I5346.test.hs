import Test.Cabal.Prelude
main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    skipIfWindows
    withProjectFile "cabal.repo.project" $ do
      withRepo "repo" $ do
        cabalG ["--store-dir=" ++ storeDir] "v2-build" ["current", "--offline"]
