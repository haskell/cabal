import Test.Cabal.Prelude
main = cabalTest $ withShorterPathForNewBuildStore $ do
    skipUnlessGhcVersion ">= 8.1"
    skipIfWindows -- TODO: https://github.com/haskell/cabal/issues/6271
    withProjectFile "cabal.repo.project" $ do
      withRepo "repo" $ do
        cabal "v2-build" ["exe"]
