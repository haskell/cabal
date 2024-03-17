import Test.Cabal.Prelude

main =
  cabalTest $ withShorterPathForNewBuildStore $ do
    skipUnlessGhcVersion ">= 8.1"
    skipIfWindows
    withProjectFile "cabal.repo.project" $ do
      withRepo "repo" $ do
        fails $ cabal "v2-build" ["current", "--offline"]
        cabal "v2-build" ["current"]
        cabal "v2-build" ["current", "--offline"]
