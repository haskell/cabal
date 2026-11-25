import Test.Cabal.Prelude

-- --offline refuses to download from a remote repository, so the package
-- has to come from a real (secure, http-served) one; a file+noindex
-- repository is a local directory and is not refused.
main =
  skipIfCIAndWindows 10230 >> cabalTest (flakyIfCI 9530 $ withShorterPathForNewBuildStore $ do
    skipUnlessGhcVersion ">= 8.1"
    withProjectFile "cabal.repo.project" $ do
      withRemoteRepo "repo" $ do
        -- The update output carries the index-state timestamp of the run.
        recordMode DoNotRecord $ cabal "v2-update" []
        fails (cabal' "v2-build" ["current", "--offline"])
          >>= assertOutputContains "refusing to download the package: remote"
        cabal "v2-build" ["current"]
        cabal "v2-build" ["current", "--offline"])
