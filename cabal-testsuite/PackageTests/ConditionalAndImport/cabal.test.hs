import Test.Cabal.Prelude
main = cabalTest $
  withRepo "repo" $ do
     -- this shouldn't succeed, but it does, because this doesn't override path correctly, sigh.
     fails $ withEnv [("PATH",Just "")] $ cabal "v2-sdist" []
     withEnv [("PATH", Just "")] $ cabal "v2-sdist" ["--project-file=boring.project"]

     cabal "v2-run" [ "some-exe" ]
     fails $ cabal "v2-build" [ "--project=cabal-cyclical.project" ]
     fails $ cabal "v2-build" [ "--project=cabal-bad-conditional.project" ]
