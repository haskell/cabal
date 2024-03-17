import Test.Cabal.Prelude

main =
    cabalTest $
      expectBroken 0 $
        withProjectFile "cabal.project" $
          withRepo "repo" $
            cabal "v2-build" ["libA"]

