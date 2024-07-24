import Test.Cabal.Prelude

main = cabalTest $ flaky 10182 $ withProjectFile "cabal.project" $ do
    cabal' "build" ["--dry-run"]
    cabal' "clean" []
    return ()
