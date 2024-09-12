import Test.Cabal.Prelude

main = cabalTest $ withProjectFile "cabal.project" $ do
    void $ cabal' "build" ["--dry-run"]
    void $ cabal' "clean" []
