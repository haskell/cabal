import Test.Cabal.Prelude

main = cabalTest . void $ do
    -- Building a target that contains whitespace
    cabal' "v2-build" ["a app/Main.hs"]
