import Test.Cabal.Prelude

main = cabalTest . recordMode DoNotRecord $ do
    -- Building a target that contains whitespace
    cabal "test" ["regression-simple"]
    cabal "bench" ["regression-simple"]
