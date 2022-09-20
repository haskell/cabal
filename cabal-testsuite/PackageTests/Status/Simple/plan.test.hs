import           Test.Cabal.Prelude

main = cabalTest $ do
    -- Make sure plan.json is generated, even if no target is resolved
    cabal "status" ["--output-format=json", "--target", "src/Main.hs"]
    withPlan $ do
        pure ()
