import Test.Cabal.Prelude
main = cabalTest $ do
    -- Parallel flag means output of this test is nondeterministic
    recordMode DoNotRecord $
        cabal "v2-build" ["-j", "T3460"]
