import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnlessGhcVersion ">= 9.7"
    -- Parallel flag means output of this test is nondeterministic
    recordMode DoNotRecord $
        cabal "v2-build" ["-j", "--semaphore", "Semaphore"]
