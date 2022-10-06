import Test.Cabal.Prelude
-- Test building with profiling detail "late"
main = do
    cabalTest $ do
        skipUnlessGhcVersion ">= 9.4"
        cabal' "clean" []
        res <- cabal' "build" ["-v2", "profLate", "--enable-profiling", "--profiling-detail=late"]
        assertOutputContains "-fprof-late" res

