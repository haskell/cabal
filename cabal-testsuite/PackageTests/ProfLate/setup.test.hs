import Test.Cabal.Prelude
-- Test building with profiling detail "late"
main = do
    setupAndCabalTest $ do
        skipUnless "no profiling libs" =<< hasProfiledLibraries
        setup_build ["--enable-profiling", "--profiling-detail=late"]
        --  ["--enable-profiling", "--profiling-detail=late-toplevel"]

