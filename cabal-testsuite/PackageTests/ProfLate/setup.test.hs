import Test.Cabal.Prelude
-- Test building with profiling detail "late"
main = do
    setupAndCabalTest $ do
        skipIfNoProfiledLibraries
        setup_build ["--enable-profiling", "--profiling-detail=late"]

