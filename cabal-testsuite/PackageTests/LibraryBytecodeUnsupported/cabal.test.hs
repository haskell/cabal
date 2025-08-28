import Test.Cabal.Prelude

-- Test that --enable-library-bytecode is ignored on unsupported compilers
main = cabalTest $ do
    -- Bytecode artifacts are unavailable prior to GHC 9.15
    skipUnlessGhcVersion "< 9.15"

    -- Building with the flag should succeed but emit a warning
    res <- cabal' "v2-build" ["--enable-library-bytecode"]
    assertOutputContains
        "This compiler does not support bytecode libraries; ignoring --enable-library-bytecode"
        res

    -- No bytecode artifacts should be produced
    assertGlobDoesNotMatchTestDir testDistDir "**/*.bytecodelib"
