import Test.Cabal.Prelude

-- Test that --enable-library-bytecode builds bytecode artifacts
main = cabalTest $ do
    -- Bytecode artifacts are only supported in GHC >= 9.15.0
    skipUnlessGhcVersion ">= 9.15"

    -- Build with bytecode enabled
    cabal "v2-build" ["--enable-library-bytecode"]

    -- Check that the bytecode library file was created
    -- The bytecode library should be in dist-newstyle/build/.../build/libHS*.bytecodelib
    -- Look for the bytecode library file using a glob pattern
    bytecodeLibs <- assertGlobMatchesTestDir testDistDir "**/*.bytecodelib"

    -- Verify we found at least one bytecode library
    when (null bytecodeLibs) $
        assertFailure "Expected to find at least one .bytecodelib file after building with --enable-library-bytecode"

