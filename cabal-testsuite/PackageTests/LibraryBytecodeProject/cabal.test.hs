import Test.Cabal.Prelude

-- Ensure project-level library-bytecode flag builds bytecode artifacts
main = cabalTest $ do
    skipUnlessGhcVersion ">= 9.15"
    cabal "v2-build" []
    bytecodeLibs <- assertGlobMatchesTestDir testDistDir "**/*.bytecodelib"
    when (null bytecodeLibs) $
        assertFailure "Expected to find at least one .bytecodelib file when library-bytecode is enabled via cabal.project"
