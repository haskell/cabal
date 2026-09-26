import Test.Cabal.Prelude

-- Test that --with-build-compiler is accepted and the build succeeds.
-- Requires --with-build-compiler to be passed to the test runner; skipped otherwise.
main = cabalTest . recordMode DoNotRecord $ do
    withBuildCompiler $ \bc ->
        cabal "v2-build" ["--with-build-compiler=" ++ bc, "all"]
