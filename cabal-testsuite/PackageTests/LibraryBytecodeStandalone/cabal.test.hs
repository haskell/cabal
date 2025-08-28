import Test.Cabal.Prelude

-- Test that bytecode libraries cannot be the only build artifacts
main = cabalTest $ do
    skipUnlessGhcVersion ">= 9.15"

    res <-
        fails $
            cabal'
                "v2-build"
                [ "--enable-library-bytecode"
                , "--disable-library-vanilla"
                , "--disable-shared"
                ]

    assertOutputContains
        "The ecosystem doesn't support building packages with just bytecode libraries yet."
        res
