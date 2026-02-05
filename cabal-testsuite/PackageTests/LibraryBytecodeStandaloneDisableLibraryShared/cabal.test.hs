import Test.Cabal.Prelude

main = cabalTest $ do
    skipUnlessGhcVersion ">= 9.15"

    res <-
        fails $
            cabal'
                "build"
                [ "--disable-library-vanilla"
                , "--disable-shared"
                , "--enable-library-bytecode"
                ]

    assertOutputContains
        "The ecosystem doesn't support building packages with just bytecode libraries yet."
        res
