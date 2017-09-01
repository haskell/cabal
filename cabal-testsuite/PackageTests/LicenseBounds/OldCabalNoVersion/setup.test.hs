import Test.Cabal.Prelude

main = setupAndCabalTest $ do

        configureResult <- setup' "configure" []
        sdistResult <- setup' "sdist" []

        assertOutputDoesNotContain "Distribution quality warnings:" sdistResult
        assertOutputDoesNotContain "Distribution quality errors:" sdistResult

        return ()
