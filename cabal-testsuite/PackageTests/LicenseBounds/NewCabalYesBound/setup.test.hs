import Test.Cabal.Prelude

main = setupAndCabalTest $ do

        configureResult <- setup' "configure" []
        sdistResult <- setup' "sdist" []

        assertOutputDoesNotContain "Distribution quality errors:" sdistResult
        assertOutputDoesNotContain "Distribution quality warnings:" sdistResult

        return ()
