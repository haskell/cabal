import Test.Cabal.Prelude

main = setupAndCabalTest $ do

        configureResult <- setup' "configure" []
        sdistResult <- setup' "sdist" []

        -- Package check messages.
        let noBoundsWithNewVersionMsg =
              "Please specify whether the license is GPL-3ExactOnly or GPL-3OrLater."

        assertOutputDoesNotContain noBoundsWithNewVersionMsg configureResult

        assertOutputDoesNotContain "Distribution quality errors:" sdistResult
        assertOutputDoesNotContain "Distribution quality warnings:" sdistResult

        return ()
