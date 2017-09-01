import Test.Cabal.Prelude

main = setupAndCabalTest $ do

        configureResult <- setup' "configure" []
        sdistResult <- setup' "sdist" []

        -- Package check messages.
        let noBoundsWithNewVersionMsg =
              "Please add a bound ('ExactOnly' or 'OrLater') to the 'license' field."

        assertOutputDoesNotContain "Distribution quality warnings:" sdistResult

        assertOutputContains "Distribution quality errors:" sdistResult
        assertOutputContains noBoundsWithNewVersionMsg sdistResult

        return ()
