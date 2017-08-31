import Test.Cabal.Prelude

main = setupAndCabalTest $ do

        configureResult <- setup' "configure" []
        sdistResult <- setup' "sdist" []

        -- Package check messages.
        let usedBoundsWithOldVersionMsg =
              "Specifying whether the license version is 'only this exact version'"

        assertOutputContains usedBoundsWithOldVersionMsg configureResult

        assertOutputContains "Distribution quality errors:" sdistResult
        assertOutputContains usedBoundsWithOldVersionMsg sdistResult

        assertOutputDoesNotContain "Distribution quality warnings:" sdistResult

        return ()
