import Test.Cabal.Prelude

main = setupAndCabalTest $ do

        configureResult <- setup' "configure" []
        sdistResult <- setup' "sdist" []

        -- Package check messages.
        let noLicenseVersionWithNewVersionMsg =
              "When using 'cabal-version: >= 2.1' or later, the license 'GPL' requires a version"
        let noBoundsWithNewVersionMsg =
              "Please add a bound ('ExactOnly' or 'OrLater') to the 'license' field."

        assertOutputDoesNotContain "Distribution quality warnings:" sdistResult

        assertOutputContains "Distribution quality errors:" sdistResult
        assertOutputContains noLicenseVersionWithNewVersionMsg sdistResult
        assertOutputDoesNotContain noBoundsWithNewVersionMsg sdistResult

        return ()
