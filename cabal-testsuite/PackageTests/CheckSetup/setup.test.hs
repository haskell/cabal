import Test.Cabal.Prelude

-- Test that setup shows all the 'autogen-modules' warnings.
main = cabalTest $ do

        checkResult <- fails $ cabal_raw' ["check"] Nothing

        -- Package check messages.
        let libError1 =
              "The dependency 'setup-depends: 'Cabal' does not specify "
              ++ "an upper bound on the version number"
            libError2 =
              "The dependency 'setup-depends: 'base' does not specify "
              ++ "an upper bound on the version number"

        -- Asserts for the desired check messages after configure.
        assertOutputContains libError1 checkResult
        assertOutputContains libError2 checkResult

        return ()
