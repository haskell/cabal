import Test.Cabal.Prelude
import Data.List (isInfixOf)

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

        -- Replace line breaks with spaces in the haystack so that we can search
        -- for a string that wraps lines.
        let lineBreakBlind = needleHaystack{txHaystack = txFwdBwdId{txFwd = lineBreaksToSpaces}}

        -- Asserts for the desired check messages after configure.
        assertOn isInfixOf lineBreakBlind libError1 checkResult
        assertOn isInfixOf lineBreakBlind libError2 checkResult

        return ()
