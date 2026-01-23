import Test.Cabal.Prelude

-- Test that setup shows all the 'autogen-modules' warnings.
main = setupAndCabalTest $ do

        configureResult <- setup' "configure" []
        assertOutputDoesNotContain "MyDummy.hs doesn't exist" configureResult
        sdistResult <- fails $ setup' "sdist" []
        assertOutputContains "MyDummy.hs doesn't exist" sdistResult

        return ()
