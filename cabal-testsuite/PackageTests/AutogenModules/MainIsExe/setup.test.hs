import Test.Cabal.Prelude

-- Test that setup shows all the 'autogen-modules' warnings.
main = setupAndCabalTest $ do

        configureResult <- setup' "configure" []
        assertOutputDoesNotContain "Could not find Haskell source file MyDummy.hs" configureResult
        sdistResult <- fails $ setup' "sdist" []
        assertOutputContains "Could not find Haskell source file MyDummy.hs" sdistResult

        return ()
