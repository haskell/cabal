import Test.Cabal.Prelude

-- Test that setup shows all the 'autogen-modules' warnings.
main = setupAndCabalTest $ do
    setup' "configure" [] >>=
         assertOutputDoesNotContain "Warning: Instead of 'ghc-options: -I0' use 'include-dirs: 0'"
