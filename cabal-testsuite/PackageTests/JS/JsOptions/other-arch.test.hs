import Test.Cabal.Prelude

main = do
    skipIfJavaScript
    cabalTest $ do
        -- Ensure the field `js-options` does not raise issues
        res <- cabal' "v2-run" ["demo"]
        assertOutputContains "foo_fallback" res
