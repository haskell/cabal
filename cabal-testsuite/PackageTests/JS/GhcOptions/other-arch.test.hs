import Test.Cabal.Prelude

main = do
    skipIfJavaScript
    cabalTest $ do
        -- Ensure the field `js-sources` does not raise issues
        res <- cabal' "v2-run" ["demo"]
        assertOutputContains "Not JS foo" res
