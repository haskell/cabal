import Test.Cabal.Prelude

main = cabalTest $ do
    skipIfJavaScript
    -- Ensure the field `js-sources` does not raise issues
    res <- cabal' "v2-run" ["demo"]
    assertOutputContains "Hello Not JS!" res
