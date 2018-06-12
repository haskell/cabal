import Test.Cabal.Prelude
main = cabalTest $ do
    withSandbox $ do
        fails $ cabal "v1-exec" ["my-executable"]
        cabal "v1-install" []
        cabal' "v1-exec" ["my-executable"]
            >>= assertOutputContains "This is my-executable"
