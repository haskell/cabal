import Test.Cabal.Prelude
main = cabalTest $ do
    withSandbox $ do
        fails $ cabal "v1-exec" ["my-executable"]
        cabal "v1-install" []
        -- Execute indirectly via bash to ensure that we go through $PATH
        cabal' "v1-exec" ["sh", "--", "-c", "my-executable"]
            >>= assertOutputContains "This is my-executable"
