import Test.Cabal.Prelude
main = cabalTest $ do
    cabal' "v1-exec" ["echo", "find_me_in_output"]
        >>= assertOutputContains "find_me_in_output"
