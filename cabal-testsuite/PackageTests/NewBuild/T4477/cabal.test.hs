import Test.Cabal.Prelude
main = cabalTest $ do
    expectBroken 4477 $ do
        cabal' "new-run" ["foo"] >>= assertOutputContains "Hello World"
