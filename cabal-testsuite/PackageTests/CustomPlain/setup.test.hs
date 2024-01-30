import Test.Cabal.Prelude
main = setupTest $ do
    setup' "configure" [] >>= assertOutputContains "ThisIsCustomYeah"
    setup' "build"     [] >>= assertOutputContains "ThisIsCustomYeah"
