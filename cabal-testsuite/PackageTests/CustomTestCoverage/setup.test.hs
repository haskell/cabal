import Test.Cabal.Prelude
main = setupTest $ do
  recordMode DoNotRecord $ do
    setup' "configure" ["--enable-tests", "--enable-coverage"] >>= assertOutputContains "ThisIsCustomYeah"
    setup' "build"     []
    setup' "test"      [] >>= assertOutputContains "Package coverage report written to"
