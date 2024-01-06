import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    skipIfWindows -- TODO: https://github.com/haskell/cabal/issues/6271
    -- #6397
    cabal "test" ["--enable-coverage", "includes2-test"]
