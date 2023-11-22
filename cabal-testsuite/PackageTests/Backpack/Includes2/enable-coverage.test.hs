import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    -- #6397
    cabal "test" ["--enable-coverage"]
