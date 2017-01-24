import Test.Cabal.Prelude
-- Test unneed version bound on internal build-tools deps
main = cabalTest $ do
    cabal "new-build" ["client"]
