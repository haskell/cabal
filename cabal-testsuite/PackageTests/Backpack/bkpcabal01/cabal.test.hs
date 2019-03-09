import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,2])
    cabal "v2-build" ["all"]
