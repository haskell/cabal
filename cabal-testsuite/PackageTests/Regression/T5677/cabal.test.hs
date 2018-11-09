import Test.Cabal.Prelude
main = cabalTest $ do
  skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
  cabal "new-build" ["all"]

