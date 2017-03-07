import Test.Cabal.Prelude
main = cabalTest $ do
    -- Don't run this test unless the GHC is sufficiently recent
    -- to not ship boot old-time/old-locale
    skipUnless =<< ghcVersionIs (>= mkVersion [7,11])
    withRepo "repo" $ do
        cabal "new-build" ["a"]
