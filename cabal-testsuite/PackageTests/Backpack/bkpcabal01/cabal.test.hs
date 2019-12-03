import Test.Cabal.Prelude
main = cabalTest $ do
    -- GHC 8.2.2 had a regression ("unknown package: hole"), see also #4908
    skipUnless =<< ghcVersionIs (\v -> v >= mkVersion [8,2] && v /= mkVersion [8,2,2])
    skipIf =<< isWindows -- TODO: https://github.com/haskell/cabal/issues/6271
    cabal "v2-build" ["all"]
