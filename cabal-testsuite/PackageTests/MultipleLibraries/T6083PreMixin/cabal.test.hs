import Test.Cabal.Prelude

-- This is like T6083Pre, but also goes via mixins
--
main = cabalTest $ do
    -- https://github.com/haskell/cabal/pull/6047 should make this work for older GHCs too?
    skipIf =<< ghcVersionIs (< mkVersion [8,8])

    cabal' "v2-run" ["pkg-abc:program"] >>= assertOutputContains "pkg-abc:pkg-def"
