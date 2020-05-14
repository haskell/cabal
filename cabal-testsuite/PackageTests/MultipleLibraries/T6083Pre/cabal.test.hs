import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/6083
-- see pkg-abc.cabal
main = cabalTest $ do
    -- https://github.com/haskell/cabal/pull/6047 should make this work for older GHCs too?
    skipIf =<< ghcVersionIs (< mkVersion [8,8])

    cabal' "v2-run" ["pkg-abc:program"] >>= assertOutputContains "pkg-abc:pkg-def"
