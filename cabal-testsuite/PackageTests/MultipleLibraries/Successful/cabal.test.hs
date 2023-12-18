import Test.Cabal.Prelude
main = cabalTest $ do
    cabal' "v2-run" ["pkg-abc:program"] >>= assertOutputContains "pkg-def:publib"
    return ()
