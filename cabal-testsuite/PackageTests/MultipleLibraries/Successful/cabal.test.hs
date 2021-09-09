import Test.Cabal.Prelude
main = cabalTest $
    cabal' "v2-run" ["pkg-abc:program"] >>= assertOutputContains "pkg-def:publib"

