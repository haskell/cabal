import Test.Cabal.Prelude

-- This is like T6083Pre, but also goes via mixins
--
main = cabalTest $
    cabal' "v2-run" ["pkg-abc:program"] >>= assertOutputContains "pkg-def:pkg-def"
