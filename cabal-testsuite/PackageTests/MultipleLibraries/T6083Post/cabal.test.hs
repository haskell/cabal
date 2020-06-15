import Test.Cabal.Prelude

-- https://github.com/haskell/cabal/issues/6083
-- see pkg-abc.cabal
main = cabalTest $
    cabal' "v2-run" ["pkg-abc:program"] >>= assertOutputContains "pkg-def:pkg-def"
