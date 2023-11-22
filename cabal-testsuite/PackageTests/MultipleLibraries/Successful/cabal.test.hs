import Test.Cabal.Prelude
main = cabalTest $ do
    cabal' "v2-run" ["pkg-abc:program"] >>= assertOutputContains "pkg-def:publib"

    -- #8609
    expectBroken 8609 $
      cabal' "v2-test" ["--enable-coverage", "all"]

    return ()
