import Test.Cabal.Prelude
main = cabalTest $ do
    -- #8609
    expectBroken 8609 $
      cabal' "v2-test" ["--enable-coverage", "all"]

    return ()
