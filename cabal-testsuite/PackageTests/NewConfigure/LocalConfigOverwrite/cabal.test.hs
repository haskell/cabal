import Test.Cabal.Prelude

main = cabalTest $
    withSourceCopy $ do
        r <- cabal' "v2-configure" []
        assertOutputContains "Now overwriting it" r
