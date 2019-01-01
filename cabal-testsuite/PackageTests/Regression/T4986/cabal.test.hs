import Test.Cabal.Prelude
main = cabalTest $
    withSourceCopy $
        cabal "v2-configure" []
