import Test.Cabal.Prelude
main = cabalTest $
    withSourceCopy $
        cabal "new-configure" []
