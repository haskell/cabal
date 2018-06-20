import Test.Cabal.Prelude
main = cabalTest $ do
    skipIf =<< (ghcVersionIs (< mkVersion [7,10]))
    setup "configure" []
    setup "build" []
