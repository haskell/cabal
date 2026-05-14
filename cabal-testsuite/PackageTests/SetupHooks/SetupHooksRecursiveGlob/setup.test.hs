import Test.Cabal.Prelude

main =
  setupTest $ do
    setup "configure" []
    setup "build" []
    -- Modify one file in the temp copy (testCurrentDir) and rebuild; only
    -- the preprocessor rule for Foo.B should re-run, not the one for A.
    writeSourceFile "src/Foo/B.ppExt" "module Foo.B where\nModified text"
    setup "build" []
