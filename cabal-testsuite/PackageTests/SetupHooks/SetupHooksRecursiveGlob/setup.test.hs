import Test.Cabal.Prelude
main = setupTest $ do
  setup "configure" []
  setup "build" []
  -- Now modify one file and build again to see if that exact file gets rebuilt
  env <- getTestEnv
  liftIO $ do
    let srcDir = testSourceDir env
    writeFile (srcDir </> "src/Foo/B.hs") "module Foo.B where\nModified text"
  setup "build" []
