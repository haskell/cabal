import Test.Cabal.Prelude

import Control.Monad.Catch (finally)

main =
  setupTest $ do
    runSetupTest `finally` cleanup
  where
    runSetupTest = do
        setup "configure" []
        setup "build" []
        -- Now modify one file and build again to see if that exact file gets rebuilt
        env <- getTestEnv
        liftIO $ do
          let srcDir = testSourceDir env
          writeFile (srcDir </> "src/Foo/B.ppExt") "module Foo.B where\nModified text"
        setup "build" []
    cleanup = do
      env <- getTestEnv
      let srcDir = testSourceDir env
      liftIO $ writeFile (srcDir </> "src/Foo/B.ppExt") "module Foo.B where\nOriginal text\n"
