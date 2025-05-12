import Test.Cabal.Prelude

main = cabalTest $ do
    -- NB: Uses cabal_raw' here to direct simulate what a user would write (#10772)
    res <- defaultRecordMode RecordMarked $ do
      recordHeader ["cabal", "v2-run"]
      cabal_raw' ["v2-run", "script.hs", marked_verbose] Nothing
    assertOutputContains "Hello World" res

    env      <- getTestEnv
    cacheDir <- getScriptCacheDirectory (testCurrentDir env </> "script.hs")
    liftIO $ print (testTmpDir env </> "build")

    shouldExist $ cacheDir </> "fake-package.cabal"
    shouldExist $ cacheDir </> "scriptlocation"
    shouldDirectoryNotExist $ testTmpDir env </> "build"
    -- "dist-newstyle" should exist, because the folder has a cabal.project in
    -- so the v2-run command runs in that context.
    shouldDirectoryExist $ testTmpDir env </> "dist-newstyle"
