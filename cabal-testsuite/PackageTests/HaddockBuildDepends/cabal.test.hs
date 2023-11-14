import Test.Cabal.Prelude


main = cabalTest . withRepo "repo" $ do
    cabal "build" ["--enable-documentation"]

    env <- getTestEnv
    let storeDir = testCabalDir env </> "store"

    -- Check properties of executable component
    libDir <- liftIO $ findDependencyInStore storeDir "exe"
    -- Documentation is enabled..
    assertFileDoesContain (libDir </> "cabal-hash.txt") "documentation: True"
    -- But not built
    shouldDirectoryNotExist (  libDir </> "share" </> "doc" )

    -- Check properties of library
    libDir <- liftIO $ findDependencyInStore storeDir "lib"
    -- Documentation is enabled..
    assertFileDoesContain (libDir </> "cabal-hash.txt") "documentation: True"
    -- and has been built
    shouldDirectoryExist (  libDir </> "share" </> "doc" )
