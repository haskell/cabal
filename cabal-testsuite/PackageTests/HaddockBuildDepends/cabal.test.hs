import Test.Cabal.Prelude


main = cabalTest . withRepo "repo" $ do
    cabal "build" ["--enable-documentation"]

    env <- getTestEnv

    -- Check properties of executable component
    libDir <- findDependencyInStore "exe"
    -- Documentation is enabled..
    assertFileDoesContain (libDir </> "cabal-hash.txt") "documentation: True"
    -- But not built
    shouldDirectoryNotExist (  libDir </> "share" </> "doc" )

    -- Check properties of library
    libDir <- findDependencyInStore "lib"
    -- Documentation is enabled..
    assertFileDoesContain (libDir </> "cabal-hash.txt") "documentation: True"
    -- and has been built
    shouldDirectoryExist (  libDir </> "share" </> "doc" )
