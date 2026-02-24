import Test.Cabal.Prelude


main = cabalTest $ withRepo "repo" $ do

    -- See also discussion in #11510
    expectBrokenIfOSXAndGhc "<= 8.10.7" 7209 $ do

        cabal "build" ["--enable-documentation"]

        env <- getTestEnv

        -- Check properties of executable component
        exeDir <- findDependencyInStore "exe"
        -- Documentation is enabled..
        assertFileDoesContain (exeDir </> "cabal-hash.txt") "documentation: True"
        -- But not built
        shouldDirectoryNotExist (exeDir </> "share" </> "doc")

        -- Check properties of library
        libDir <- findDependencyInStore "lib"
        -- Documentation is enabled..
        assertFileDoesContain (libDir </> "cabal-hash.txt") "documentation: True"
        -- and has been built
        shouldDirectoryExist (libDir </> "share" </> "doc")
