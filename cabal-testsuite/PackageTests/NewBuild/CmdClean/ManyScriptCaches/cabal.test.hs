import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing, listDirectory)

-- Cleaning looks at the @scriptlocation@ file of every script build cache. It
-- must not hold them all open at once, as there can be more caches than the
-- limit on open files.
main = cabalTest $ do
    liftIO $ skipIfWindows "has no limit on open files to lower"
    env <- getTestEnv
    let cacheDir = testCabalDir env </> "script-builds"
        limit = 256

    -- Each cache is for a script that doesn't exist, so cleaning deletes it.
    forM_ [1 .. limit + 1] $ \i -> liftIO $ do
        let cache = cacheDir </> ("cache-" ++ show i)
        createDirectoryIfMissing True cache
        writeFile (cache </> "scriptlocation") (testCurrentDir env </> ("script-" ++ show i) <.> "hs")

    withOpenFilesLimit limit $ cabal "v2-clean" []

    caches <- liftIO $ listDirectory cacheDir
    assertEqual "script build caches left after cleaning" [] caches
