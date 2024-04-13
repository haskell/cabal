import Test.Cabal.Prelude
import Distribution.Simple.Utils

-- This test ensures the following fix holds:
-- > Fix project-local build flags being ignored.
-- >
-- > I noticed that running ‘cabal install’ with two separate sets of dynamic /
-- > static build flags (e.g. one with none, and one with ‘--enable-shared
-- > --enable-executable-dynamic --disable-library-vanilla’) produced packages with
-- > the same hash, instead of different hashes.
-- >
-- > After debugging this issue I found that this command (with no explicit cabal
-- > project file) was resulting in these build configuration flags being ignored,
-- > because in ProjectPlanning.hs, the sdist was not considered a local package, so
-- > the (non-shared) local-package-only configuration was being dropped.
-- >
-- > This fix ensures that these command-line arguments properly make it through to
-- > where they belong in cases like this.
--
-- Basically, take a simple package, build it under two sets of build flags:
-- > (nothing)
-- > --enable-shared --enable-executable-dynamic --disable-library-vanilla
--
-- And ensure that whereas before they produced the same hash, now the package
-- hashes produced are different.  (And also supplementarily ensure that
-- re-running the same build with the same flags a second time produces a
-- deterministic hash too; the hash should differ only when we change these
-- flags.)
--
-- Based on the UniqueIPID test.

import Control.Monad (forM, foldM_)
import Data.List (isPrefixOf, tails)

data Linking = Static | Dynamic deriving (Eq, Ord, Show)

links :: [Linking]
links = [Static, Dynamic]

linkConfigFlags :: Linking -> [String]
linkConfigFlags Static  =
    [
    ]
linkConfigFlags Dynamic =
    [
        "--enable-shared",
        "--enable-executable-dynamic",
        "--disable-library-vanilla"
    ]

lrun :: [Linking]
lrun = [Static, Dynamic, Static, Dynamic]

main = cabalTest $ do
    -- Skip if on Windows, since my default Chocolatey Windows setup (and the CI
    -- server setup at the time, presumably) lacks support for dynamic builds
    -- since the base package appears to be static only, lacking e.g. ‘.dyn_o’
    -- files.  Normal Windows installations would need support for dynamic
    -- builds, or else this test would fail when it tries to build with the
    -- dynamic flags.
    skipIfWindows

    env <- getTestEnv
    withPackageDb $ do
        -- Phase 1: get 4 hashes according to config flags.
        results <- forM (zip [0..] lrun) $ \(idx, linking) -> do
            liftIO $ copyDirectoryRecursive minBound (testCurrentDir env </> "basic") (testCurrentDir env </> "basic" ++ show idx)
            withDirectory ("basic" ++ show idx) $ do
                        packageEnv <- (</> ("basic" ++ show idx ++ ".env")) . testWorkDir <$> getTestEnv
                        let installOptions = ["--disable-deterministic", "--lib", "--package-env=" ++ packageEnv] ++ linkConfigFlags linking ++ ["basic"]
                        recordMode RecordMarked $ do
                            recordHeader $ "install options:" : installOptions
                            cabal "v2-install" installOptions
                            recordHeader $ "install options:" : installOptions
                            fails $ cabal "v2-install" installOptions
                            recordHeader $ "install options:" : "--force-reinstalls" : installOptions
                            cabal "v2-install" $ "--force-reinstalls" : installOptions
                        let exIPID s = takeWhile (/= '\n') . head . filter (\t -> any (`isPrefixOf` t) ["basic-0.1-", "bsc-0.1-"]) $ tails s
                        hashedIpid <- exIPID <$> liftIO (readFile packageEnv)
                        return $ ((idx, linking), hashedIpid)
        -- Phase 2: make sure we have different hashes iff we have different config flags.
        -- In particular make sure the dynamic config flags weren't silently
        -- dropped and ignored, since this is the bug that prompted this test.
        (\step -> foldM_ step (const $ return ()) results) $ \acc x -> do
            acc x
            return $ \future -> acc future >> do
                let
                    ((thisIdx,   thisLinking),   thisHashedIpid)   = x
                    ((futureIdx, futureLinking), futureHashedIpid) = future
                when ((thisHashedIpid == futureHashedIpid) /= (thisLinking == futureLinking)) $ do
                    assertFailure . unlines $
                        if thisLinking /= futureLinking
                            then
                                -- What we are primarily concerned with testing
                                -- here.
                                [
                                    "Error: static and dynamic config flags produced an IPID with the same hash; were the dynamic flags silently dropped?",
                                    "\thashed IPID: " ++ thisHashedIpid
                                ]
                            else
                                -- Help test our test can also make equal
                                -- hashes.
                                [
                                    "Error: config flags were equal, yet a different IPID hash was produced.",
                                    "\thashed IPID 1 : " ++ thisHashedIpid,
                                    "\thashed IPID 2 : " ++ futureHashedIpid,
                                    "\tlinking flags : " ++ show thisLinking
                                ]
