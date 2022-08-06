import Test.Cabal.Prelude

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
-- deterministic hash too.)
import Data.List (isInfixOf)
main = setupAndCabalTest $ do
    withPackageDb $ do
        -- Get env file paths.  We'll check 4 files to extract hashes.
        env <- getTestEnv
        let
            workDir = testWorkDir env
            dyn1Path = workDir </> "dyn1.env"
            dyn2Path = workDir </> "dyn2.env"
            static1Path = workDir </> "static1.env"
            static2Path = workDir </> "static2.env"

        -- 4 phases.  We'll collect the results in ‘setup.dist/*.env’, and make
        -- sure they look fine afterward.

        -- Phase 1: dynamic, first sample.
        dyn1Result <- do
            withDirectory "basic" $ do
                cabal "v2-install" $
                    [
                        "--lib",
                        "--package-env=" ++ dyn1Path
                    ] ++
                    [
                        "--enable-shared",
                        "--enable-executable-dynamic",
                        "--disable-library-vanilla"
                    ]

        -- Phase 2: static, first sample.
        static1Result <- do
            withDirectory "basic" $ do
                cabal "v2-install" $
                    [
                        "--lib",
                        "--package-env=" ++ static1Path
                    ] ++
                    [
                    ]

        -- Phase 3: dynamic, second sample.
        dyn2Result <- do
            withDirectory "basic" $ do
                cabal "v2-install" $
                    [
                        "--lib",
                        "--package-env=" ++ dyn2Path
                    ] ++
                    [
                        "--enable-shared",
                        "--enable-executable-dynamic",
                        "--disable-library-vanilla"
                    ]

        -- Phase 4: static, second sample.
        static2Result <- do
            withDirectory "basic" $ do
                cabal "v2-install" $
                    [
                        "--lib",
                        "--package-env=" ++ static2Path
                    ] ++
                    [
                    ]

        -- Now read the environment files.
        let
            extract path = do
                contents <- liftIO $ readFile path
                let
                    ls = lines contents
                    prefix = "package-id basic-1.0-"
                    basics = map (drop $ length prefix) . filter ("package-id basic-1.0-" `isInfixOf`) $ ls
                line <- case basics of
                    [extraction] -> return extraction
                    _ -> do
                        (>> return "ERROR") . assertFailure . unlines $
                            [
                                "Error: failed to find the ‘basic-1.0’ hash from ‘" ++ path ++ "’.",
                                "\tMake sure the suffix includes a hash and not just the version.",
                                "\tAlso make sure there is exactly 1 line starting with ‘" ++ (prefix) ++ "’ (found " ++ (show (length basics)) ++ ")."
                            ]
                return line
        dyn1UID    <- extract dyn1Path
        static1UID <- extract static1Path
        dyn2UID    <- extract dyn2Path
        static2UID <- extract static2Path

        -- First make sure the two samples are deterministic.
        -- (Non-essential test.)
        when (dyn1UID /= dyn2UID) $ do
            assertFailure . unlines $
                [
                    "Error: dyn1UID /= dyn2UID: ‘" ++ dyn1UID ++ "’ /= ‘" ++ dyn2UID ++ "’."
                ]
        when (static1UID /= static2UID) $ do
            assertFailure . unlines $
                [
                    "Error: dyn1UID /= dyn2UID: ‘" ++ dyn1UID ++ "’ /= ‘" ++ dyn2UID ++ "’."
                ]

        -- Now make sure dyn and static are different.  What we're mainly testing.
        when (dyn1UID == static1UID) $ do
            assertFailure . unlines $
                [
                    "Error: dyn1UID == static1UID: ‘" ++ dyn1UID ++ "’ == ‘" ++ static1UID ++ "’.",
                    "\tThese packages should have been configured with different config flags",
                    "\tproducing different hashes."
                ]

        return ()
