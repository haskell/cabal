import Test.Cabal.Prelude

-- Build and install a package dynamically only, then build and install a
-- package statically that depends on that dynamic package.  Old cabals are
-- tempted to consider both the source package and the installed package
-- (IPI) option with dynamic-only flags as valid, so they normally construct a
-- build plan with this IPI option that results in a build error like the
-- following:
-- > [1 of 1] Compiling Main             ( Main.hs, ../setup.dist/work/depender/dist/build/depender/depender-tmp/Main.o )
-- >
-- > Main.hs:3:1: error:
-- >     Could not find module `Dynamic'
-- >     There are files missing in the `dynamic-1.0' package,
-- >     try running 'ghc-pkg check'.
-- >     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- >   |
-- >   | import qualified Dynamic (number)
-- >   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--
-- However, with ‘--require-artifacts’ rather than ‘--no-require-artifacts’,
-- cabal will detect this error in advance and reject this particular IPI,
-- leaving only building the source package as the only valid package option
-- (I) we can choose as an assignment for the QPN (and any other valid IPIs if
-- there were multiple pre-installed packages to choose from, including those
-- with configure flags that work for us).

import Data.Maybe (fromMaybe)  -- for ghcPkg1'
import Data.Version
import System.Directory
import System.FilePath

main = do
    cabalTest $ do
        -- Skip if on Windows, since my default Chocolatey Windows setup (and the CI
        -- server setup at the time, presumably) lacks support for dynamic builds
        -- since the base package appears to be static only, lacking e.g. ‘.dyn_o’
        -- files.  Normal Windows installations would need suport for dynamic
        -- builds, or else this test would fail when it tries to build with the
        -- dynamic flags.
        skipIfWindows

        withPackageDb $ do
            -- If ghc-pkg is too old, cabal-install still works but has the
            -- same bug which we fixed, and our test would fail.  Skip.
            skipIfOldGhcPkg

            -- Build a package with only dynamic build artifacts.
            sdistRepoDir <- (</> "dynamic-sdist-repo") . testWorkDir <$> getTestEnv
            installDynamic sdistRepoDir

            -- TODO: Before building a package that depends on this, just
            -- double check that we actually have an IPI in the same packageDB
            -- that will be used so that cabal-install will see it and be tempted.

            -- Build a package that requires static build artifacts.  Old
            -- cabal-installs don't bother to check static and dynamic
            -- configuration, so it'll probably produce a build plan that'll
            -- fail as we described above.  With the build artifact checker,
            -- our pre-installed IPI option we made earlier is detected to not
            -- be a valid option in advance, so rather than producing a build
            -- plan we know will fail, instead reject this particular option,
            -- so that the moduler resolver cabal-install uses only picks the
            -- only valid option left, which is to build from source.  (For our
            -- test to work, we need the depender build to be aware of both the
            -- pre-installed option and also the source package so that it can
            -- rebuild from source with the correct flags, so that the
            -- bug/enhancement scenario can be reproduced.)
            installDepender sdistRepoDir

-- Run ‘ghc-pkg field base pkg-vanilla-lib’ to test whether the ghc-pkg
-- we are using is new enough to support the 5 new IPI fields in the ‘.conf’
-- files.  If ghc-pkg is too old, then its Cabal-syntax dependency
-- (cabal-install also uses Cabal-syntax for the IPI fields) will emit an
-- ‘Unknown field’ warning if cabal-install tries to update or register an IPI
-- with new fields, but it should otherwise work besides having full
-- functionality of the artifact checker.
skipIfOldGhcPkg :: TestM ()
skipIfOldGhcPkg = do
    control <- resultExitCode <$> ghcPkg1' "field" ["*", "id"]
    hasArts <- resultExitCode <$> ghcPkg1' "field" ["*", "pkg-vanilla-lib"]

    -- cabal-install will still work without these 5 build artifact fields,
    -- except the artifact checker wouldn't detect missing artifacts
    -- without knowing what artifacts installed packages provide.
    skipIf "ghc-pkg too old for 5 arts fields" $ hasArts /= control

-- ghcPkg' that can return non-zero.
--
-- It's basically a copy except without ‘requireSuccess’.
ghcPkg1' :: String -> [String] -> TestM Result
ghcPkg1' cmd args = do
    env <- getTestEnv
    unless (testHavePackageDb env) $
        error "Must initialize package database using withPackageDb"
    -- NB: testDBStack already has the local database
    ghcConfProg <- requireProgramM ghcProgram
    let db_stack = testPackageDBStack env
        extraArgs = ghcPkgPackageDBParams
                        (fromMaybe
                            (error "ghc-pkg: cannot detect version")
                            (programVersion ghcConfProg))
                        db_stack
    recordHeader ["ghc-pkg", cmd]
    runProgram1M ghcPkgProgram (cmd : extraArgs ++ args) Nothing
    where
        runProgram1M :: Program -> [String] -> Maybe String -> TestM Result
        runProgram1M prog args input = do
            configured_prog <- requireProgramM prog
            -- TODO: Consider also using other information from
            -- ConfiguredProgram, e.g., env and args
            run1M (programPath configured_prog) args input

        run1M :: FilePath -> [String] -> Maybe String -> TestM Result
        run1M path args input = do
            env <- getTestEnv
            r <- liftIO $ run (testVerbosity env)
                        (Just (testCurrentDir env))
                        (testEnvironment env)
                        path
                        args
                        input
            recordLog r
            return r

-- Flags.
-- (Swap the line comments to trigger the bug the artifect checker validation
-- step fixes - the ‘missing files’ error.)
--commonArgs = ["--disable-backup", "--no-require-artifacts"]
commonArgs = ["--disable-backup"]
dynamicArgs =
    [
        "--enable-shared",
        "--enable-executable-dynamic",
        "--disable-library-vanilla",
        "--disable-static",
        "--disable-executable-static"
    ]
staticArgs =
    [
        "--enable-static"
    ]

-- Build a package with only dynamic build artifacts.
installDynamic :: FilePath -> TestM ()
installDynamic sdistRepoDir = do
    withDirectory "dynamic" $ do
        withSourceCopyDir ("dyn") $ do
            cwd <- fmap testSourceCopyDir getTestEnv
            -- (Now do ‘cd ..’, since withSourceCopyDir made our previous
            -- previous such withDirectories now accumulate to be
            -- relative to cabal.dist/dyn, not testSourceDir
            -- (see 'testCurrentDir').)
            withDirectory ".." $ do
                -- Our project still resides in ‘dynamic/’.
                withDirectory "dynamic" $ do
                    cabal "v2-configure" $ [] ++ commonArgs ++ dynamicArgs
                    cabal "v2-build" $ []
                    recordMode DoNotRecord $ do
                        cabal "v2-install" $ ["--lib"] ++ commonArgs ++ dynamicArgs
                    tmpBuildDir <- (</> "dynamic-sdist-build") . testWorkDir <$> getTestEnv
                    cabal "v2-sdist" $ ["-o", sdistRepoDir, "--builddir", tmpBuildDir]

-- Build a package that requires static build artifacts.  (The same packageDB
-- is shared.)
installDepender :: FilePath -> TestM ()
installDepender sdistRepoDir = do
    withDirectory "depender" $ do
        withSourceCopyDir ("depr") $ do
            cwd <- fmap testSourceCopyDir getTestEnv
            -- (As before.)
            withDirectory ".." $ do
                withDirectory "depender" $ do
                    -- depender knows of the source package and the installed package.
                    -- The installed package should only have dynamic files (.dyn_hi,
                    -- .so), but not any static files (.a, .hi).  New ghc-pkg IPI file
                    -- fields track these, so with a new GHC, a new cabal-install
                    -- should reject the installed package while building the tree
                    -- (reason: missing build artifacts) and instead choose the sdist
                    -- (source package) so that it can figure out its own configuration
                    -- flags.
                    --
                    -- (For instance, if you comment out the sdist references so that we
                    -- only see the installed package, you should see an error message
                    -- like this (e.g. remove those two ‘-- ’ strings to write out only
                    -- a ‘packages: ./../dep…’ line):)
                    -- > Error: cabal: Could not resolve dependencies:
                    -- > [__0] trying: depender-1.0 (user goal)
                    -- > [__1] next goal: dynamic (dependency of depender)
                    -- > [__1] rejecting: dynamic-1.0/installed-19c7c1e50b8f1e56115c4f668dfdadd7114fc2c7dad267c2df43028892cc0ff5 (missing build artifacts: static artifacts)
                    -- > [__1] fail (backjumping, conflict set: depender, dynamic)
                    -- > After searching the rest of the dependency tree exhaustively, these were the goals I've had most trouble fulfilling: depender (3), dynamic (2)

                    -- Setup the project file.
                    -- > sed -nEe 's/\{SDIST\}/…path…to…sdist…dir…/g; p' < cabal.project.in > cabal.project
                    writeSourceFile "cabal.project" . unlines $
                        [
                            "packages: ./../depender/*.cabal",
                            -- "" {-
                            "",
                            "repository my-local-repository",
                            "    url: file+noindex://" ++ sdistRepoDir ++ "#shared-cache"
                            --    -}
                        ]

                    -- Make sure our test scenario setup lets the depender see
                    -- the pre-installed dynamic package IPI we built.
                    guessedPackageDbPath <- do
                        recordMode DoNotRecord $ do
                            guessPackageDbPathDepender
                    let sharedPackageDbFlags = ["--package-db=" ++ guessedPackageDbPath]

                    -- Use 'staticArgs' here.
                    cabal "v2-configure" $ [] ++ commonArgs ++ staticArgs ++ sharedPackageDbFlags
                    recordMode DoNotRecord $ do
                        cabal "v2-build" $ [] ++ sharedPackageDbFlags

                    -- Optional: check the output.
                    recordMode DoNotRecord $ do
                        cabal "v2-install" $ [] ++ commonArgs ++ staticArgs
                    withPlan $ do
                        runPlanExe' "depender" "depender" []
                            >>= assertOutputContains "Dynamic's number is 3."

guessPackageDbPathDepender :: TestM FilePath
guessPackageDbPathDepender = do
    env <- getTestEnv
    hasGhc <- isAvailableProgram ghcProgram
    skipUnless "failed to guess package-db: couldn't find ghc" hasGhc
    tryProgramVersion <- programVersion <$> requireProgramM ghcProgram
    let convertVersion = makeVersion . versionNumbers
    programVersion <- maybe (skip "failed to guess package-db: unknown ghc version" >> return "") return $ showVersion . convertVersion <$> tryProgramVersion
    path <- liftIO . canonicalizePath $ testCabalDir env </> "store" </> "ghc-" ++ programVersion </> "package.db"
    exists <- liftIO $ doesPathExist path
    skipUnless ("failed to guess package-db: guessed dir path does not exist: " ++ path) exists
    return path
