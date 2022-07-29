import Test.Cabal.Prelude

-- This test shows how 2022-07-28 Archlinux Haskell with a standard ghc and
-- cabal-install fails to build e.g. even attoparsec.  Essentially, the system
-- packages strip away static libraries and files, and build with only dynamic
-- files.
--
-- (ghc-static provides its own custom packagedb location, in e.g.
-- /usr/lib/static-package.conf.d rather than /usr/lib/package.conf.d, which
-- cabal and ghc doesn't know about unless you add it with --package-db.  But
-- the haskell-* libraries build with flags like
-- "--enable-shared --enable-executabledynamic --disable-library-vanilla".)
--
-- Then a vanilla cabal build will see these packages are installed, but when
-- it's trying to build with a "ghc" that has "-static", itthinks the packages
-- installs provide the files, but whereas it would compile if only with
-- "-dynamic", it fails for "-static" with errors like the following:
--
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
-- (A workaround to the system "haskell-*" packages lacking static libraries
-- (ghc-static provides some, though) without a fixed solver is to use stack
-- for everything.)

import Data.Version
import System.Directory
import System.FilePath

-- Simulate the above scenario but in our framework (without a cabal with the
-- project-local build flags fix, the test should still pass but not pass the
-- correct dynamic vs static flags through, so on these old cabals this test
-- would pass where it should fail because dynamic is being built with static
-- options enabled too; however, with a new enough cabal but an old GHC, the
-- build artifacts won't be threaded through the IPIs, so it should still fail
-- with an older GHC).
main = do
    cabalTest $ do
        -- Skip for < GHC 9.6; perhaps 9.6 will depend on a Cabal-syntax that
        -- provides the new IPI fields.
        skipUnlessGhcVersion ">= 9.6"
        env <- getTestEnv
        let
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

        -- Preprocess configuration.
        (sdistDir, sdistRepoDir) <- sdistDirs
        projectFilePath <- configureDepender env sdistRepoDir
        packageDbPath <- guessPackageDbPath

        -- Now test.
        let noBackup = ["--disable-backup"]
        withDirectory "dynamic" $ do
            -- Use 'dynamicArgs' here.
            cabal "v2-configure" $ [] ++ dynamicArgs ++ noBackup
            cabal "v2-build" []
            cabal "v2-install" $ ["--lib"] ++ dynamicArgs
            cabal "v2-sdist" ["-o", sdistRepoDir, "--builddir", sdistDir]
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
            -- like this:)
            -- > Error: cabal: Could not resolve dependencies:
            -- > [__0] trying: depender-1.0 (user goal)
            -- > [__1] next goal: dynamic (dependency of depender)
            -- > [__1] rejecting: dynamic-1.0/installed-19c7c1e50b8f1e56115c4f668dfdadd7114fc2c7dad267c2df43028892cc0ff5 (missing build artifacts: static artifacts)
            -- > [__1] fail (backjumping, conflict set: depender, dynamic)
            -- > After searching the rest of the dependency tree exhaustively, these were the goals I've had most trouble fulfilling: depender (3), dynamic (2)

            -- Use 'staticArgs' here.
            let projectFileArgs = ["--project-file=" ++ projectFilePath]
            let packageDbArgs = ["--package-db=" ++ packageDbPath]
            cabal "v2-configure" $ [] ++ projectFileArgs ++ staticArgs ++ noBackup
            cabal "v2-build" $ [] ++ projectFileArgs ++ packageDbArgs

            -- Optional: check the output.
            cabal "v2-install" $ [] ++ projectFileArgs ++ staticArgs
            ran <- runCabalInstalledExe' "depender" []
            assertOutputContains "Dynamic's number is 3." ran
    where
        sdistDirs = do
            env <- getTestEnv
            let distDir = testDistDir env
            let sdistDir = distDir </> "dynamic-dist"
            let sdistRepoDir = distDir </> "sdist"
            return (sdistDir, sdistRepoDir)

        guessPackageDbPath :: TestM FilePath
        guessPackageDbPath = do
            env <- getTestEnv
            tryProgramVersion <- programVersion <$> requireProgramM ghcProgram
            let convertVersion = makeVersion . versionNumbers
            ver <- maybe (error "guessPackageDbPath: unknown GHC version") return $ convertVersion <$> tryProgramVersion
            liftIO . canonicalizePath $ testCabalDir env </> "store" </> "ghc-" ++ (showVersion ver) </> "package.db"

        -- The purpose of this is to let ‘depender’ know of a *source repo*
        -- containing an sdist for ‘dynamic’.  (The other part, the IPI, is
        -- known through ‘--package-db=’.)
        --
        -- Use as e.g.
        -- > sed -nEe 's/\{SDIST\}/…path…to…sdist…dir…/g; p' < cabal.project.in > cabal.project
        writeProjectFile = writeFile

        -- Set up cabal project file (get its path).
        configureDepender env sdistRepoDir = do
            projectFilePath <- return $ testWorkDir env </> "cabal.project.depender"
            let
                dependerProjectFile :: String
                dependerProjectFile = unlines $
                    [
                        "packages: ./../depender/*.cabal",
                        "",
                        "repository my-local-repository",
                        "    url: file+noindex://" ++ sdistRepoDir ++ "#shared-cache"
                    ]
            liftIO $ writeProjectFile projectFilePath dependerProjectFile

            return projectFilePath

        -- Like 'runInstalledExe'' but with a fixed path.
        runCabalInstalledExe' :: String -> [String] -> TestM Result
        runCabalInstalledExe' exe_name args = do
            env <- getTestEnv
            defaultRecordMode RecordAll $ do
                recordHeader [exe_name]
                runM (testCabalDir env </> "bin" </> exe_name) args Nothing
