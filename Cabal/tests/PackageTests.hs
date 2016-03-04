-- The intention is that this will be the new unit test framework.
-- Please add any working tests here.  This file should do nothing
-- but import tests from other modules.
--
-- Stephen Blackheath, 2009

module Main where

import PackageTests.Options
import PackageTests.PackageTester
import PackageTests.Tests

import Distribution.Simple.Configure
    ( ConfigStateFileError(..), findDistPrefOrDefault, getConfigStateFile
    , interpretPackageDbFlags )
import Distribution.Simple.Compiler (PackageDB(..), PackageDBStack)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.Types (Program(..), programPath, programVersion)
import Distribution.Simple.Program.Builtin
    ( ghcProgram, ghcPkgProgram, haddockProgram )
import Distribution.Simple.Program.Db (requireProgram)
import Distribution.Simple.Setup (Flag(..), readPackageDbList, showPackageDbList)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Text (display, simpleParse)
import Distribution.Verbosity (normal, flagToVerbosity)
import Distribution.ReadE (readEOrFail)

import Control.Exception
import Data.Proxy                      ( Proxy(..) )
import Distribution.Compat.Environment ( lookupEnv )
import System.Directory
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Ingredients
import Data.Maybe

#if MIN_VERSION_base(4,6,0)
import System.Environment ( getExecutablePath )
#endif

main :: IO ()
main = do
    -- In abstract, the Cabal test suite makes calls to the "Setup"
    -- executable and tests the output of Cabal.  However, we have to
    -- responsible for building this executable in the first place,
    -- since (1) Cabal doesn't support a test-suite depending on an
    -- executable, so we can't put a "Setup" executable in the Cabal
    -- file and then depend on it, (2) we don't want to call the Cabal
    -- functions *directly* because we need to capture and save the
    -- stdout and stderr, and (3) even if we could do all that, we will
    -- want to test some Custom setup scripts, which will be specific to
    -- the test at hand and need to be compiled against Cabal.
    --
    -- To be able to build the executable, there is some information
    -- we need:
    --
    --      1. We need to know what ghc to use,
    --
    --      2. We need to know what package databases (plural!) contain
    --      all of the necessary dependencies to make our Cabal package
    --      well-formed.
    --
    -- We could have the user pass these all in as arguments (TODO: this
    -- should be an option),  but there's a more convenient way to get
    -- this information: the *build configuration* that was used to
    -- build the Cabal library (and this test suite) in the first place.
    -- To do this, we need to find the 'dist' directory that was set as
    -- the build directory for Cabal.

    dist_dir <- guessDistDir
    -- Might be bottom, if we can't figure it out.  If you override
    -- all the relevant parameters you might still succeed.
    lbi <- getPersistBuildConfig_ (dist_dir </> "setup-config")

    -- You need to run the test suite in the right directory, sorry.
    test_dir <- getCurrentDirectory

    -- Pull out the information we need from the LBI
    -- TODO: The paths to GHC are configurable by command line, but you
    -- have to be careful: some tests might depend on the Cabal library,
    -- in which case you REALLY need to have built and installed Cabal
    -- for the version that the test suite is being built against.  The
    -- easiest thing to do is make sure you built Cabal the same way as
    -- you will run the tests.
    let getExePathFromEnvOrLBI env_name prog = do
            r <- lookupEnv env_name
            case r of
                Nothing -> do
                    (conf, _) <- requireProgram normal prog (withPrograms lbi)
                    return (programPath conf)
                Just x -> return x
    -- It is too much damn work to actually properly configure it
    -- (Cabal will go off and probe GHC and we really aren't keen
    -- on doing this every time we run the test suite.)
    ghc_path     <- getExePathFromEnvOrLBI "CABAL_PACKAGETESTS_GHC" ghcProgram
    ghc_pkg_path <- getExePathFromEnvOrLBI "CABAL_PACKAGETESTS_GHC_PKG"
                    ghcPkgProgram
    haddock_path <- getExePathFromEnvOrLBI "CABAL_PACKAGETESTS_HADDOCK"
                    haddockProgram

    with_ghc_path     <- fromMaybe ghc_path
                         `fmap` lookupEnv "CABAL_PACKAGETESTS_WITH_GHC"

    ghc_version_env <- lookupEnv "CABAL_PACKAGETESTS_GHC_VERSION"
    ghc_version <- case ghc_version_env of
        Nothing -> do
            (ghcConf, _) <- requireProgram normal ghcProgram (withPrograms lbi)
            return (fromJust (programVersion ghcConf))
        Just str ->
            return (fromJust (simpleParse str))

    with_ghc_version <- do
         version <- programFindVersion ghcProgram normal with_ghc_path
         case version of
           Nothing -> error "Cannot determine version of GHC used for --with-ghc"
           Just v -> return v

    -- Package DBs are not guaranteed to be absolute, so make them so in
    -- case a subprocess using the package DB needs a different CWD.
    db_stack_env <- lookupEnv "CABAL_PACKAGETESTS_DB_STACK"
    let packageDBStack0 = case db_stack_env of
            Nothing -> withPackageDB lbi
            Just str -> interpretPackageDbFlags True -- user install? why not.
                            (concatMap readPackageDbList
                                (splitSearchPath str))
    packageDBStack1 <- mapM canonicalizePackageDB packageDBStack0

    -- The packageDBStack is worth some commentary.  The database
    -- stack we extract from the LBI will contain enough package
    -- databases to make the Cabal package well-formed.  However,
    -- it does not *contain* the inplace installed Cabal package.
    -- So we need to add that to the stack.
    let packageDBStack2
            = packageDBStack1 ++
              [SpecificPackageDB
                (dist_dir </> "package.conf.inplace")]

    -- THIS ISN'T EVEN MY FINAL FORM.  The package database stack
    -- controls where we install a package; specifically, the package is
    -- installed to the top-most package on the stack (this makes the
    -- most sense, since it could depend on any of the packages below
    -- it.)  If the test wants to register anything (as opposed to just
    -- working in place), then we need to have another temporary
    -- database we can install into (and not accidentally clobber any of
    -- the other stacks.)  This is done on a per-test basis.
    --
    -- ONE MORE THING. On the subject of installing the package (with
    -- copy/register) it is EXTREMELY important that we also overload
    -- the install directories, so we don't clobber anything in the
    -- default install paths.  VERY IMPORTANT.

    -- TODO: make this controllable by a flag
    verbosity <- maybe normal (readEOrFail flagToVerbosity)
                 `fmap` lookupEnv "VERBOSE"
        -- The inplaceDB is where the Cabal library was registered
        -- in place (and is usable.)  inplaceConfig is a convenient
        -- set of flags to make sure we make it visible.
    let suite = SuiteConfig
                 { cabalDistPref = dist_dir
                 , ghcPath = ghc_path
                 , ghcVersion = ghc_version
                 , ghcPkgPath = ghc_pkg_path
                 , withGhcPath = with_ghc_path
                 , withGhcVersion = with_ghc_version
                 , packageDBStack = packageDBStack2
                 , suiteVerbosity = verbosity
                 , absoluteCWD = test_dir
                 }

    putStrLn $ "Cabal test suite - testing cabal version "
      ++ display cabalVersion
    putStrLn $ "Cabal build directory: " ++ dist_dir
    putStrLn $ "Test directory: " ++ test_dir
    -- TODO: it might be useful to factor this out so that ./Setup
    -- configure dumps this file, so we can read it without in a version
    -- stable way.
    putStrLn $ "Environment:"
    putStrLn $ "CABAL_PACKAGETESTS_GHC=" ++ show ghc_path ++ " \\"
    putStrLn $ "CABAL_PACKAGETESTS_GHC_VERSION="
      ++ show (display ghc_version) ++ " \\"
    putStrLn $ "CABAL_PACKAGETESTS_GHC_PKG=" ++ show ghc_pkg_path ++ " \\"
    putStrLn $ "CABAL_PACKAGETESTS_WITH_GHC=" ++ show with_ghc_path ++ " \\"
    putStrLn $ "CABAL_PACKAGETESTS_HADDOCK=" ++ show haddock_path ++ " \\"
    -- For brevity, do pre-canonicalization
    putStrLn $ "CABAL_PACKAGETESTS_DB_STACK=" ++
                show (intercalate [searchPathSeparator]
                    (showPackageDbList (uninterpretPackageDBFlags
                                        packageDBStack0)))

    -- Create a shared Setup executable to speed up Simple tests
    putStrLn $ "Building shared ./Setup executable"
    rawCompileSetup verbosity suite [] "tests"

    defaultMainWithIngredients options $
        runTestTree "Package Tests" (tests suite)

-- Reverse of 'interpretPackageDbFlags'.
-- prop_idem stk b
--      = interpretPackageDbFlags b (uninterpretPackageDBFlags stk) == stk
uninterpretPackageDBFlags :: PackageDBStack -> [Maybe PackageDB]
uninterpretPackageDBFlags stk = Nothing : map (\x -> Just x) stk

-- | Guess what the 'dist' directory Cabal was installed in is.  There's
-- no 100% reliable way to find this, but there are a few good shots:
--
--     1. Test programs are ~always built in-place, in a directory
--        that looks like dist/build/package-tests/package-tests;
--        thus the directory can be determined by looking at $0.
--        This method is robust against sandboxes, Nix local
--        builds, and Stack, but doesn't work if you're running
--        in an interpreter.
--
--     2. We can use the normal input methods (as per Cabal),
--        checking for the CABAL_BUILDDIR environment variable as
--        well as the default location in the current working directory.
--
-- NB: If you update this, also update its copy in cabal-install's
-- IntegrationTests
guessDistDir :: IO FilePath
guessDistDir = do
#if MIN_VERSION_base(4,6,0)
    -- Method (1)
    -- TODO: this needs to be BC'ified, probably.
    exe_path <- canonicalizePath =<< getExecutablePath
    -- exe_path is something like /path/to/dist/build/package-tests/package-tests
    let dist0 = dropFileName exe_path </> ".." </> ".."
    b <- doesFileExist (dist0 </> "setup-config")
#else
    let dist0 = error "no path"
        b = False
#endif
    -- Method (2)
    if b then canonicalizePath dist0
         else findDistPrefOrDefault NoFlag >>= canonicalizePath

canonicalizePackageDB :: PackageDB -> IO PackageDB
canonicalizePackageDB (SpecificPackageDB path)
    = SpecificPackageDB `fmap` canonicalizePath path
canonicalizePackageDB x = return x

-- | Like Distribution.Simple.Configure.getPersistBuildConfig but
-- doesn't check that the Cabal version matches, which it doesn't when
-- we run Cabal's own test suite, due to bootstrapping issues.
-- Here's the situation:
--
--      1. There's some system Cabal-1.0 installed.  We use this
--         to build Setup.hs
--      2. We run ./Setup configure, which uses Cabal-1.0 to
--         write out the LocalBuildInfo
--      3. We build the Cabal library, whose version is Cabal-2.0
--      4. We build the package-tests executable, which LINKS AGAINST
--         Cabal-2.0
--      5. We try to read the LocalBuildInfo that ./Setup configure
--         wrote out, but it's Cabal-1.0 format!
--
-- It's a bit skeevy that we're trying to read Cabal-1.0 LocalBuildInfo
-- using Cabal-2.0's parser, but this seems to work OK in practice
-- because LocalBuildInfo is a slow-moving data structure.  If
-- we ever make a major change, this won't work, and we'll have to
-- take a different approach (either setting "build-type: Custom"
-- so we bootstrap with the most recent Cabal, or by writing the
-- information we need in another format.)
getPersistBuildConfig_ :: FilePath -> IO LocalBuildInfo
getPersistBuildConfig_ filename = do
    eLBI <- try $ getConfigStateFile filename
    case eLBI of
      Left (ConfigStateFileBadVersion _ _ (Right lbi)) -> return lbi
      -- These errors are lazy!  We might not need these parameters.
      Left (ConfigStateFileBadVersion _ _ (Left err))
        -> return . error $
           "We couldn't understand the build configuration.  Try " ++
           "editing Cabal.cabal to have 'build-type: Custom' " ++
           "and then rebuilding, or manually specifying CABAL_PACKAGETESTS_* " ++
           "environment variables (see README.md for more details)." ++
           "\n\nOriginal error: " ++
           show err
      Left err -> return (throw err)
      Right lbi -> return lbi

options :: [Ingredient]
options = includingOptions
          [Option (Proxy :: Proxy OptionEnableAllTests)] :
          defaultIngredients
