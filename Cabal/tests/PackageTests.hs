{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

-- This is the runner for the package-tests suite.  The actual
-- tests are in in PackageTests.Tests

module Main where

import PackageTests.Options
import PackageTests.PackageTester
import PackageTests.Tests

import Distribution.Simple.Configure
    ( ConfigStateFileError(..), findDistPrefOrDefault, getConfigStateFile
    , interpretPackageDbFlags, configCompilerEx )
import Distribution.Simple.Compiler (PackageDB(..), PackageDBStack
                                    ,CompilerFlavor(GHC))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program (defaultProgramDb)
import Distribution.Simple.Setup (Flag(..), readPackageDbList, showPackageDbList)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Text (display)
import Distribution.Verbosity (normal, flagToVerbosity, lessVerbose)
import Distribution.ReadE (readEOrFail)
import Distribution.Compat.Time (calibrateMtimeChangeDelay)

import Control.Exception
import Data.Proxy                      ( Proxy(..) )
import Distribution.Compat.Environment ( lookupEnv )
import System.Directory
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Ingredients

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
    -- We could have the user pass these all in as arguments,  but
    -- there's a more convenient way to get this information: the *build
    -- configuration* that was used to build the Cabal library (and this
    -- test suite) in the first place.  To do this, we need to find the
    -- 'dist' directory that was set as the build directory for Cabal.

    -- First, figure out the dist directory associated with this Cabal.
    dist_dir :: FilePath <- guessDistDir

    -- Next, attempt to read out the LBI.  This may not work, in which
    -- case we'll try to guess the correct parameters.  This is ignored
    -- if values are explicitly passed into the test suite.
    mb_lbi <- getPersistBuildConfig_ (dist_dir </> "setup-config")

    -- You need to run the test suite in the right directory, sorry.
    -- This variable is modestly misnamed: this refers to the base
    -- directory of Cabal (so, CHECKOUT_DIR/Cabal, not
    -- CHECKOUT_DIR/Cabal/test).
    cabal_dir <- getCurrentDirectory

    -- TODO: make this controllable by a flag.  We do have a flag
    -- parser but it's not called early enough for this verbosity...
    verbosity <- maybe normal (readEOrFail flagToVerbosity)
                 `fmap` lookupEnv "VERBOSE"

    -------------------------------------------------------------------
    -- SETTING UP GHC AND GHC-PKG
    -------------------------------------------------------------------

    -- NOTE: There are TWO configurations of GHC we have to manage
    -- when running the test suite.
    --
    --  1. The primary GHC is the one that was used to build the
    --  copy of Cabal that we are testing.  This configuration
    --  can be pulled out of the LBI.
    --
    --  2. The "with" GHC is the version of GHC we ask the Cabal
    --  we are testing to use (i.e., using --with-compiler).  Notice
    --  that this does NOT have to match the version we compiled
    --  the library with!  (Not all tests will work in this situation,
    --  however, since some need to link against the Cabal library.)
    --  By default we use the same configuration as the one from the
    --  LBI, but a user can override it to test against a different
    --  version of GHC.
    mb_ghc_path     <- lookupEnv "CABAL_PACKAGETESTS_GHC"
    mb_ghc_pkg_path <- lookupEnv "CABAL_PACKAGETESTS_GHC_PKG"
    boot_programs <-
        case (mb_ghc_path, mb_ghc_pkg_path) of
            (Nothing, Nothing) | Just lbi <- mb_lbi -> do
                putStrLn "Using configuration from LBI"
                return (withPrograms lbi)
            _ -> do
                putStrLn "(Re)configuring test suite (ignoring LBI)"
                (_comp, _compPlatform, programDb)
                    <- configCompilerEx
                        (Just GHC) mb_ghc_path mb_ghc_pkg_path
                        -- NB: if we accept full ConfigFlags parser then
                        -- should use (mkProgramsConfig cfg (configPrograms cfg))
                        -- instead.
                        defaultProgramDb
                        (lessVerbose verbosity)
                return programDb

    mb_with_ghc_path     <- lookupEnv "CABAL_PACKAGETESTS_WITH_GHC"
    mb_with_ghc_pkg_path <- lookupEnv "CABAL_PACKAGETESTS_WITH_GHC_PKG"
    with_programs <-
        case (mb_with_ghc_path, mb_with_ghc_path) of
            (Nothing, Nothing) -> return boot_programs
            _ -> do
                putStrLn "Configuring test suite for --with-compiler"
                (_comp, _compPlatform, with_programs)
                    <- configCompilerEx
                        (Just GHC) mb_with_ghc_path mb_with_ghc_pkg_path
                        defaultProgramDb
                        (lessVerbose verbosity)
                return with_programs

    -------------------------------------------------------------------
    -- SETTING UP THE DATABASE STACK
    -------------------------------------------------------------------

    -- Figure out what database stack to use. (This is the tricky bit,
    -- because we need to have enough databases to make the just-built
    -- Cabal package well-formed).
    db_stack_env <- lookupEnv "CABAL_PACKAGETESTS_DB_STACK"
    let packageDBStack0 = case db_stack_env of
            Just str -> interpretPackageDbFlags True -- user install? why not.
                            (concatMap readPackageDbList
                                (splitSearchPath str))
            Nothing ->
                case mb_lbi of
                    Just lbi -> withPackageDB lbi
                    -- A wild guess!
                    Nothing -> interpretPackageDbFlags True []

    -- Package DBs are not guaranteed to be absolute, so make them so in
    -- case a subprocess using the package DB needs a different CWD.
    packageDBStack1 <- mapM canonicalizePackageDB packageDBStack0

    -- The LBI's database stack does *not* contain the inplace installed
    -- Cabal package.  So we need to add that to the stack.
    let package_db_stack
            = packageDBStack1 ++
              [SpecificPackageDB
                (dist_dir </> "package.conf.inplace")]

    -- NB: It's possible that our database stack is broken (e.g.,
    -- it's got a database for the wrong version of GHC, or it
    -- doesn't have enough to let us build Cabal.)  We'll notice
    -- when we attempt to compile setup.

    -- There is also is a parameter for the stack for --with-compiler,
    -- since if GHC is a different version we need a different set of
    -- databases.  The default should actually be quite reasonable
    -- as, unlike in the case of the GHC used to build Cabal, we don't
    -- expect htere to be a Cabal available.
    with_ghc_db_stack_env :: Maybe String
        <- lookupEnv "CABAL_PACKAGETESTS_WITH_GHC_DB_STACK"
    let withGhcDBStack0 :: PackageDBStack
        withGhcDBStack0 =
              interpretPackageDbFlags True
            $ case with_ghc_db_stack_env of
                Nothing -> []
                Just str -> concatMap readPackageDbList (splitSearchPath str)
    with_ghc_db_stack :: PackageDBStack
        <- mapM canonicalizePackageDB withGhcDBStack0

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

    -- Figure out how long we need to delay for recompilation tests
    (mtimeChange, mtimeChange') <- calibrateMtimeChangeDelay

    let suite = SuiteConfig
                 { cabalDistPref      = dist_dir
                 , bootProgramDb      = boot_programs
                 , withProgramDb      = with_programs
                 , packageDBStack     = package_db_stack
                 , withGhcDBStack     = with_ghc_db_stack
                 , suiteVerbosity     = verbosity
                 , absoluteCWD        = cabal_dir
                 , mtimeChangeDelay   = mtimeChange'
                 }

    let toMillis :: Int -> Double
        toMillis x = fromIntegral x / 1000.0

    putStrLn $ "Cabal test suite - testing cabal version "
      ++ display cabalVersion
    putStrLn $ "Cabal build directory: " ++ dist_dir
    putStrLn $ "Cabal source directory: " ++ cabal_dir
    putStrLn $ "File modtime calibration: " ++ show (toMillis mtimeChange')
            ++ " (maximum observed: " ++ show (toMillis mtimeChange) ++ ")"
    -- TODO: it might be useful to factor this out so that ./Setup
    -- configure dumps this file, so we can read it without in a version
    -- stable way.
    putStrLn $ "Environment:"
    putStrLn $ "CABAL_PACKAGETESTS_GHC=" ++ show (ghcPath suite) ++ " \\"
    putStrLn $ "CABAL_PACKAGETESTS_GHC_PKG=" ++ show (ghcPkgPath suite) ++ " \\"
    putStrLn $ "CABAL_PACKAGETESTS_WITH_GHC=" ++ show (withGhcPath suite) ++ " \\"
    putStrLn $ "CABAL_PACKAGETESTS_WITH_GHC_PKG=" ++ show (withGhcPkgPath suite) ++ " \\"
    -- For brevity, we use the pre-canonicalized values
    let showDBStack = show
                    . intercalate [searchPathSeparator]
                    . showPackageDbList
                    . uninterpretPackageDBFlags
    putStrLn $ "CABAL_PACKAGETESTS_DB_STACK=" ++ showDBStack packageDBStack0
    putStrLn $ "CABAL_PACKAGETESTS_WITH_DB_STACK=" ++ showDBStack withGhcDBStack0

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
getPersistBuildConfig_ :: FilePath -> IO (Maybe LocalBuildInfo)
getPersistBuildConfig_ filename = do
    eLBI <- try $ getConfigStateFile filename
    case eLBI of
      -- If the version doesn't match but we still got a successful
      -- parse, don't complain and just use it!
      Left (ConfigStateFileBadVersion _ _ (Right lbi)) -> return (Just lbi)
      Left _ -> return Nothing
      Right lbi -> return (Just lbi)

options :: [Ingredient]
options = includingOptions
          [Option (Proxy :: Proxy OptionEnableAllTests)] :
          defaultIngredients
