{-# LANGUAGE ScopedTypeVariables #-}

-- | The test monad
module Test.Cabal.Monad (
    -- * High-level runners
    setupAndCabalTest,
    setupTest,
    cabalTest,
    cabalTest',
    -- * The monad
    TestM,
    runTestM,
    -- * Helper functions
    programPathM,
    requireProgramM,
    isAvailableProgram,
    hackageRepoToolProgram,
    gitProgram,
    cabalProgram,
    diffProgram,
    python3Program,
    -- * The test environment
    TestEnv(..),
    getTestEnv,
    -- * Recording mode
    RecordMode(..),
    testRecordMode,
    -- * Derived values from 'TestEnv'
    testCurrentDir,
    testWorkDir,
    testPrefixDir,
    testDistDir,
    testPackageDbDir,
    testRepoDir,
    testKeysDir,
    testSourceCopyDir,
    testCabalDir,
    testUserCabalConfigFile,
    testActualFile,
    -- * Skipping tests
    skip,
    skipIf,
    skipUnless,
    -- * Known broken tests
    expectedBroken,
    unexpectedSuccess,
    -- whenHasSharedLibraries,
    -- * Arguments (TODO: move me)
    CommonArgs(..),
    renderCommonArgs,
    commonArgParser,
    -- * Version Constants
    cabalVersionLibrary,
) where

import Test.Cabal.Script
import Test.Cabal.Plan
import Test.Cabal.OutputNormalizer
import Test.Cabal.TestCode

import Distribution.Simple.Compiler
    ( PackageDBStack, PackageDB(..), compilerFlavor
    , Compiler, compilerVersion )
import Distribution.System
import Distribution.Simple.Program.Db
import Distribution.Simple.Program
import Distribution.Simple.Configure
    ( configCompilerEx )
import qualified Distribution.Simple.Utils as U (cabalVersion)
import Distribution.Text

import Distribution.Verbosity
import Distribution.Version

import Data.Monoid ((<>), mempty)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Maybe
import Control.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process hiding (env)
import Options.Applicative

data CommonArgs = CommonArgs {
        argCabalInstallPath    :: Maybe FilePath,
        argGhcPath             :: Maybe FilePath,
        argHackageRepoToolPath :: Maybe FilePath,
        argHaddockPath         :: Maybe FilePath,
        argAccept              :: Bool,
        argSkipSetupTests      :: Bool
    }

commonArgParser :: Parser CommonArgs
commonArgParser = CommonArgs
    <$> optional (option str
        ( help "Path to cabal-install executable to test"
       Data.Monoid.<> long "with-cabal"
       <> metavar "PATH"
        ))
    <*> optional (option str
        ( help "GHC to ask Cabal to use via --with-ghc flag"
       <> short 'w'
       <> long "with-ghc"
       <> metavar "PATH"
        ))
    <*> optional (option str
        ( help "Path to hackage-repo-tool to use for repository manipulation"
       <> long "with-hackage-repo-tool"
       <> metavar "PATH"
        ))
    <*> optional (option str
        ( help "Path to haddock to use for --with-haddock flag"
       <> long "with-haddock"
       <> metavar "PATH"
        ))
    <*> switch
        ( long "accept"
       <> help "Accept output"
        )
    <*> switch (long "skip-setup-tests" <> help "Skip setup tests")

renderCommonArgs :: CommonArgs -> [String]
renderCommonArgs args =
    maybe [] (\x -> ["--with-cabal",             x]) (argCabalInstallPath    args) ++
    maybe [] (\x -> ["--with-ghc",               x]) (argGhcPath             args) ++
    maybe [] (\x -> ["--with-haddock",           x]) (argHaddockPath         args) ++
    maybe [] (\x -> ["--with-hackage-repo-tool", x]) (argHackageRepoToolPath args) ++
    (if argAccept args then ["--accept"] else []) ++
    (if argSkipSetupTests args then ["--skip-setup-tests"] else [])

data TestArgs = TestArgs {
        testArgDistDir    :: FilePath,
        testArgScriptPath :: FilePath,
        testCommonArgs    :: CommonArgs
    }

testArgParser :: Parser TestArgs
testArgParser = TestArgs
    <$> option str
        ( help "Build directory of cabal-testsuite"
       <> long "builddir"
       <> metavar "DIR")
    <*> argument str ( metavar "FILE")
    <*> commonArgParser

skip :: String -> TestM ()
skip reason = liftIO $ do
    putStrLn ("SKIP " ++ reason)
    E.throwIO (TestCodeSkip reason)

skipIf :: String -> Bool -> TestM ()
skipIf reason b = when b (skip reason)

skipUnless :: String -> Bool -> TestM ()
skipUnless reason b = unless b (skip reason)

expectedBroken :: TestM ()
expectedBroken = liftIO $ do
    putStrLn "EXPECTED FAIL"
    E.throwIO TestCodeKnownFail

unexpectedSuccess :: TestM ()
unexpectedSuccess = liftIO $ do
    putStrLn "UNEXPECTED OK"
    E.throwIO TestCodeUnexpectedOk

trySkip :: IO a -> IO (Either String a)
trySkip m = fmap Right m `E.catch` \e -> case e of
    TestCodeSkip msg -> return (Left msg)
    _                -> E.throwIO e

setupAndCabalTest :: TestM () -> IO ()
setupAndCabalTest m = do
    r1 <- trySkip (setupTest m)
    r2 <- trySkip (cabalTest' "cabal" m)
    case (r1, r2) of
        (Left msg1, Left msg2) -> E.throwIO (TestCodeSkip (msg1 ++ "; " ++ msg2))
        _                      -> return ()

setupTest :: TestM () -> IO ()
setupTest m = runTestM "" $ do
    env <- getTestEnv
    skipIf "setup test" (testSkipSetupTests env)
    m

cabalTest :: TestM () -> IO ()
cabalTest = cabalTest' ""

cabalTest' :: String -> TestM () -> IO ()
cabalTest' mode m = runTestM mode $ do
    skipUnless "no cabal-install" =<< isAvailableProgram cabalProgram
    withReaderT (\nenv -> nenv { testCabalInstallAsSetup = True }) m

type TestM = ReaderT TestEnv IO

gitProgram :: Program
gitProgram = simpleProgram "git"

hackageRepoToolProgram :: Program
hackageRepoToolProgram = simpleProgram "hackage-repo-tool"

cabalProgram :: Program
cabalProgram = (simpleProgram "cabal") {
        -- Do NOT search for executable named cabal, it's probably
        -- not the one you were intending to test
        programFindLocation = \_ _ -> return Nothing
    }

diffProgram :: Program
diffProgram = simpleProgram "diff"

python3Program :: Program
python3Program = simpleProgram "python3"

-- | Run a test in the test monad according to program's arguments.
runTestM :: String -> TestM a -> IO a
runTestM mode m = withSystemTempDirectory "cabal-testsuite" $ \tmp_dir -> do
    args <- execParser (info testArgParser Data.Monoid.mempty)
    let dist_dir = testArgDistDir args
        (script_dir0, script_filename) = splitFileName (testArgScriptPath args)
        script_base = dropExtensions script_filename
    -- Canonicalize this so that it is stable across working directory changes
    script_dir <- canonicalizePath script_dir0
    let verbosity = normal -- TODO: configurable
    senv <- mkScriptEnv verbosity
    -- Add test suite specific programs
    let program_db0 =
            addKnownPrograms
                ([gitProgram, hackageRepoToolProgram, cabalProgram, diffProgram, python3Program] ++ builtinPrograms)
                (runnerProgramDb senv)
    -- Reconfigure according to user flags
    let cargs = testCommonArgs args

    -- Reconfigure GHC
    (comp, platform, program_db2) <- case argGhcPath cargs of
        Nothing -> return (runnerCompiler senv, runnerPlatform senv, program_db0)
        Just ghc_path -> do
            -- All the things that get updated paths from
            -- configCompilerEx.  The point is to make sure
            -- we reconfigure these when we need them.
            let program_db1 = unconfigureProgram "ghc"
                            . unconfigureProgram "ghc-pkg"
                            . unconfigureProgram "hsc2hs"
                            . unconfigureProgram "haddock"
                            . unconfigureProgram "hpc"
                            . unconfigureProgram "runghc"
                            . unconfigureProgram "gcc"
                            . unconfigureProgram "ld"
                            . unconfigureProgram "ar"
                            . unconfigureProgram "strip"
                            $ program_db0
            -- TODO: this actually leaves a pile of things unconfigured.
            -- Optimal strategy for us is to lazily configure them, so
            -- we don't pay for things we don't need.  A bit difficult
            -- to do in the current design.
            configCompilerEx
                (Just (compilerFlavor (runnerCompiler senv)))
                (Just ghc_path)
                Nothing
                program_db1
                verbosity

    program_db3 <-
        reconfigurePrograms verbosity
            ([("cabal", p)   | p <- maybeToList (argCabalInstallPath cargs)] ++
             [("hackage-repo-tool", p)
                             | p <- maybeToList (argHackageRepoToolPath cargs)] ++
             [("haddock", p) | p <- maybeToList (argHaddockPath cargs)])
            [] -- --prog-options not supported ATM
            program_db2
    -- configCompilerEx only marks some programs as known, so to pick
    -- them up we must configure them
    program_db <- configureAllKnownPrograms verbosity program_db3

    let ghcAndRunnedGhcAreTheSame :: Bool
        ghcAndRunnedGhcAreTheSame = fromMaybe False $ do
            ghc_program        <- lookupProgram ghcProgram program_db
            runner_ghc_program <- lookupProgram ghcProgram (runnerProgramDb senv)
            return $ programPath ghc_program == programPath runner_ghc_program

    let db_stack =
            case argGhcPath (testCommonArgs args) of
                Nothing -> runnerPackageDbStack senv -- NB: canonicalized
                -- Can't use the build package db stack since they
                -- are all for the wrong versions!  TODO: Make
                -- this configurable
                --
                -- Oleg: if runner ghc and provided ghc are the same,
                -- use runnerPackageDbStack. See 'hasCabalForGhc' check.
                Just _
                    | ghcAndRunnedGhcAreTheSame -> runnerPackageDbStack senv
                    | otherwise                 -> [GlobalPackageDB]
        env = TestEnv {
                    testSourceDir = script_dir,
                    testTmpDir = tmp_dir,
                    testSubName = script_base,
                    testMode = mode,
                    testProgramDb = program_db,
                    testPlatform = platform,
                    testCompiler = comp,
                    testPackageDBStack = db_stack,
                    testVerbosity = verbosity,
                    testMtimeChangeDelay = Nothing,
                    testScriptEnv = senv,
                    testSetupPath = dist_dir </> "build" </> "setup" </> "setup",
                    testSkipSetupTests =  argSkipSetupTests (testCommonArgs args),
                    testHaveCabalShared = runnerWithSharedLib senv,
                    testEnvironment =
                        -- Try to avoid Unicode output
                        [ ("LC_ALL", Just "C")
                        -- Hermetic builds (knot-tied)
                        , ("HOME", Just (testHomeDir env))
                        -- Set CABAL_DIR in addition to HOME, since HOME has no
                        -- effect on Windows.
                        , ("CABAL_DIR", Just (testCabalDir env))
                        , ("CABAL_CONFIG", Just $ testCabalDir env </> "config")
                        ],
                    testShouldFail = False,
                    testRelativeCurrentDir = ".",
                    testHavePackageDb = False,
                    testHaveRepo = False,
                    testHaveSourceCopy = False,
                    testCabalInstallAsSetup = False,
                    testCabalProjectFile = "cabal.project",
                    testPlan = Nothing,
                    testRecordDefaultMode = DoNotRecord,
                    testRecordUserMode = Nothing,
                    testRecordNormalizer = id,
                    testSourceCopyRelativeDir = "source"
                }
    let go = do cleanup
                r <- m
                check_expect (argAccept (testCommonArgs args))
                return r
    runReaderT go env
  where
    cleanup = do
        env <- getTestEnv
        onlyIfExists . removeDirectoryRecursive $ testWorkDir env
        -- NB: it's important to initialize this ourselves, as
        -- the default configuration hardcodes Hackage, which we do
        -- NOT want to assume for these tests (no test should
        -- hit Hackage.)
        liftIO $ createDirectoryIfMissing True (testCabalDir env)
        ghc_path <- programPathM ghcProgram
        liftIO $ writeFile (testUserCabalConfigFile env)
               $ unlines [ "with-compiler: " ++ ghc_path ]

    check_expect accept = do
        env <- getTestEnv
        actual_raw <- liftIO $ readFileOrEmpty (testActualFile env)
        expect <- liftIO $ readFileOrEmpty (testExpectFile env)
        norm_env <- mkNormalizerEnv
        let actual = normalizeOutput norm_env actual_raw
        when (words actual /= words expect) $ do
            -- First try whitespace insensitive diff
            let actual_fp = testNormalizedActualFile env
                expect_fp = testNormalizedExpectFile env
            liftIO $ writeFile actual_fp actual
            liftIO $ writeFile expect_fp expect
            liftIO $ putStrLn "Actual output differs from expected:"
            b <- diff ["-uw"] expect_fp actual_fp
            unless b . void $ diff ["-u"] expect_fp actual_fp
            if accept
                then do liftIO $ putStrLn "Accepting new output."
                        liftIO $ writeFileNoCR (testExpectFile env) actual
                else liftIO $ exitWith (ExitFailure 1)

readFileOrEmpty :: FilePath -> IO String
readFileOrEmpty f = readFile f `E.catch` \e ->
                                    if isDoesNotExistError e
                                        then return ""
                                        else E.throwIO e

-- | Runs 'diff' with some arguments on two files, outputting the
-- diff to stderr, and returning true if the two files differ
diff :: [String] -> FilePath -> FilePath -> TestM Bool
diff args path1 path2 = do
    diff_path <- programPathM diffProgram
    (_,_,_,h) <- liftIO $
        createProcess (proc diff_path (args ++ [path1, path2])) {
                std_out = UseHandle stderr
            }
    r <- liftIO $ waitForProcess h
    return (r /= ExitSuccess)

-- | Write a file with no CRs, always.
writeFileNoCR :: FilePath -> String -> IO ()
writeFileNoCR f s =
    withFile f WriteMode $ \h -> do
        hSetNewlineMode h noNewlineTranslation
        hPutStr h s

mkNormalizerEnv :: TestM NormalizerEnv
mkNormalizerEnv = do
    env <- getTestEnv
    ghc_pkg_program <- requireProgramM ghcPkgProgram
    -- Arguably we should use Cabal's APIs but I am too lazy
    -- to remember what it is
    list_out <- liftIO $ readProcess (programPath ghc_pkg_program)
                      ["list", "--global", "--simple-output"] ""
    tmpDir <- liftIO $ getTemporaryDirectory

    return NormalizerEnv {
        normalizerRoot
            = addTrailingPathSeparator (testSourceDir env),
        normalizerTmpDir
            = addTrailingPathSeparator (testTmpDir env),
        normalizerGblTmpDir
            = addTrailingPathSeparator tmpDir,
        normalizerGhcVersion
            = compilerVersion (testCompiler env),
        normalizerKnownPackages
            = mapMaybe simpleParse (words list_out),
        normalizerPlatform
            = testPlatform env,
        normalizerCabalVersion
            = cabalVersionLibrary
    }
    where

cabalVersionLibrary :: Version
cabalVersionLibrary = U.cabalVersion

requireProgramM :: Program -> TestM ConfiguredProgram
requireProgramM program = do
    env <- getTestEnv
    (configured_program, _) <- liftIO $
        requireProgram (testVerbosity env) program (testProgramDb env)
    return configured_program

programPathM :: Program -> TestM FilePath
programPathM program = do
    fmap programPath (requireProgramM program)

isAvailableProgram :: Program -> TestM Bool
isAvailableProgram program = do
    env <- getTestEnv
    case lookupProgram program (testProgramDb env) of
        Just _ -> return True
        Nothing -> do
            -- It might not have been configured. Try to configure.
            progdb <- liftIO $ configureProgram (testVerbosity env) program (testProgramDb env)
            case lookupProgram program progdb of
                Just _  -> return True
                Nothing -> return False

-- | Run an IO action, and suppress a "does not exist" error.
onlyIfExists :: MonadIO m => IO () -> m ()
onlyIfExists m =
    liftIO $ E.catch m $ \(e :: IOError) ->
        unless (isDoesNotExistError e) $ E.throwIO e

data TestEnv = TestEnv
    -- UNCHANGING:

    {
    -- | Path to the test directory, as specified by path to test
    -- script.
      testSourceDir     :: FilePath
    -- | Somewhere to stow temporary files needed by the test.
    , testTmpDir        :: FilePath
    -- | Test sub-name, used to qualify dist/database directory to avoid
    -- conflicts.
    , testSubName       :: String
    -- | Test mode, further qualifies multiple invocations of the
    -- same test source code.
    , testMode          :: String
    -- | Program database to use when we want ghc, ghc-pkg, etc.
    , testProgramDb     :: ProgramDb
    -- | Compiler we are running tests for
    , testCompiler      :: Compiler
    -- | Platform we are running tests on
    , testPlatform      :: Platform
    -- | Package database stack (actually this changes lol)
    , testPackageDBStack :: PackageDBStack
    -- | How verbose to be
    , testVerbosity     :: Verbosity
    -- | How long we should 'threadDelay' to make sure the file timestamp is
    -- updated correctly for recompilation tests.  Nothing if we haven't
    -- calibrated yet.
    , testMtimeChangeDelay :: Maybe Int
    -- | Script environment for runghc
    , testScriptEnv :: ScriptEnv
    -- | Setup script path
    , testSetupPath :: FilePath
    -- | Skip Setup tests?
    , testSkipSetupTests :: Bool
    -- | Do we have shared libraries for the Cabal-under-tests?
    -- This is used for example to determine whether we can build
    -- detailed-0.9 tests dynamically, since they link against Cabal-under-test.
    , testHaveCabalShared :: Bool

    -- CHANGING:

    -- | Environment override
    , testEnvironment   :: [(String, Maybe String)]
    -- | When true, we invert the meaning of command execution failure
    , testShouldFail    :: Bool
    -- | The current working directory, relative to 'testSourceDir'
    , testRelativeCurrentDir :: FilePath
    -- | Says if we've initialized the per-test package DB
    , testHavePackageDb  :: Bool
    -- | Says if we've setup a repository
    , testHaveRepo :: Bool
    -- | Says if we've copied the source to a hermetic directory
    , testHaveSourceCopy :: Bool
    -- | Says if we're testing cabal-install as setup
    , testCabalInstallAsSetup :: Bool
    -- | Says what cabal.project file to use (probed)
    , testCabalProjectFile :: FilePath
    -- | Cached record of the plan metadata from a new-build
    -- invocation; controlled by 'withPlan'.
    , testPlan :: Maybe Plan
    -- | If user mode is not set, this is the record mode we default to.
    , testRecordDefaultMode :: RecordMode
    -- | User explicitly set record mode.  Not implemented ATM.
    , testRecordUserMode :: Maybe RecordMode
    -- | Function to normalize recorded output
    , testRecordNormalizer :: String -> String
    -- | Name of the subdirectory we copied the test's sources to,
    -- relative to 'testSourceDir'
    , testSourceCopyRelativeDir :: FilePath
    }

testRecordMode :: TestEnv -> RecordMode
testRecordMode env = fromMaybe (testRecordDefaultMode env) (testRecordUserMode env)

data RecordMode = DoNotRecord | RecordMarked | RecordAll
    deriving (Show, Eq, Ord)

getTestEnv :: TestM TestEnv
getTestEnv = ask

------------------------------------------------------------------------
-- * Directories

-- | The absolute path to the root of the package directory; it's
-- where the Cabal file lives.  This is what you want the CWD of cabal
-- calls to be.
testCurrentDir :: TestEnv -> FilePath
testCurrentDir env =
    (if testHaveSourceCopy env
        then testSourceCopyDir env
        else testSourceDir env) </> testRelativeCurrentDir env

testName :: TestEnv -> String
testName env = testSubName env <.> testMode env

-- | The absolute path to the directory containing all the
-- files for ALL tests associated with a test (respecting
-- subtests.)  To clean, you ONLY need to delete this directory.
testWorkDir :: TestEnv -> FilePath
testWorkDir env =
    testSourceDir env </> (testName env <.> "dist")

-- | The absolute prefix where installs go.
testPrefixDir :: TestEnv -> FilePath
testPrefixDir env = testWorkDir env </> "usr"

-- | The absolute path to the build directory that should be used
-- for the current package in a test.
testDistDir :: TestEnv -> FilePath
testDistDir env = testWorkDir env </> "work" </> testRelativeCurrentDir env </> "dist"

-- | The absolute path to the shared package database that should
-- be used by all packages in this test.
testPackageDbDir :: TestEnv -> FilePath
testPackageDbDir env = testWorkDir env </> "packagedb"

-- | The absolute prefix where our simulated HOME directory is.
testHomeDir :: TestEnv -> FilePath
testHomeDir env = testWorkDir env </> "home"

-- | The absolute prefix of our local secure repository, which we
-- use to simulate "external" packages
testRepoDir :: TestEnv -> FilePath
testRepoDir env = testWorkDir env </> "repo"

-- | The absolute prefix of keys for the test.
testKeysDir :: TestEnv -> FilePath
testKeysDir env = testWorkDir env </> "keys"

-- | If 'withSourceCopy' is used, where the source files go.
testSourceCopyDir :: TestEnv -> FilePath
testSourceCopyDir env = testWorkDir env </> testSourceCopyRelativeDir env

-- | The user cabal directory
testCabalDir :: TestEnv -> FilePath
testCabalDir env = testHomeDir env </> ".cabal"

-- | The user cabal config file
testUserCabalConfigFile :: TestEnv -> FilePath
testUserCabalConfigFile env = testCabalDir env </> "config"

-- | The file where the expected output of the test lives
testExpectFile :: TestEnv -> FilePath
testExpectFile env = testSourceDir env </> testName env <.> "out"

-- | Where we store the actual output
testActualFile :: TestEnv -> FilePath
testActualFile env = testWorkDir env </> testName env <.> "comp.out"

-- | Where we will write the normalized actual file (for diffing)
testNormalizedActualFile :: TestEnv -> FilePath
testNormalizedActualFile env = testActualFile env <.> "normalized"

-- | Where we will write the normalized expected file (for diffing)
testNormalizedExpectFile :: TestEnv -> FilePath
testNormalizedExpectFile env = testWorkDir env </> testName env <.> "out.normalized"
