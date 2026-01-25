{-# LANGUAGE ScopedTypeVariables #-}

-- | The test monad
module Test.Cabal.Monad
  ( -- * High-level runners
    setupAndCabalTest
  , setupTest
  , cabalTest
  , cabalTest'

    -- * The monad
  , TestM
  , runTestM

    -- * Helper functions
  , programPathM
  , requireProgramM
  , isAvailableProgram
  , hackageRepoToolProgram
  , gitProgram
  , cabalProgram
  , diffProgram
  , python3Program
  , requireSuccess
  , initWorkDir
  , recordLog

    -- * The test environment
  , TestEnv (..)
  , getTestEnv

    -- * Recording mode
  , RecordMode (..)
  , testRecordMode

    -- * Derived values from 'TestEnv'
  , testCurrentDir
  , testWorkDir
  , testPrefixDir
  , testLibInstallDir
  , testDistDir
  , testSystemTmpDir
  , testPackageDbDir
  , testRepoDir
  , testKeysDir
  , testSourceCopyDir
  , testCabalDir
  , testStoreDir
  , testUserCabalConfigFile
  , testActualFile
  , testVerbosity

    -- * Skipping tests
  , skip
  , skipIO
  , skipIf
  , skipIfIO
  , skipUnless
  , skipUnlessIO

    -- * Known broken tests
  , expectBroken
  , expectBrokenIf
  , expectBrokenUnless

    -- * Flaky tests
  , flaky
  , flakyIf

    -- * Arguments (TODO: move me)
  , CommonArgs (..)
  , renderCommonArgs
  , commonArgParser

    -- * Version Constants
  , cabalVersionLibrary
  ) where

import Test.Cabal.OutputNormalizer
import Test.Cabal.Plan
import Test.Cabal.Script
import Test.Cabal.TestCode

import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler
  ( Compiler
  , PackageDBStackCWD
  , PackageDBX (..)
  , compilerFlavor
  , compilerVersion
  , showCompilerIdWithAbi
  )
import Distribution.Simple.Configure
  ( configCompilerEx
  )
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import qualified Distribution.Simple.Utils as U (cabalVersion)
import Distribution.System
import Distribution.Text

import Distribution.Verbosity
import Distribution.Version
import Test.Utils.TempTestDir (removeDirectoryRecursiveHack, withTestDir')

import Control.Applicative
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe
import Data.Monoid (mempty)
import Distribution.Simple.Utils hiding (info)
import GHC.Stack
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process hiding (env)
import Test.Cabal.Run

data CommonArgs = CommonArgs
  { argCabalInstallPath :: Maybe FilePath
  , argGhcPath :: Maybe FilePath
  , argHackageRepoToolPath :: Maybe FilePath
  , argHaddockPath :: Maybe FilePath
  , argKeepTmpFiles :: Bool
  , argAccept :: Bool
  , argSkipSetupTests :: Bool
  }

commonArgParser :: Parser CommonArgs
commonArgParser =
  CommonArgs
    <$> optional
      ( option
          str
          ( help "Path to cabal-install executable to test. If omitted, tests involving cabal-install are skipped!"
              <> long "with-cabal"
              <> metavar "PATH"
          )
      )
    <*> optional
      ( option
          str
          ( help "GHC to ask Cabal to use via --with-ghc flag"
              <> short 'w'
              <> long "with-ghc"
              <> metavar "PATH"
          )
      )
    <*> optional
      ( option
          str
          ( help "Path to hackage-repo-tool to use for repository manipulation"
              <> long "with-hackage-repo-tool"
              <> metavar "PATH"
          )
      )
    <*> optional
      ( option
          str
          ( help "Path to haddock to use for --with-haddock flag"
              <> long "with-haddock"
              <> metavar "PATH"
          )
      )
    <*> switch
      ( long "keep-tmp-files"
          <> help "Keep temporary files"
      )
    <*> switch
      ( long "accept"
          <> help "Accept output"
      )
    <*> switch (long "skip-setup-tests" <> help "Skip setup tests")

renderCommonArgs :: CommonArgs -> [String]
renderCommonArgs args =
  maybe [] (\x -> ["--with-cabal", x]) (argCabalInstallPath args)
    ++ maybe [] (\x -> ["--with-ghc", x]) (argGhcPath args)
    ++ maybe [] (\x -> ["--with-haddock", x]) (argHaddockPath args)
    ++ maybe [] (\x -> ["--with-hackage-repo-tool", x]) (argHackageRepoToolPath args)
    ++ (if argAccept args then ["--accept"] else [])
    ++ (if argKeepTmpFiles args then ["--keep-tmp-files"] else [])
    ++ (if argSkipSetupTests args then ["--skip-setup-tests"] else [])

data TestArgs = TestArgs
  { testArgDistDir :: FilePath
  , testArgPackageDb :: [FilePath]
  , testArgScriptPath :: FilePath
  , testCommonArgs :: CommonArgs
  }

testArgParser :: Parser TestArgs
testArgParser =
  TestArgs
    <$> option
      str
      ( help "Build directory of cabal-testsuite"
          <> long "builddir"
          <> metavar "DIR"
      )
    <*> many
      ( option
          str
          ( help "Package DB which contains Cabal and Cabal-syntax"
              <> long "extra-package-db"
              <> metavar "DIR"
          )
      )
    <*> argument str (metavar "FILE")
    <*> commonArgParser

-- * skip tests

skipIO :: String -> IO ()
skipIO reason = do
  putStrLn $ "SKIP (" <> reason <> ")"
  E.throwIO (TestCodeSkip reason)

skip :: String -> TestM ()
skip = liftIO . skipIO

skipIfIO :: String -> Bool -> IO ()
skipIfIO reason b = when b (skipIO reason)

skipIf :: String -> Bool -> TestM ()
skipIf reason b = when b (skip reason)

skipUnlessIO :: String -> Bool -> IO ()
skipUnlessIO reason b = unless b (skipIO reason)

skipUnless :: String -> Bool -> TestM ()
skipUnless reason b = unless b (skip reason)

-- * Broken tests

expectBroken :: IssueID -> TestM a -> TestM a
expectBroken ticket m = do
  env <- getTestEnv
  liftIO . withAsync (runReaderT m env) $ \a -> do
    r <- waitCatch a
    case r of
      Left e -> do
        putStrLn $ "This test is known broken, see #" ++ show ticket ++ ":"
        print e
        throwExpectedBroken ticket
      Right _ -> do
        throwUnexpectedSuccess ticket

expectBrokenIf :: Bool -> IssueID -> TestM a -> TestM a
expectBrokenIf True ticket m = expectBroken ticket m
expectBrokenIf False _ m = m

expectBrokenUnless :: Bool -> IssueID -> TestM a -> TestM a
expectBrokenUnless b = expectBrokenIf (not b)

throwExpectedBroken :: IssueID -> IO a
throwExpectedBroken ticket = do
  putStrLn $ "EXPECTED FAIL (#" <> show ticket <> ")"
  E.throwIO (TestCodeKnownFail ticket)

throwUnexpectedSuccess :: IssueID -> IO a
throwUnexpectedSuccess ticket = do
  putStrLn $ "UNEXPECTED OK (#" <> show ticket <> ")"
  E.throwIO (TestCodeUnexpectedOk ticket)

-- * Flaky tests

flaky :: IssueID -> TestM a -> TestM a
flaky ticket m = do
  env <- getTestEnv
  liftIO . withAsync (runReaderT m env) $ \a -> do
    r <- waitCatch a
    case r of
      Left e -> do
        putStrLn $ "This test is known flaky, and it failed, see #" ++ show ticket ++ ":"
        print e
        throwFlakyFail ticket
      Right _ -> do
        putStrLn $ "This test is known flaky, but it passed, see #" ++ show ticket ++ ":"
        throwFlakyPass ticket

flakyIf :: Bool -> IssueID -> TestM a -> TestM a
flakyIf True ticket m = flaky ticket m
flakyIf False _ m = m

throwFlakyFail :: IssueID -> IO a
throwFlakyFail ticket = do
  putStrLn $ "FLAKY FAIL (#" <> show ticket <> ")"
  E.throwIO (TestCodeFlakyFailed ticket)

throwFlakyPass :: IssueID -> IO a
throwFlakyPass ticket = do
  putStrLn $ "FLAKY OK (#" <> show ticket <> ")"
  E.throwIO (TestCodeFlakyPassed ticket)

trySkip :: IO a -> IO (Either String a)
trySkip m =
  fmap Right m `E.catch` \e -> case e of
    TestCodeSkip msg -> return (Left msg)
    _ -> E.throwIO e

setupAndCabalTest :: TestM () -> IO ()
setupAndCabalTest m = do
  r1 <- trySkip (setupTest m)
  r2 <- trySkip (cabalTest' "cabal" m)
  case (r1, r2) of
    (Left msg1, Left msg2) -> E.throwIO (TestCodeSkip (msg1 ++ "; " ++ msg2))
    _ -> return ()

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
  withReaderT (\nenv -> nenv{testCabalInstallAsSetup = True}) m

type TestM = ReaderT TestEnv IO

gitProgram :: Program
gitProgram = simpleProgram "git"

hackageRepoToolProgram :: Program
hackageRepoToolProgram = simpleProgram "hackage-repo-tool"

cabalProgram :: Program
cabalProgram =
  (simpleProgram "cabal")
    { -- Do NOT search for executable named cabal, it's probably
      -- not the one you were intending to test
      programFindLocation = \_ _ -> return Nothing
    }

diffProgram :: Program
diffProgram = simpleProgram "diff"

python3Program :: Program
python3Program = simpleProgram "python3"

-- | Run a test in the test monad according to program's arguments.
runTestM :: String -> TestM () -> IO ()
runTestM mode m =
  execParser (info testArgParser Data.Monoid.mempty) >>= \args ->
    withTestDir'
      verbosity
      (defaultTempFileOptions{optKeepTempFiles = argKeepTmpFiles (testCommonArgs args)})
      "cabal-testsuite"
      $ \tmp_dir -> do
        let dist_dir = testArgDistDir args
            (script_dir0, script_filename) = splitFileName (testArgScriptPath args)

            stripped =
              stripExtension ".test.hs" script_filename
                <|> stripExtension ".multitest.hs" script_filename
            script_base = fromMaybe (dropExtensions script_filename) stripped

        -- Canonicalize this so that it is stable across working directory changes
        script_dir <- canonicalizePath script_dir0
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
            let program_db1 =
                  unconfigureProgram "ghc"
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

        (configuredGhcProg, _) <- requireProgram verbosity ghcProgram program_db2

        program_db3 <-
          reconfigurePrograms
            verbosity
            ( [("cabal", p) | p <- maybeToList (argCabalInstallPath cargs)]
                ++ [ ("hackage-repo-tool", p)
                   | p <- maybeToList (argHackageRepoToolPath cargs)
                   ]
                ++ [("haddock", p) | p <- maybeToList (argHaddockPath cargs)]
            )
            [] -- --prog-options not supported ATM
            program_db2
        -- configCompilerEx only marks some programs as known, so to pick
        -- them up we must configure them
        program_db <- configureAllKnownPrograms verbosity program_db3

        let db_stack = [GlobalPackageDB]
            env =
              TestEnv
                { testSourceDir = script_dir
                , testTmpDir = tmp_dir
                , testSubName = script_base
                , testMode = mode
                , testProgramDb = program_db
                , testPlatform = platform
                , testCompiler = comp
                , testCompilerPath = programPath configuredGhcProg
                , testPackageDBStack = db_stack
                , testVerbosityFlags = verbosityFlags verbosity
                , testMtimeChangeDelay = Nothing
                , testScriptEnv = senv
                , testSetupPath = dist_dir </> "build" </> "setup" </> "setup"
                , testPackageDbPath = case testArgPackageDb args of [] -> Nothing; xs -> Just xs
                , testSkipSetupTests = argSkipSetupTests (testCommonArgs args)
                , testHaveCabalShared = runnerWithSharedLib senv
                , testEnvironment =
                    -- Use UTF-8 output on all platforms.
                    [ ("LC_ALL", Just "en_US.UTF-8")
                    , -- Hermetic builds (knot-tied)
                      ("HOME", Just (testHomeDir env))
                    , -- Set CABAL_DIR in addition to HOME, since HOME has no
                      -- effect on Windows.
                      ("CABAL_DIR", Just (testCabalDir env))
                    , ("CABAL_CONFIG", Just (testUserCabalConfigFile env))
                    , -- Set `TMPDIR` so that temporary files aren't created in the global `TMPDIR`.
                      ("TMPDIR", Just (testSystemTmpDir env))
                    , -- Windows uses `TMP` for the `TMPDIR`.
                      ("TMP", Just (testSystemTmpDir env))
                    ]
                , testShouldFail = False
                , testRelativeCurrentDir = "."
                , testHavePackageDb = False
                , testHaveRepo = False
                , testCabalInstallAsSetup = False
                , testCabalProjectFile = Nothing
                , testPlan = Nothing
                , testRecordDefaultMode = DoNotRecord
                , testRecordUserMode = Nothing
                , testMaybeStoreDir = Nothing
                }
        runReaderT cleanup env
        join $
          E.catch
            ( runReaderT
                ( do
                    withSourceCopy m
                    check_expect (argAccept (testCommonArgs args)) Nothing
                )
                env
            )
            ( \(e :: TestCode) -> do
                -- A test that resulted in unexpected success should check its output
                -- because maybe it is the output the one that makes it fail!
                case isTestCodeUnexpectedSuccess e of
                  Just t -> runReaderT (check_expect (argAccept (testCommonArgs args)) (Just (t, False))) env
                  Nothing ->
                    -- A test that is reported flaky but passed might fail because of the output
                    case isTestCodeFlaky e of
                      Flaky True t -> runReaderT (check_expect (argAccept (testCommonArgs args)) (Just (t, True))) env
                      _ -> E.throwIO e
            )
  where
    verbosity = mkVerbosity defaultVerbosityHandles normal
    -- TODO: make this configurable by the test-suite driver

    cleanup = do
      env <- getTestEnv
      onlyIfExists . removeDirectoryRecursiveHack verbosity $ testWorkDir env
      -- NB: it's important to initialize this ourselves, as
      -- the default configuration hardcodes Hackage, which we do
      -- NOT want to assume for these tests (no test should
      -- hit Hackage.)
      liftIO $ createDirectoryIfMissing True (testCabalDir env)
      ghc_path <- programPathM ghcProgram
      liftIO $
        writeFile (testUserCabalConfigFile env) $
          unlines ["with-compiler: " ++ ghc_path]

    check_expect accept was_expected_to_fail = do
      env <- getTestEnv
      actual_raw <- liftIO $ readFileOrEmpty (testActualFile env)
      expect <- liftIO $ readFileOrEmpty (testExpectFile env)
      norm_env <- mkNormalizerEnv
      let actual = normalizeOutput norm_env actual_raw
      case (was_expected_to_fail, words actual /= words expect) of
        -- normal test, output doesn't match
        (Nothing, True) -> do
          -- First try whitespace insensitive diff
          let actual_fp = testNormalizedActualFile env
              expect_fp = testNormalizedExpectFile env
          liftIO $ writeFile actual_fp actual
          liftIO $ writeFile expect_fp expect
          liftIO $ putStrLn "Actual output differs from expected:"
          b <- diff ["-uw"] expect_fp actual_fp
          unless b . void $ diff ["-u"] expect_fp actual_fp
          if accept
            then do
              liftIO $ putStrLn $ "Writing actual test output to " <> testExpectAcceptFile env
              liftIO $ writeFileNoCR (testExpectAcceptFile env) actual
              pure (pure ())
            else pure (E.throwIO TestCodeFail)
        -- normal test, output matches
        (Nothing, False) -> pure (pure ())
        -- expected fail, output matches
        (Just (t, was_flaky), False) -> pure (E.throwIO $ if was_flaky then TestCodeFlakyPassed t else TestCodeUnexpectedOk t)
        -- expected fail, output doesn't match
        (Just (t, was_flaky), True) -> do
          -- First try whitespace insensitive diff
          let actual_fp = testNormalizedActualFile env
              expect_fp = testNormalizedExpectFile env
          liftIO $ writeFile actual_fp actual
          liftIO $ writeFile expect_fp expect
          liftIO $ putStrLn "Actual output differs from expected:"
          b <- diff ["-uw"] expect_fp actual_fp
          unless b . void $ diff ["-u"] expect_fp actual_fp
          pure (E.throwIO $ if was_flaky then TestCodeFlakyFailed t else TestCodeKnownFail t)

readFileOrEmpty :: FilePath -> IO String
readFileOrEmpty f =
  readFile f `E.catch` \e ->
    if isDoesNotExistError e
      then return ""
      else E.throwIO e

-- | Run an IO action, and suppress a "does not exist" error.
onlyIfExists :: MonadIO m => IO () -> m ()
onlyIfExists m =
  liftIO $ E.catch m $ \(e :: IOError) ->
    unless (isDoesNotExistError e) $ E.throwIO e

-- | Make a hermetic copy of the test directory.
--
-- This requires the test repository to be a Git checkout, because
-- we use the Git metadata to figure out what files to copy into the
-- hermetic copy.
withSourceCopy :: TestM a -> TestM a
withSourceCopy m = do
  env <- getTestEnv
  initWorkDir
  let curdir = testSourceDir env
      dest = testSourceCopyDir env
  fs <- getSourceFiles
  when
    (null fs)
    ( error
        ( unlines
            [ "withSourceCopy: No files to copy from " ++ curdir
            , "You need to \"git add\" any files before they are copied by the testsuite."
            ]
        )
    )
  forM_ fs $ \f -> do
    unless (isTestFile f) $ liftIO $ do
      putStrLn ("Copying " ++ (curdir </> f) ++ " to " ++ (dest </> f))
      createDirectoryIfMissing True (takeDirectory (dest </> f))
      d <- liftIO $ doesDirectoryExist (curdir </> f)
      if d
        then copyDirectoryRecursive (mkVerbosity defaultVerbosityHandles normal) (curdir </> f) (dest </> f)
        else copyFile (curdir </> f) (dest </> f)
  m

-- NB: Keep this synchronized with partitionTests
isTestFile :: FilePath -> Bool
isTestFile f =
  case takeExtensions f of
    ".test.hs" -> True
    ".multitest.hs" -> True
    _ -> False

initWorkDir :: TestM ()
initWorkDir = do
  env <- getTestEnv
  liftIO $ createDirectoryIfMissing True (testWorkDir env)
  liftIO $ createDirectoryIfMissing True (testSystemTmpDir env)

getSourceFiles :: TestM [FilePath]
getSourceFiles = do
  env <- getTestEnv
  configured_prog <- requireProgramM gitProgram
  r <-
    liftIO $
      run
        (Just $ testSourceDir env)
        (testEnvironment env)
        (programPath configured_prog)
        ["ls-files", "--cached", "--modified"]
        Nothing
  recordLog r
  _ <- requireSuccess r
  return (lines $ resultOutput r)

recordLog :: Result -> TestM ()
recordLog res = do
  env <- getTestEnv
  let mode = testRecordMode env
  initWorkDir
  liftIO $
    C.appendFile
      (testWorkDir env </> "test.log")
      ( C.pack $
          "+ "
            ++ resultCommand res
            ++ "\n"
            ++ resultOutput res
            ++ "\n\n"
      )
  liftIO . C.appendFile (testActualFile env) . C.pack $
    case mode of
      RecordAll -> unlines (lines (resultOutput res))
      RecordMarked -> getMarkedOutput (resultOutput res)
      DoNotRecord -> ""

------------------------------------------------------------------------

-- * Subprocess run results

requireSuccess :: Result -> TestM Result
requireSuccess
  r@Result
    { resultCommand = cmd
    , resultExitCode = exitCode
    , resultOutput = output
    } = withFrozenCallStack $ do
    env <- getTestEnv
    when (exitCode /= ExitSuccess && not (testShouldFail env)) $
      assertFailure $
        "Command "
          ++ cmd
          ++ " failed.\n"
          ++ "Output:\n"
          ++ output
          ++ "\n"
    when (exitCode == ExitSuccess && testShouldFail env) $
      assertFailure $
        "Command "
          ++ cmd
          ++ " succeeded.\n"
          ++ "Output:\n"
          ++ output
          ++ "\n"
    return r

assertFailure :: String -> m ()
assertFailure msg = withFrozenCallStack $ error msg

-- | Runs 'diff' with some arguments on two files, outputting the
-- diff to stderr, and returning true if the two files differ
diff :: [String] -> FilePath -> FilePath -> TestM Bool
diff args path1 path2 = do
  diff_path <- programPathM diffProgram
  (_, _, _, h) <-
    liftIO $
      createProcess
        (proc diff_path (args ++ [path1, path2]))
          { std_out = UseHandle stderr
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
  list_out <-
    liftIO $
      readProcess
        (programPath ghc_pkg_program)
        ["list", "--global", "--simple-output"]
        ""
  tmpDir <- liftIO $ getTemporaryDirectory

  canonicalizedTestTmpDir <- liftIO $ canonicalizePath (testTmpDir env)
  canonicalizedGblDir <- liftIO $ canonicalizePath tmpDir

  -- 'cabal' is configured in the package-db, but doesn't specify how to find the program version
  -- Thus we find the program location, if it exists, and query for the program version for
  -- output normalisation.
  cabalVersionM <- do
    cabalProgM <- needProgramM "cabal"
    case cabalProgM of
      Nothing -> pure Nothing
      Just cabalProg -> do
        liftIO $
          findProgramVersion
            "--numeric-version"
            id
            (testVerbosity env)
            (programPath cabalProg)

  return
    NormalizerEnv
      { normalizerTmpDir =
          ( if buildOS == Windows
              then joinDrive "\\" . dropDrive
              else id
          )
            $ addTrailingPathSeparator (testTmpDir env)
      , normalizerCanonicalTmpDir =
          ( if buildOS == Windows
              then joinDrive "\\" . dropDrive
              else id
          )
            $ addTrailingPathSeparator canonicalizedTestTmpDir
      , normalizerGblTmpDir =
          ( if buildOS == Windows
              then joinDrive "\\" . dropDrive
              else id
          )
            $ addTrailingPathSeparator tmpDir
      , normalizerCanonicalGblTmpDir =
          ( if buildOS == Windows
              then joinDrive "\\" . dropDrive
              else id
          )
            $ addTrailingPathSeparator canonicalizedGblDir
      , normalizerGhcVersion =
          compilerVersion (testCompiler env)
      , normalizerGhcPath =
          testCompilerPath env
      , normalizerKnownPackages =
          mapMaybe simpleParse (words list_out)
      , normalizerPlatform =
          testPlatform env
      , normalizerCabalVersion =
          cabalVersionLibrary
      , normalizerCabalInstallVersion =
          cabalVersionM
      }

cabalVersionLibrary :: Version
cabalVersionLibrary = U.cabalVersion

requireProgramM :: Program -> TestM ConfiguredProgram
requireProgramM program = do
  env <- getTestEnv
  (configured_program, _) <-
    liftIO $
      requireProgram
        (testVerbosity env)
        program
        (testProgramDb env)
  return configured_program

needProgramM :: String -> TestM (Maybe ConfiguredProgram)
needProgramM program = do
  env <- getTestEnv
  return $ lookupProgramByName program (testProgramDb env)

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
      progdb <-
        liftIO $
          configureProgram
            (testVerbosity env)
            program
            (testProgramDb env)
      case lookupProgram program progdb of
        Just _ -> return True
        Nothing -> return False

getMarkedOutput :: String -> String -- trailing newline
getMarkedOutput out = unlines (go (lines out) False)
  where
    go [] _ = []
    go (x : xs) True
      | "-----END CABAL OUTPUT-----" `isPrefixOf` x =
          go xs False
      | otherwise = x : go xs True
    go (x : xs) False
      -- NB: Windows has extra goo at the end
      | "-----BEGIN CABAL OUTPUT-----" `isPrefixOf` x =
          go xs True
      | otherwise = go xs False

data TestEnv = TestEnv
  -- UNCHANGING:

  { testSourceDir :: FilePath
  -- ^ Path to the test directory, as specified by path to test
  -- script.
  , testTmpDir :: FilePath
  -- ^ Somewhere to stow temporary files needed by the test.
  , testSubName :: String
  -- ^ Test sub-name, used to qualify dist/database directory to avoid
  -- conflicts.
  , testMode :: String
  -- ^ Test mode, further qualifies multiple invocations of the
  -- same test source code.
  , testProgramDb :: ProgramDb
  -- ^ Program database to use when we want ghc, ghc-pkg, etc.
  , testCompiler :: Compiler
  -- ^ Compiler we are running tests for
  , testCompilerPath :: FilePath
  , testPlatform :: Platform
  -- ^ Platform we are running tests on
  , testPackageDBStack :: PackageDBStackCWD
  -- ^ Package database stack (actually this changes lol)
  , testVerbosityFlags :: VerbosityFlags
  -- ^ How verbose to be
  , testMtimeChangeDelay :: Maybe Int
  -- ^ How long we should 'threadDelay' to make sure the file timestamp is
  -- updated correctly for recompilation tests.  Nothing if we haven't
  -- calibrated yet.
  , testScriptEnv :: ScriptEnv
  -- ^ Script environment for runghc
  , testSetupPath :: FilePath
  -- ^ Setup script path
  , testPackageDbPath :: Maybe [FilePath]
  -- ^ Setup package-db path which contains Cabal and Cabal-syntax for cabal-install to
  -- use when compiling custom setups, plus the store with possible dependencies of those setup packages.
  , testSkipSetupTests :: Bool
  -- ^ Skip Setup tests?
  , testHaveCabalShared :: Bool
  -- ^ Do we have shared libraries for the Cabal-under-tests?
  -- This is used for example to determine whether we can build
  -- detailed-0.9 tests dynamically, since they link against Cabal-under-test.
  , -- CHANGING:

    testEnvironment :: [(String, Maybe String)]
  -- ^ Environment override
  , testShouldFail :: Bool
  -- ^ When true, we invert the meaning of command execution failure
  , testRelativeCurrentDir :: FilePath
  -- ^ The current working directory, relative to 'testSourceDir'
  , testHavePackageDb :: Bool
  -- ^ Says if we've initialized the per-test package DB
  , testHaveRepo :: Bool
  -- ^ Says if we've setup a repository
  , testCabalInstallAsSetup :: Bool
  -- ^ Says if we're testing cabal-install as setup
  , testCabalProjectFile :: Maybe FilePath
  -- ^ Says what cabal.project file to use (probed)
  , testPlan :: Maybe Plan
  -- ^ Cached record of the plan metadata from a new-build
  -- invocation; controlled by 'withPlan'.
  , testRecordDefaultMode :: RecordMode
  -- ^ If user mode is not set, this is the record mode we default to.
  , testRecordUserMode :: Maybe RecordMode
  -- ^ User explicitly set record mode.  Not implemented ATM.
  , testMaybeStoreDir :: Maybe FilePath
  -- ^ Path to the storedir used by the test, if not the default
  }
  deriving (Show)

testVerbosity :: TestEnv -> Verbosity
testVerbosity = mkVerbosity defaultVerbosityHandles . testVerbosityFlags

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
testCurrentDir env = testSourceCopyDir env </> testRelativeCurrentDir env

testName :: TestEnv -> String
testName env = testSubName env <.> testMode env

-- | The absolute path to the directory containing all the
-- files for ALL tests associated with a test (respecting
-- subtests.)  To clean, you ONLY need to delete this directory.
testWorkDir :: TestEnv -> FilePath
testWorkDir env = testTmpDir env </> (testName env <.> "dist")

-- The folder which TMPDIR is set to.
-- This is different to testTmpDir, which is the folder which is the test is run from.
testSystemTmpDir :: TestEnv -> FilePath
testSystemTmpDir env = testWorkDir env </> "tmp"

-- | The absolute prefix where installs go.
testPrefixDir :: TestEnv -> FilePath
testPrefixDir env = testWorkDir env </> "usr"

-- | The absolute path where library installs go.
testLibInstallDir :: TestEnv -> FilePath
testLibInstallDir env = libDir </> compilerDir
  where
    platform@(Platform _ os) = testPlatform env
    libDir = case os of
      Windows -> testPrefixDir env
      _ -> testPrefixDir env </> "lib"
    compilerDir = prettyShow platform ++ "-" ++ showCompilerIdWithAbi (testCompiler env)

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
testSourceCopyDir env = testTmpDir env

-- | The user cabal directory
testCabalDir :: TestEnv -> FilePath
testCabalDir env = testHomeDir env </> ".cabal"

testStoreDir :: TestEnv -> FilePath
testStoreDir env = case testMaybeStoreDir env of
  Just dir -> dir
  Nothing -> testCabalDir env </> "store"

-- | The user cabal config file
testUserCabalConfigFile :: TestEnv -> FilePath
testUserCabalConfigFile env = testCabalDir env </> "config"

-- | The file where the expected output of the test lives
--
-- Pointing to the @testTmpDir@ allows us to modify the expected output if
-- needed, to adapt it to outcomes of previous steps in the test.
testExpectFile :: TestEnv -> FilePath
testExpectFile env = testTmpDir env </> testName env <.> "out"

-- | The file where the expected output of the test is written in @--accept@ mode
--
-- Note: This needs to point to `testSourceDir` so the output is visible in the
-- user's repository.
testExpectAcceptFile :: TestEnv -> FilePath
testExpectAcceptFile env = testSourceDir env </> testName env <.> "out"

-- | Where we store the actual output
testActualFile :: TestEnv -> FilePath
testActualFile env = testWorkDir env </> testName env <.> "comp.out"

-- | Where we will write the normalized actual file (for diffing)
testNormalizedActualFile :: TestEnv -> FilePath
testNormalizedActualFile env = testActualFile env <.> "normalized"

-- | Where we will write the normalized expected file (for diffing)
testNormalizedExpectFile :: TestEnv -> FilePath
testNormalizedExpectFile env = testWorkDir env </> testName env <.> "out.normalized"
