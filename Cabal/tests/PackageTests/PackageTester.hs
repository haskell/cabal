{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE CPP #-}

module PackageTests.PackageTester
    ( PackageSpec
    , SuiteConfig(..)
    , TestConfig(..)
    , Result(..)
    , TestM
    , runTestM

    -- * Paths
    , ghcPath
    , withGhcPath
    , ghcPkgPath
    , withGhcPkgPath

    , packageDir
    , distDir
    , relativeDistDir
    , sharedDBPath
    , getWithGhcPath
    , prefixDir

    -- * Running cabal commands
    , cabal
    , cabal'
    , cabal_build
    , cabal_install
    , cabal_install_with_docs
    , ghcPkg
    , ghcPkg'
    , compileSetup
    , shell
    , run
    , runExe
    , runExe'
    , runInstalledExe
    , runInstalledExe'
    , rawRun
    , rawCompileSetup
    , withPackage
    , withEnv
    , withPackageDb

    -- * Polymorphic versions of HUnit functions
    , assertFailure
    , assertEqual
    , assertBool
    , shouldExist
    , shouldNotExist

    -- * Test helpers
    , shouldFail
    , whenGhcVersion
    , assertOutputContains
    , assertOutputDoesNotContain
    , assertFindInFile
    , concatOutput
    , ghcFileModDelay
    , withSymlink

    -- * Test trees
    , TestTreeM
    , runTestTree
    , testTree
    , testTreeSteps
    , testTreeSub
    , testTreeSubSteps
    , testTree'
    , groupTests
    , mapTestTrees
    , testWhen
    , testUnless
    , unlessWindows
    , hasSharedLibraries
    , hasCabalForGhc

    , getPersistBuildConfig

    -- Common utilities
    , module System.FilePath
    , module Data.List
    , module Control.Monad.IO.Class
    , module Text.Regex.Posix
    ) where

import PackageTests.Options

import Distribution.Compat.CreatePipe (createPipe)
import Distribution.Simple.Compiler (PackageDBStack, PackageDB(..))
import Distribution.Simple.Program.Run (getEffectiveEnvironment)
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Simple.Program
import Distribution.System (OS(Windows), buildOS)
import Distribution.Simple.Utils
    ( printRawCommandAndArgsAndEnv, withFileContents )
import Distribution.Simple.Configure
    ( getPersistBuildConfig )
import Distribution.Verbosity (Verbosity)
import Distribution.Version

import Distribution.Simple.BuildPaths (exeExtension)

import Distribution.Simple.Utils (cabalVersion)
import Distribution.Text (display)

import qualified Test.Tasty.HUnit as HUnit
import Text.Regex.Posix

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Data.List
import System.Directory
    ( doesFileExist, canonicalizePath, createDirectoryIfMissing
    , removeDirectoryRecursive, getPermissions, setPermissions
    , setOwnerExecutable )
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process (runProcess, waitForProcess, showCommandForUser)
import Control.Concurrent (threadDelay)
import Test.Tasty (TestTree, askOption, testGroup)

#ifndef mingw32_HOST_OS
import Control.Monad.Catch ( bracket_ )
import System.Directory    ( removeFile )
import System.Posix.Files  ( createSymbolicLink )
#endif

-- | Our test monad maintains an environment recording the global test
-- suite configuration 'SuiteConfig', and the local per-test
-- configuration 'TestConfig'.
type TestM = ReaderT (SuiteConfig, TestConfig) IO

-- | Run a test in the test monad.
runTestM :: SuiteConfig -> FilePath -> Maybe String -> TestM a -> IO ()
runTestM suite name subname m = do
    let test = TestConfig {
                    testMainName = name,
                    testSubName = subname,
                    testShouldFail = False,
                    testCurrentPackage = ".",
                    testPackageDb = False,
                    -- Try to avoid Unicode output
                    testEnvironment = [("LC_ALL", Just "C")]
               }
    void (runReaderT (cleanup >> m) (suite, test))
  where
    -- TODO: option not to clean up dist dirs; this should be
    -- harmless!
    cleanup = do
        onlyIfExists . removeDirectoryRecursive =<< topDir

-- | Run an IO action, and suppress a "does not exist" error.
onlyIfExists :: MonadIO m => IO () -> m ()
onlyIfExists m = liftIO $
                 E.catch m $ \(e :: IOError) ->
                    if isDoesNotExistError e
                        then return ()
                        else E.throwIO e

-- cleaning up:
--  cabal clean will clean up dist directory, but we also need to zap
--  Setup etc.
--
-- Suggestion: just copy the files somewhere else!

-- | Global configuration for the entire test suite.
data SuiteConfig = SuiteConfig
    -- | The programs used to build the Cabal under test.
    -- Invariant: ghc and ghc-pkg are configured.
    { bootProgramDb :: ProgramDb
    -- | The programs that are requested using @--with-compiler@
    -- and @--with-hc-pkg@ to Cabal the under-test.
    -- Invariant: ghc and ghc-pkg are configured.
    , withProgramDb :: ProgramDb
    -- | The build directory that was used to build Cabal (used
    -- to compile Setup scripts.)
    , cabalDistPref :: FilePath
    -- | The package database stack which makes the *built*
    -- Cabal well-formed.  In general, this is going to be
    -- the package DB stack from the LBI you used to build
    -- Cabal, PLUS the inplace database (since you also want Cabal).
    -- TODO: I forgot what this comment means:
    -- We don't add these by default because then you have to
    -- link against Cabal which makes the build go longer.
    , packageDBStack :: PackageDBStack
    -- | The package database stack for 'withGhcPath'.  This
    -- is ignored if @'withGhcPath' suite == 'ghcPath' suite@.
    -- We don't assume this includes the inplace database (since
    -- Cabal would have been built with the wrong version of GHC,
    -- so the databases aren't compatible anyway.)
    , withGhcDBStack :: PackageDBStack
    -- | How verbose should we be
    , suiteVerbosity :: Verbosity
    -- | The absolute current working directory
    , absoluteCWD :: FilePath
    -- | How long we should 'threadDelay' to make sure the file timestamp is
    -- updated correctly for recompilation tests.
    , mtimeChangeDelay :: Int
    }

getProgram :: ProgramDb -> Program -> ConfiguredProgram
getProgram progdb program = prog
    where Just prog = lookupProgram program progdb -- invariant!

getBootProgram :: SuiteConfig -> Program -> ConfiguredProgram
getBootProgram suite = getProgram (bootProgramDb suite)

getWithProgram :: SuiteConfig -> Program -> ConfiguredProgram
getWithProgram suite = getProgram (withProgramDb suite)

ghcProg :: SuiteConfig -> ConfiguredProgram
ghcProg suite = getBootProgram suite ghcProgram

withGhcProg :: SuiteConfig -> ConfiguredProgram
withGhcProg suite = getWithProgram suite ghcProgram

withGhcPkgProg :: SuiteConfig -> ConfiguredProgram
withGhcPkgProg suite = getWithProgram suite ghcPkgProgram

programVersion' :: ConfiguredProgram -> Version
programVersion' prog = version
    where Just version = programVersion prog -- invariant!

ghcPath :: SuiteConfig -> FilePath
ghcPath = programPath . ghcProg

-- Basically you should never use these two...

ghcPkgProg :: SuiteConfig -> ConfiguredProgram
ghcPkgProg suite = getBootProgram suite ghcPkgProgram

ghcPkgPath :: SuiteConfig -> FilePath
ghcPkgPath = programPath . ghcPkgProg

ghcVersion :: SuiteConfig -> Version
ghcVersion = programVersion' . ghcProg

withGhcPath :: SuiteConfig -> FilePath
withGhcPath = programPath . withGhcProg

withGhcVersion :: SuiteConfig -> Version
withGhcVersion = programVersion' . withGhcProg

-- Ditto...
withGhcPkgPath :: SuiteConfig -> FilePath
withGhcPkgPath = programPath . withGhcPkgProg

-- Selects the correct database stack to pass to the Cabal under
-- test as the "database stack" to use for testing.  This may be
-- something different if the GHC we want Cabal to use is different
-- from the GHC Cabal was built with.
testDBStack :: SuiteConfig -> PackageDBStack
testDBStack suite
    | withGhcPath suite == ghcPath suite
    = packageDBStack suite
    | otherwise
    = withGhcDBStack suite

data TestConfig = TestConfig
    -- | Test name, MUST be the directory the test packages live in
    -- relative to tests/PackageTests
    { testMainName :: FilePath
    -- | Test sub-name, used to qualify dist/database directory to avoid
    -- conflicts.
    , testSubName :: Maybe String
    -- | This gets modified sometimes
    , testShouldFail :: Bool
    -- | The "current" package, ala current directory
    , testCurrentPackage :: PackageSpec
    -- | Says if we've initialized the per-test package DB
    , testPackageDb :: Bool
    -- | Environment override
    , testEnvironment :: [(String, Maybe String)]
    }

-- | A package that can be built.
type PackageSpec = FilePath

------------------------------------------------------------------------
-- * Directories

simpleSetupPath :: TestM FilePath
simpleSetupPath = do
    (suite, _) <- ask
    return (absoluteCWD suite </> "tests/Setup")

-- | The absolute path to the directory containing the files for
-- this tests; usually @Check.hs@ and any test packages.
testDir :: TestM FilePath
testDir = do
    (suite, test) <- ask
    return $ absoluteCWD suite </> "tests/PackageTests" </> testMainName test

-- | The absolute path to the root of the package directory; it's
-- where the Cabal file lives.  This is what you want the CWD of cabal
-- calls to be.
packageDir :: TestM FilePath
packageDir = do
    (_, test) <- ask
    test_dir <- testDir
    return $ test_dir </> testCurrentPackage test

-- | The absolute path to the directory containing all the
-- files for ALL tests associated with a test (respecting
-- subtests.)  To clean, you ONLY need to delete this directory.
topDir :: TestM FilePath
topDir = do
    test_dir <- testDir
    (_, test) <- ask
    return $ test_dir </>
                case testSubName test of
                    Nothing -> "dist-test"
                    Just n -> "dist-test." ++ n

prefixDir :: TestM FilePath
prefixDir = do
    top_dir <- topDir
    return $ top_dir </> "usr"

-- | The absolute path to the build directory that should be used
-- for the current package in a test.
distDir :: TestM FilePath
distDir = do
    top_dir <- topDir
    (_, test) <- ask
    return $ top_dir </> testCurrentPackage test </> "dist"

definitelyMakeRelative :: FilePath -> FilePath -> FilePath
definitelyMakeRelative base0 path0 =
    let go [] path = joinPath path
        go base [] = joinPath (replicate (length base) "..")
        go (".":xs) ys = go xs ys
        go xs (".":ys) = go xs ys
        go (x:xs) (y:ys)
            | x == y    = go xs ys
            | otherwise = go (x:xs) [] </> go [] (y:ys)
    in go (splitPath base0) (splitPath path0)

-- hpc is stupid and doesn't understand absolute paths.
relativeDistDir :: TestM FilePath
relativeDistDir = do
    dist_dir0 <- distDir
    pkg_dir <- packageDir
    return $ definitelyMakeRelative pkg_dir dist_dir0

-- | The absolute path to the shared package database that should
-- be used by all packages in this test.
sharedDBPath :: TestM FilePath
sharedDBPath = do
    top_dir <- topDir
    return $ top_dir </> "packagedb"

getWithGhcPath :: TestM FilePath
getWithGhcPath = do
    (suite, _) <- ask
    return $ withGhcPath suite

------------------------------------------------------------------------
-- * Running cabal

cabal :: String -> [String] -> TestM ()
cabal cmd extraArgs0 = void (cabal' cmd extraArgs0)

cabal' :: String -> [String] -> TestM Result
cabal' cmd extraArgs0 = do
    (suite, test) <- ask
    prefix_dir <- prefixDir
    when ((cmd == "register" || cmd == "copy") && not (testPackageDb test)) $
        error "Cannot register/copy without using 'withPackageDb'"
    let extraArgs1 = case cmd of
            "configure" ->
                -- If the package database is empty, setting --global
                -- here will make us error loudly if we try to install
                -- into a bad place.
                [ "--global"
                , "--with-ghc", withGhcPath suite
                -- This improves precision but it increases the number
                -- of flags one has to specify and I don't like that;
                -- Cabal is going to configure it and usually figure
                -- out the right location in any case.
                -- , "--with-ghc-pkg", withGhcPkgPath suite
                -- These flags make the test suite run faster
                -- Can't do this unless we LD_LIBRARY_PATH correctly
                -- , "--enable-executable-dynamic"
                , "--disable-optimization"
                -- Specify where we want our installed packages to go
                , "--prefix=" ++ prefix_dir
                ] -- Only add the LBI package stack if the GHC version
                  -- matches.
                  ++ packageDBParams (testDBStack suite)
                  ++ extraArgs0
            _ -> extraArgs0
    -- This is a horrible hack to make hpc work correctly
    dist_dir <- relativeDistDir
    let extraArgs = ["-v", "--distdir", dist_dir] ++ extraArgs1
    doCabal (cmd:extraArgs)

-- | This abstracts the common pattern of configuring and then building.
cabal_build :: [String] -> TestM ()
cabal_build args = do
    cabal "configure" args
    cabal "build" []
    return ()

-- | This abstracts the common pattern of "installing" a package.
cabal_install :: [String] -> TestM ()
cabal_install args = do
    cabal "configure" args
    cabal "build" []
    cabal "copy" []
    cabal "register" []
    return ()

-- | This abstracts the common pattern of "installing" a package,
-- with haddock documentation.
cabal_install_with_docs :: [String] -> TestM ()
cabal_install_with_docs args = do
    cabal "configure" args
    cabal "build" []
    cabal "haddock" []
    cabal "copy" []
    cabal "register" []
    return ()

-- | Determines what Setup executable to run and runs it
doCabal :: [String]  -- ^ extra arguments
      -> TestM Result
doCabal cabalArgs = do
    pkg_dir <- packageDir
    customSetup <- liftIO $ doesFileExist (pkg_dir </> "Setup.hs")
    if customSetup
        then do
            compileSetup
            -- TODO make this less racey
            let path = pkg_dir </> "Setup"
            run (Just pkg_dir) path cabalArgs
        else do
            -- Use shared Setup executable (only for Simple build types).
            path <- simpleSetupPath
            run (Just pkg_dir) path cabalArgs

packageDBParams :: PackageDBStack -> [String]
packageDBParams dbs = "--package-db=clear"
                    : map (("--package-db=" ++) . convert) dbs
  where
    convert :: PackageDB -> String
    convert  GlobalPackageDB         = "global"
    convert  UserPackageDB           = "user"
    convert (SpecificPackageDB path) = path

------------------------------------------------------------------------
-- * Compiling setup scripts

compileSetup :: TestM ()
compileSetup = do
    (suite, test) <- ask
    pkg_path <- packageDir
    liftIO $ rawCompileSetup (suiteVerbosity suite) suite (testEnvironment test) pkg_path

rawCompileSetup :: Verbosity -> SuiteConfig -> [(String, Maybe String)] -> FilePath -> IO ()
rawCompileSetup verbosity suite e path = do
    -- NB: Use 'ghcPath', not 'withGhcPath', since we need to be able to
    -- link against the Cabal library which was built with 'ghcPath'.
    -- Ditto with packageDBStack.
    r <- rawRun verbosity (Just path) (ghcPath suite) e $
        [ "--make"] ++
        ghcPackageDBParams (ghcVersion suite) (packageDBStack suite) ++
        [ "-hide-package Cabal"
        -- This mostly works, UNLESS you've installed a
        -- version of Cabal with the SAME version number.
        -- Then old GHCs will incorrectly select the installed
        -- version (because it prefers the FIRST package it finds.)
        -- It also semi-works to not specify "-hide-all-packages"
        -- at all, except if there's a later version of Cabal
        -- installed GHC will prefer that.
        , "-package Cabal-" ++ display cabalVersion
        , "-O0"
        , "Setup.hs" ]
    unless (resultExitCode r == ExitSuccess) $
        error $
        "could not build shared Setup executable\n" ++
        "  ran: " ++ resultCommand r ++ "\n" ++
        "  output:\n" ++ resultOutput r ++ "\n\n"

ghcPackageDBParams :: Version -> PackageDBStack -> [String]
ghcPackageDBParams ghc_version dbs
    | ghc_version >= mkVersion [7,6]
    = "-clear-package-db" : map convert dbs
    | otherwise
    = concatMap convertLegacy dbs
  where
    convert :: PackageDB -> String
    convert  GlobalPackageDB         = "-global-package-db"
    convert  UserPackageDB           = "-user-package-db"
    convert (SpecificPackageDB path) = "-package-db=" ++ path

    convertLegacy :: PackageDB -> [String]
    convertLegacy (SpecificPackageDB path) = ["-package-conf=" ++ path]
    convertLegacy _ = []

------------------------------------------------------------------------
-- * Running ghc-pkg

ghcPkg :: String -> [String] -> TestM ()
ghcPkg cmd args = void (ghcPkg' cmd args)

ghcPkg' :: String -> [String] -> TestM Result
ghcPkg' cmd args = do
    (config, test) <- ask
    unless (testPackageDb test) $
        error "Must initialize package database using withPackageDb"
    -- NB: testDBStack already has the local database
    let db_stack = testDBStack config
        extraArgs = ghcPkgPackageDBParams (withGhcVersion config) db_stack
    run Nothing (withGhcPkgPath config) (cmd : extraArgs ++ args)

ghcPkgPackageDBParams :: Version -> PackageDBStack -> [String]
ghcPkgPackageDBParams version dbs = concatMap convert dbs where
    convert :: PackageDB -> [String]
    -- Ignoring global/user is dodgy but there's no way good
    -- way to give ghc-pkg the correct flags in this case.
    convert  GlobalPackageDB         = []
    convert  UserPackageDB           = []
    convert (SpecificPackageDB path)
        | version >= mkVersion [7,6]
        = ["--package-db=" ++ path]
        | otherwise
        = ["--package-conf=" ++ path]

------------------------------------------------------------------------
-- * Running other things

-- | Run an executable that was produced by cabal.  The @exe_name@
-- is precisely the name of the executable section in the file.
runExe :: String -> [String] -> TestM ()
runExe exe_name args = void (runExe' exe_name args)

runExe' :: String -> [String] -> TestM Result
runExe' exe_name args = do
    dist_dir <- distDir
    let exe = dist_dir </> "build" </> exe_name </> exe_name
    run Nothing exe args

-- | Run an executable that was installed by cabal.  The @exe_name@
-- is precisely the name of the executable.
runInstalledExe :: String -> [String] -> TestM ()
runInstalledExe exe_name args = void (runInstalledExe' exe_name args)

runInstalledExe' :: String -> [String] -> TestM Result
runInstalledExe' exe_name args = do
    usr <- prefixDir
    let exe = usr </> "bin" </> exe_name
    run Nothing exe args

shell :: String -> [String] -> TestM Result
shell exe args = do
    pkg_dir <- packageDir
    run (Just pkg_dir) exe args

run :: Maybe FilePath -> String -> [String] -> TestM Result
run mb_cwd path args = do
    verbosity <- getVerbosity
    (_, test) <- ask
    r <- liftIO $ rawRun verbosity mb_cwd path (testEnvironment test) args
    record r
    requireSuccess r

rawRun :: Verbosity -> Maybe FilePath -> String -> [(String, Maybe String)] -> [String] -> IO Result
rawRun verbosity mb_cwd path envOverrides args = do
    -- path is relative to the current directory; canonicalizePath makes it
    -- absolute, so that runProcess will find it even when changing directory.
    path' <- do pathExists <- doesFileExist path
                exePathExists <- doesFileExist (path <.> exeExtension)
                case () of
                 _ | pathExists    -> canonicalizePath path
                   | exePathExists -> canonicalizePath (path <.> exeExtension)
                   | otherwise     -> return path
    menv <- getEffectiveEnvironment envOverrides

    printRawCommandAndArgsAndEnv verbosity path' args menv
    (readh, writeh) <- createPipe
    pid <- runProcess path' args mb_cwd menv Nothing (Just writeh) (Just writeh)

    out <- hGetContents readh
    void $ E.evaluate (length out) -- force the output
    hClose readh

    -- wait for the program to terminate
    exitcode <- waitForProcess pid
    return Result {
            resultExitCode = exitcode,
            resultDirectory = mb_cwd,
            resultCommand = showCommandForUser path' args,
            resultOutput = out
        }

------------------------------------------------------------------------
-- * Subprocess run results

data Result = Result
    { resultExitCode :: ExitCode
    , resultDirectory :: Maybe FilePath
    , resultCommand :: String
    , resultOutput :: String
    } deriving Show

requireSuccess :: Result -> TestM Result
requireSuccess r@Result { resultCommand = cmd
                        , resultExitCode = exitCode
                        , resultOutput = output } = do
    (_, test) <- ask
    when (exitCode /= ExitSuccess && not (testShouldFail test)) $
        assertFailure $ "Command " ++ cmd ++ " failed.\n" ++
        "Output:\n" ++ output ++ "\n"
    when (exitCode == ExitSuccess && testShouldFail test) $
        assertFailure $ "Command " ++ cmd ++ " succeeded.\n" ++
        "Output:\n" ++ output ++ "\n"
    return r

record :: Result -> TestM ()
record res = do
    log_dir <- topDir
    (suite, _) <- ask
    liftIO $ createDirectoryIfMissing True log_dir
    liftIO $ C.appendFile (log_dir </> "test.log")
                         (C.pack $ "+ " ++ resultCommand res ++ "\n"
                            ++ resultOutput res ++ "\n\n")
    let test_sh = log_dir </> "test.sh"
    b <- liftIO $ doesFileExist test_sh
    when (not b) . liftIO $ do
        -- This is hella racey but this is not that security important
        C.appendFile test_sh
            (C.pack $ "#/bin/sh\nset -ev\n" ++
                      "cd "++ show (absoluteCWD suite) ++"\n")
        perms <- getPermissions test_sh
        setPermissions test_sh (setOwnerExecutable True perms)

    liftIO $ C.appendFile test_sh
                (C.pack
                  (case resultDirectory res of
                    Nothing -> resultCommand res ++ "\n"
                    Just d -> "(cd " ++ show d ++ " && " ++ resultCommand res ++ ")\n"))

------------------------------------------------------------------------
-- * Test helpers

assertFailure :: MonadIO m => String -> m ()
assertFailure = liftIO . HUnit.assertFailure

assertEqual :: (Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
assertEqual s x y = liftIO $ HUnit.assertEqual s x y

assertBool :: MonadIO m => String -> Bool -> m ()
assertBool s x = liftIO $ HUnit.assertBool s x

shouldExist :: MonadIO m => FilePath -> m ()
shouldExist path = liftIO $ doesFileExist path >>= assertBool (path ++ " should exist")

shouldNotExist :: MonadIO m => FilePath -> m ()
shouldNotExist path =
    liftIO $ doesFileExist path >>= assertBool (path ++ " should exist") . not

shouldFail :: TestM a -> TestM a
shouldFail = withReaderT (\(suite, test) -> (suite, test { testShouldFail = not (testShouldFail test) }))

whenGhcVersion :: (Version -> Bool) -> TestM () -> TestM ()
whenGhcVersion p m = do
    (suite, _) <- ask
    when (p (withGhcVersion suite)) m

withPackage :: FilePath -> TestM a -> TestM a
withPackage f = withReaderT (\(suite, test) -> (suite, test { testCurrentPackage = f }))

-- We append to the environment list, as per 'getEffectiveEnvironment'
-- which prefers the latest override.
withEnv :: [(String, Maybe String)] -> TestM a -> TestM a
withEnv e m = do
    withReaderT (\(suite, test) -> (suite, test { testEnvironment = testEnvironment test ++ e })) m

withPackageDb :: TestM a -> TestM a
withPackageDb m = do
    (_, test0) <- ask
    db_path <- sharedDBPath
    if testPackageDb test0
        then m
        else withReaderT (\(suite, test) ->
                            (suite { packageDBStack
                                        = packageDBStack suite
                                       ++ [SpecificPackageDB db_path]
                                   , withGhcDBStack
                                        = withGhcDBStack suite
                                       ++ [SpecificPackageDB db_path]},
                             test { testPackageDb = True }))
               $ do ghcPkg "init" [db_path]
                    m

assertOutputContains :: MonadIO m => String -> Result -> m ()
assertOutputContains needle result =
    unless (needle `isInfixOf` (concatOutput output)) $
    assertFailure $
    " expected: " ++ needle ++ "\n" ++
    " in output: " ++ output ++ ""
  where output = resultOutput result

assertOutputDoesNotContain :: MonadIO m => String -> Result -> m ()
assertOutputDoesNotContain needle result =
    when (needle `isInfixOf` (concatOutput output)) $
    assertFailure $
    "unexpected: " ++ needle ++
    " in output: " ++ output
  where output = resultOutput result

assertFindInFile :: MonadIO m => String -> FilePath -> m ()
assertFindInFile needle path =
    liftIO $ withFileContents path
                 (\contents ->
                  unless (needle `isInfixOf` contents)
                         (assertFailure ("expected: " ++ needle ++ "\n" ++
                                         " in file: " ++ path)))

-- | Replace line breaks with spaces, correctly handling "\r\n".
concatOutput :: String -> String
concatOutput = unwords . lines . filter ((/=) '\r')

-- | Delay a sufficient period of time to permit file timestamp
-- to be updated.
ghcFileModDelay :: TestM ()
ghcFileModDelay = do
    (suite, _) <- ask
    -- For old versions of GHC, we only had second-level precision,
    -- so we need to sleep a full second.  Newer versions use
    -- millisecond level precision, so we only have to wait
    -- the granularity of the underlying filesystem.
    -- TODO: cite commit when GHC got better precision; this
    -- version bound was empirically generated.
    let delay | withGhcVersion suite < mkVersion [7,7]
              = 1000000 -- 1s
              | otherwise
              = mtimeChangeDelay suite
    liftIO $ threadDelay delay

-- | Create a symlink for the duration of the provided action. If the symlink
-- already exists, it is deleted. Does not work on Windows.
withSymlink :: FilePath -> FilePath -> TestM a -> TestM a
#ifdef mingw32_HOST_OS
withSymlink _oldpath _newpath _act =
  error "PackageTests.PackageTester.withSymlink: does not work on Windows!"
#else
withSymlink oldpath newpath act = do
  symlinkExists <- liftIO $ doesFileExist newpath
  when symlinkExists $ liftIO $ removeFile newpath
  bracket_ (liftIO $ createSymbolicLink oldpath newpath)
           (liftIO $ removeFile newpath) act
#endif

------------------------------------------------------------------------
-- * Test trees

-- | Monad for creating test trees. The option --enable-all-tests determines
-- whether to filter tests with 'testWhen' and 'testUnless'.
type TestTreeM = WriterT [TestTree] (Reader OptionEnableAllTests)

runTestTree :: String -> TestTreeM () -> TestTree
runTestTree name ts = askOption $
                      testGroup name . runReader (execWriterT ts)

testTree :: SuiteConfig -> String -> TestM a -> TestTreeM ()
testTree config name m =
    testTree' $ HUnit.testCase name $ runTestM config name Nothing m

testTreeSteps :: SuiteConfig -> String -> ((String -> TestM ()) -> TestM a) -> TestTreeM ()
testTreeSteps config name f =
    testTree' . HUnit.testCaseSteps name
              $ \step -> runTestM config name Nothing (f (liftIO . step))

testTreeSub :: SuiteConfig -> String -> String -> TestM a -> TestTreeM ()
testTreeSub config name sub_name m =
    testTree' $ HUnit.testCase (name </> sub_name) $ runTestM config name (Just sub_name) m

testTreeSubSteps :: SuiteConfig -> String -> String
                 -> ((String -> TestM ()) -> TestM a)
                 -> TestTreeM ()
testTreeSubSteps config name sub_name f =
    testTree' . HUnit.testCaseSteps (name </> sub_name)
              $ \step -> runTestM config name (Just sub_name) (f (liftIO . step))

testTree' :: TestTree -> TestTreeM ()
testTree' tc = tell [tc]

-- | Create a test group from the output of the given action.
groupTests :: String -> TestTreeM () -> TestTreeM ()
groupTests name = censor (\ts -> [testGroup name ts])

-- | Apply a function to each top-level test tree.
mapTestTrees :: (TestTree -> TestTree) -> TestTreeM a -> TestTreeM a
mapTestTrees = censor . map

testWhen :: Bool -> TestTreeM () -> TestTreeM ()
testWhen c test = do
  OptionEnableAllTests enableAll <- lift ask
  when (enableAll || c) test

testUnless :: Bool -> TestTreeM () -> TestTreeM ()
testUnless = testWhen . not

unlessWindows :: TestTreeM () -> TestTreeM ()
unlessWindows = testUnless (buildOS == Windows)

hasSharedLibraries :: SuiteConfig -> Bool
hasSharedLibraries config =
    buildOS /= Windows || withGhcVersion config < mkVersion [7,8]

hasCabalForGhc :: SuiteConfig -> Bool
hasCabalForGhc config =
    withGhcPath config == ghcPath config

------------------------------------------------------------------------
-- Verbosity

getVerbosity :: TestM Verbosity
getVerbosity = fmap (suiteVerbosity . fst) ask
