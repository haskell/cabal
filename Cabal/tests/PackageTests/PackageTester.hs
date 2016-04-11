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
    , packageDir
    , distDir
    , relativeDistDir
    , sharedDBPath
    , getWithGhcPath

    -- * Running cabal commands
    , cabal
    , cabal'
    , cabal_build
    , cabal_install
    , ghcPkg
    , ghcPkg'
    , compileSetup
    , run
    , runExe
    , runExe'
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
    , withSymlink

    -- * Test trees
    , TestTreeM
    , runTestTree
    , testTree
    , testTree'
    , groupTests
    , mapTestTrees
    , testWhen
    , testUnless
    , unlessWindows
    , hasSharedLibraries

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
import Distribution.System (OS(Windows), buildOS)
import Distribution.Simple.Utils
    ( printRawCommandAndArgsAndEnv, withFileContents )
import Distribution.Simple.Configure
    ( getPersistBuildConfig )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.BuildPaths (exeExtension)

#ifndef CURRENT_COMPONENT_ID
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Text (display)
#endif

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
import Data.Version
import System.Directory
    ( doesFileExist, canonicalizePath, createDirectoryIfMissing
    , removeDirectoryRecursive, getPermissions, setPermissions
    , setOwnerExecutable )
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process (runProcess, waitForProcess, showCommandForUser)
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
                    testEnvironment = []
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
    -- | Path to GHC that was used to compile Cabal library under test.
    { ghcPath :: FilePath
    -- | Version of GHC that compiled Cabal.
    , ghcVersion :: Version
    -- | Path to ghc-pkg corresponding to 'ghcPath'.
    , ghcPkgPath :: FilePath
    -- | Path to GHC that we should use to "./Setup --with-ghc"
    , withGhcPath :: FilePath
    -- | Version of GHC at 'withGhcPath'.
    , withGhcVersion :: Version
    -- | The build directory that was used to build Cabal (used
    -- to compile Setup scripts.)
    , cabalDistPref :: FilePath
    -- | Configuration options you can use to make the Cabal
    -- being tested visible (e.g. if you're using the test runner).
    -- We don't add these by default because then you have to
    -- link against Cabal which makes the build go longer.
    , packageDBStack :: PackageDBStack
    -- | How verbose should we be
    , suiteVerbosity :: Verbosity
    -- | The absolute current working directory
    , absoluteCWD :: FilePath
    }

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
                -- Would really like to do this, but we're not always
                -- going to be building against sufficiently recent
                -- Cabal which provides this macro.
                -- , "--dependency=Cabal=" ++ THIS_PACKAGE_KEY
                -- These flags make the test suite run faster
                -- Can't do this unless we LD_LIBRARY_PATH correctly
                -- , "--enable-executable-dynamic"
                , "--disable-optimization"
                -- Specify where we want our installed packages to go
                , "--prefix=" ++ prefix_dir
                ] -- Only add the LBI package stack if the GHC version
                  -- matches.
                  ++ (if withGhcPath suite == ghcPath suite
                        then packageDBParams (packageDBStack suite)
                        else [])
                  ++ extraArgs0
            -- This gives us MUCH better error messages
            "build" -> "-v" : extraArgs0
            _ -> extraArgs0
    -- This is a horrible hack to make hpc work correctly
    dist_dir <- relativeDistDir
    let extraArgs = ["--distdir", dist_dir] ++ extraArgs1
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
    r <- rawRun verbosity (Just path) (ghcPath suite) e $
        [ "--make"] ++
        ghcPackageDBParams (ghcVersion suite) (packageDBStack suite) ++
        [ "-hide-all-packages"
        , "-package base"
#ifdef CURRENT_COMPONENT_ID
        -- This is best, but we don't necessarily have it
        -- if we're bootstrapping with old Cabal.
        , "-package-id " ++ CURRENT_COMPONENT_ID
#else
        -- This mostly works, UNLESS you've installed a
        -- version of Cabal with the SAME version number.
        -- Then old GHCs will incorrectly select the installed
        -- version (because it prefers the FIRST package it finds.)
        -- It also semi-works to not specify "-hide-all-packages"
        -- at all, except if there's a later version of Cabal
        -- installed GHC will prefer that.
        , "-package Cabal-" ++ display cabalVersion
#endif
        , "-O0"
        , "Setup.hs" ]
    unless (resultExitCode r == ExitSuccess) $
        error $
        "could not build shared Setup executable\n" ++
        "  ran: " ++ resultCommand r ++ "\n" ++
        "  output:\n" ++ resultOutput r ++ "\n\n"

ghcPackageDBParams :: Version -> PackageDBStack -> [String]
ghcPackageDBParams ghc_version dbs
    | ghc_version >= Version [7,6] []
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
    db_path <- sharedDBPath
    (config, test) <- ask
    unless (testPackageDb test) $
        error "Must initialize package database using withPackageDb"
    let db_stack = packageDBStack config ++ [SpecificPackageDB db_path]
        extraArgs = ghcPkgPackageDBParams (ghcVersion config) db_stack
    run Nothing (ghcPkgPath config) (cmd : extraArgs ++ args)

ghcPkgPackageDBParams :: Version -> PackageDBStack -> [String]
ghcPkgPackageDBParams version dbs = concatMap convert dbs where
    convert :: PackageDB -> [String]
    -- Ignoring global/user is dodgy but there's no way good
    -- way to give ghc-pkg the correct flags in this case.
    convert  GlobalPackageDB         = []
    convert  UserPackageDB           = []
    convert (SpecificPackageDB path)
        | version >= Version [7,6] []
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
                canonicalizePath (if pathExists then path
                                                else path <.> exeExtension)
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
    build_dir <- distDir
    (suite, _) <- ask
    liftIO $ createDirectoryIfMissing True build_dir
    liftIO $ C.appendFile (build_dir </> "test.log")
                         (C.pack $ "+ " ++ resultCommand res ++ "\n"
                            ++ resultOutput res ++ "\n\n")
    let test_sh = build_dir </> "test.sh"
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
                    Nothing -> resultCommand res
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
    when (p (ghcVersion suite)) m

withPackage :: FilePath -> TestM a -> TestM a
withPackage f = withReaderT (\(suite, test) -> (suite, test { testCurrentPackage = f }))

-- TODO: Really should accumulate... but I think to do this
-- properly we can't just append
withEnv :: [(String, Maybe String)] -> TestM a -> TestM a
withEnv e m = do
    (_, test0) <- ask
    when (not (null (testEnvironment test0)))
        $ error "nested withEnv (not yet) supported"
    withReaderT (\(suite, test) -> (suite, test { testEnvironment = e })) m

withPackageDb :: TestM a -> TestM a
withPackageDb m = do
    (_, test0) <- ask
    db_path <- sharedDBPath
    if testPackageDb test0
        then m
        else withReaderT (\(suite, test) ->
                            (suite { packageDBStack
                                        = packageDBStack suite
                                       ++ [SpecificPackageDB db_path] },
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

testTree :: SuiteConfig -> String -> Maybe String -> TestM a -> TestTreeM ()
testTree config name subname m =
    testTree' $ HUnit.testCase name $ runTestM config name subname m

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
    buildOS /= Windows || withGhcVersion config < Version [7,8] []

------------------------------------------------------------------------
-- Verbosity

getVerbosity :: TestM Verbosity
getVerbosity = fmap (suiteVerbosity . fst) ask
