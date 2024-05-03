{-# LANGUAGE CPP                      #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE PatternGuards            #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

import Test.Cabal.Workdir
import Test.Cabal.Script
import Test.Cabal.Server
import Test.Cabal.Monad
import Test.Cabal.TestCode

import Distribution.Verbosity        (normal, verbose, Verbosity)
import Distribution.Simple.Utils     (getDirectoryContentsRecursive)
import Distribution.Simple.Program
import Distribution.Utils.Path       (getSymbolicPath)

import Options.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import GHC.Conc (numCapabilities)
import Data.List
import Text.Printf
import qualified System.Clock as Clock
import System.IO
import System.FilePath
import System.Exit
import System.Process (callProcess, showCommandForUser)
import System.Directory
import Distribution.Pretty
import Data.Maybe

#if !MIN_VERSION_base(4,12,0)
import Data.Monoid ((<>))
#endif
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif

{- Note [Testsuite package environments]

There are three different package environments which are used when running the
testsuite.

1. Environment used to compile `cabal-tests` executable
2. Environment used to run test scripts "setup.test.hs"
3. Environment made available to tests themselves via `./Setup configure`

These are all distinct from each other and should be specified separately.

Where are these environments specified:

1. The build-depends on `cabal-tests` executable in `cabal-testsuite.cabal`
2. The build-depends of `test-runtime-deps` executable in `cabal-testsuite.cabal`
   These dependencies are injected in a special module (`Test.Cabal.ScriptEnv0`) which
   then is consulted in `Test.Cabal.Monad` in order to pass the right environmnet.
   This is the mechanism by which the `./Setup` tests have access to the in-tree
   `Cabal`, `Cabal-syntax` and `Cabal-hooks` libraries.
3. No specification, only the `GlobalPackageDb` is available (see
   `testPackageDBStack`) unless the test itself augments the environment with
   `withPackageDb`.

At the moment, `cabal-install` tests always use the bootstrap cabal, which is a
bit confusing but `cabal-install` is not flexible enough to be given additional
package databases (yet).

-}

-- | Record for arguments that can be passed to @cabal-tests@ executable.
data MainArgs = MainArgs {
        mainArgThreads :: Int,
        mainArgTestPaths :: [String],
        mainArgHideSuccesses :: Bool,
        mainArgVerbose :: Bool,
        mainArgQuiet   :: Bool,
        mainArgDistDir :: Maybe FilePath,
        mainArgCabalSpec :: Maybe CabalLibSpec,
        mainCommonArgs :: CommonArgs
    }

data CabalLibSpec = BootCabalLib | InTreeCabalLib FilePath FilePath | SpecificCabalLib String FilePath

cabalLibSpecParser :: Parser CabalLibSpec
cabalLibSpecParser = bootParser <|> intreeParser <|> specificParser
  where
    bootParser = flag' BootCabalLib (long "boot-cabal-lib")
    intreeParser = InTreeCabalLib <$> strOption (long "intree-cabal-lib" <> metavar "ROOT")
                                  <*> option str ( help "Test TMP" <> long "test-tmp" )
    specificParser = SpecificCabalLib <$> strOption (long "specific-cabal-lib" <> metavar "VERSION")
                                      <*> option str ( help "Test TMP" <> long "test-tmp" )


-- | optparse-applicative parser for 'MainArgs'
mainArgParser :: Parser MainArgs
mainArgParser = MainArgs
    <$> option auto
        ( help "Number of threads to run"
       <> short 'j'
       <> showDefault
       <> value numCapabilities
       <> metavar "INT")
    <*> many (argument str (metavar "FILE"))
    <*> switch
        ( long "hide-successes"
       <> help "Do not print test cases as they are being run"
        )
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Be verbose"
        )
    <*> switch
        ( long "quiet"
       <> short 'q'
       <> help "Only output stderr on failure"
        )
    <*> optional (option str
        ( help "Dist directory we were built with"
       <> long "builddir"
       <> metavar "DIR"))
    <*> optional cabalLibSpecParser
    <*> commonArgParser

-- Unpack and build a specific released version of Cabal and Cabal-syntax libraries
buildCabalLibsProject :: String -> Verbosity -> Maybe FilePath -> FilePath -> IO [FilePath]
buildCabalLibsProject projString verb mbGhc dir = do
  let prog_db = userSpecifyPaths [("ghc", path) | Just path <- [mbGhc] ]  defaultProgramDb
  (cabal, _) <- requireProgram verb (simpleProgram "cabal") prog_db
  (ghc, _) <- requireProgram verb ghcProgram prog_db

  let storeRoot = dir </> "store"
  let pv = fromMaybe (error "no ghc version") (programVersion ghc)
  let final_package_db = dir </> "dist-newstyle" </> "packagedb" </> "ghc-" ++ prettyShow pv
  createDirectoryIfMissing True dir
  writeFile (dir </> "cabal.project-test") projString

  runProgramInvocation verb
    ((programInvocation cabal
      ["--store-dir", storeRoot
      , "--project-file=" ++ dir </> "cabal.project-test"
      , "build"
      , "-w", programPath ghc
      , "Cabal", "Cabal-syntax", "Cabal-hooks"
      ] ) { progInvokeCwd = Just dir })

  -- Determine the path to the packagedb in the store for this ghc version
  storesByGhc <- getDirectoryContents storeRoot
  case filter (prettyShow pv `isInfixOf`) storesByGhc of
    [] -> return [final_package_db]
    storeForGhc:_ -> do
      let storePackageDB = (storeRoot </> storeForGhc </> "package.db")
      return [storePackageDB, final_package_db]



buildCabalLibsSpecific :: String -> Verbosity -> Maybe FilePath -> FilePath -> IO [FilePath]
buildCabalLibsSpecific ver verb mbGhc builddir_rel = do
  let prog_db = userSpecifyPaths [("ghc", path) | Just path <- [mbGhc] ]  defaultProgramDb
  (cabal, _) <- requireProgram verb (simpleProgram "cabal") prog_db
  dir <- canonicalizePath (builddir_rel </> "specific" </> ver)
  cgot <- doesDirectoryExist (dir </> "Cabal-" ++ ver)
  unless cgot $
    runProgramInvocation verb ((programInvocation cabal ["get", "Cabal-" ++ ver]) { progInvokeCwd = Just dir })
  csgot <- doesDirectoryExist (dir </> "Cabal-syntax-" ++ ver)
  unless csgot $
    runProgramInvocation verb ((programInvocation cabal ["get", "Cabal-syntax-" ++ ver]) { progInvokeCwd = Just dir })
  let hooksVerFromVer _ = "0.1"
      hooksVer = hooksVerFromVer ver
  chgot <- doesDirectoryExist (dir </> "Cabal-hooks-" ++ hooksVer)
  unless chgot $
    runProgramInvocation verb ((programInvocation cabal ["get", "Cabal-hooks-" ++ hooksVer]) { progInvokeCwd = Just dir })
  buildCabalLibsProject ("packages: Cabal-" ++ ver ++ " Cabal-syntax-" ++ ver ++ " Cabal-hooks-" ++ hooksVer) verb mbGhc dir


buildCabalLibsIntree :: String -> Verbosity -> Maybe FilePath -> FilePath -> IO [FilePath]
buildCabalLibsIntree root verb mbGhc builddir_rel = do
  dir <- canonicalizePath (builddir_rel </> "intree")
  buildCabalLibsProject ("packages: " ++ root </> "Cabal" ++ " " ++ root </> "Cabal-syntax" ++ " " ++ root </> "Cabal-hooks") verb mbGhc dir

main :: IO ()
main = do
    -- By default, stderr is not buffered.  This isn't really necessary
    -- for us, and it causes problems on Windows, see:
    -- https://github.com/appveyor/ci/issues/1364
    hSetBuffering stderr LineBuffering

    -- Parse arguments.  N.B. 'helper' adds the option `--help`.
    args <- execParser $ info (mainArgParser <**> helper) mempty
    let verbosity = if mainArgVerbose args then verbose else normal

    pkg_dbs <-
      -- Not path to cabal-install so we're not going to run cabal-install tests so we
      -- can skip setting up a Cabal library to use with cabal-install.
      case argCabalInstallPath (mainCommonArgs args) of
        Nothing -> do
          when (isJust $ mainArgCabalSpec args)
               (putStrLn "Ignoring Cabal library specification as cabal-install tests are not running")
          return []
        -- Path to cabal-install is passed, so need to install the requested relevant version of Cabal
        -- library.
        Just {} ->
          case mainArgCabalSpec args of
            Nothing -> do
              putStrLn "No Cabal library specified, using boot Cabal library with cabal-install tests"
              return []
            Just BootCabalLib -> return []
            Just (InTreeCabalLib root build_dir) ->
              buildCabalLibsIntree root verbosity (argGhcPath (mainCommonArgs args)) build_dir
            Just (SpecificCabalLib ver build_dir) ->
              buildCabalLibsSpecific ver verbosity (argGhcPath (mainCommonArgs args)) build_dir

    -- To run our test scripts, we need to be able to run Haskell code
    -- linked against the Cabal library under test.  The most efficient
    -- way to get this information is by querying the *host* build
    -- system about the information.
    --
    -- Fortunately, because we are using a Custom setup, our Setup
    -- script is bootstrapped against the Cabal library we're testing
    -- against, so can use our dependency on Cabal to read out the build
    -- info *for this package*.
    --
    -- NB: Currently assumes that per-component build is NOT turned on
    -- for Custom.
    dist_dir <- case mainArgDistDir args of
                  Just dist_dir -> return dist_dir
                  Nothing -> getSymbolicPath <$> guessDistDir
    when (verbosity >= verbose) $
        hPutStrLn stderr $ "Using dist dir: " ++ dist_dir
    -- Get ready to go!
    senv <- mkScriptEnv verbosity

    let runTest :: (Maybe cwd -> [unusedEnv] -> FilePath -> [String] -> IO result)
                -> FilePath
                -> IO result
        runTest runner path
            = runner Nothing [] path $
                ["--builddir", dist_dir, path] ++ ["--extra-package-db=" ++ pkg_db | pkg_db <- pkg_dbs] ++ renderCommonArgs (mainCommonArgs args)

    case mainArgTestPaths args of
        [path] -> do
            -- Simple runner
            (real_path, real_args) <- runTest (runnerCommand senv) path
            hPutStrLn stderr $ showCommandForUser real_path real_args
            callProcess real_path real_args
            hPutStrLn stderr "OK"
        user_paths -> do
            -- Read out tests from filesystem
            hPutStrLn stderr $ "threads: " ++ show (mainArgThreads args)

            test_scripts <- if null user_paths
                                then findTests
                                else return user_paths
            -- NB: getDirectoryContentsRecursive is lazy IO, but it
            -- doesn't handle directories disappearing gracefully. Fix
            -- this!
            (single_tests, multi_tests) <- evaluate (partitionTests test_scripts)
            let all_tests = multi_tests ++ single_tests
                margin = maximum (map length all_tests) + 2
            hPutStrLn stderr $ "tests to run: " ++ show (length all_tests)

            -- TODO: Get parallelization out of multitests by querying
            -- them for their modes and then making a separate worker
            -- for each.  But for now, just run them earlier to avoid
            -- them straggling at the end
            work_queue <- newMVar all_tests
            unexpected_fails_var  <- newMVar []
            unexpected_passes_var <- newMVar []
            skipped_var <- newMVar []

            chan <- newChan
            let logAll msg = writeChan chan (ServerLogMsg AllServers msg)
                logEnd = writeChan chan ServerLogEnd
            -- NB: don't use withAsync as we do NOT want to cancel this
            -- on an exception
            async_logger <- async (withFile "cabal-tests.log" WriteMode $ outputThread verbosity chan)

            -- Make sure we pump out all the logs before quitting
            (\m -> finally m (logEnd >> wait async_logger)) $ do

            -- NB: Need to use withAsync so that if the main thread dies
            -- (due to ctrl-c) we tear down all of the worker threads.
            let go server = do
                    let split [] = return ([], Nothing)
                        split (y:ys) = return (ys, Just y)
                        logMeta msg = writeChan chan
                                    $ ServerLogMsg
                                        (ServerMeta (serverProcessId server))
                                        msg
                    mb_work <- modifyMVar work_queue split
                    case mb_work of
                        Nothing -> return ()
                        Just path -> do
                            when (verbosity >= verbose) $
                                logMeta $ "Running " ++ path
                            start <- getTime
                            r <- runTest (runOnServer server) path
                            end <- getTime
                            let time = end - start
                                code = serverResultTestCode r

                            unless (mainArgHideSuccesses args && code == TestCodeOk) $ do
                                logMeta $
                                    path ++ replicate (margin - length path) ' ' ++ displayTestCode code ++
                                    if time >= 0.01
                                        then printf " (%.2fs)" time
                                        else ""

                            when (code == TestCodeFail) $ do
                                let description
                                      | mainArgQuiet args = serverResultStderr r
                                      | otherwise =
                                       "$ " ++ serverResultCommand r ++ "\n" ++
                                       "stdout:\n" ++ serverResultStdout r ++ "\n" ++
                                       "stderr:\n" ++ serverResultStderr r ++ "\n"
                                logMeta $
                                          description
                                       ++ "*** unexpected failure for " ++ path ++ "\n\n"
                                modifyMVar_ unexpected_fails_var $ \paths ->
                                    return (path:paths)

                            when (code == TestCodeUnexpectedOk) $
                                modifyMVar_ unexpected_passes_var $ \paths ->
                                    return (path:paths)

                            when (isTestCodeSkip code) $
                                modifyMVar_ skipped_var $ \paths ->
                                    return (path:paths)

                            go server

            -- Start as many threads as requested by -j to spawn
            -- GHCi servers and start running tests off of the
            -- run queue.
            replicateConcurrently_ (mainArgThreads args) (withNewServer chan senv go)

            unexpected_fails  <- takeMVar unexpected_fails_var
            unexpected_passes <- takeMVar unexpected_passes_var
            skipped           <- takeMVar skipped_var

            -- print summary
            let sl = show . length
                testSummary =
                  sl all_tests ++ " tests, " ++ sl skipped ++ " skipped, "
                    ++ sl unexpected_passes ++ " unexpected passes, "
                    ++ sl unexpected_fails ++ " unexpected fails."
            logAll testSummary

            -- print failed or unexpected ok
            if null (unexpected_fails ++ unexpected_passes)
            then logAll "OK"
            else do
                unless (null unexpected_passes) . logAll $
                    "UNEXPECTED OK: " ++ intercalate " " unexpected_passes
                unless (null unexpected_fails) . logAll $
                    "UNEXPECTED FAIL: " ++ intercalate " " unexpected_fails
                exitFailure

findTests :: IO [FilePath]
findTests = getDirectoryContentsRecursive "."

partitionTests :: [FilePath] -> ([FilePath], [FilePath])
partitionTests = go [] []
  where
    go ts ms [] = (ts, ms)
    go ts ms (f:fs) =
        -- NB: Keep this synchronized with isTestFile
        case takeExtensions f of
            ".test.hs"      -> go (f:ts) ms fs
            ".multitest.hs" -> go ts (f:ms) fs
            _               -> go ts ms     fs

outputThread :: Verbosity -> Chan ServerLogMsg -> Handle -> IO ()
outputThread verbosity chan log_handle = go ""
  where
    go prev_hdr = do
        v <- readChan chan
        case v of
            ServerLogEnd -> return ()
            ServerLogMsg t msg -> do
                let ls = lines msg
                    pre s c
                        | verbosity >= verbose
                        -- Didn't use printf as GHC 7.4
                        -- doesn't understand % 7s.
                        = replicate (7 - length s) ' ' ++ s ++ " " ++ c : " "
                        | otherwise = ""
                    hdr = case t of
                            AllServers   -> ""
                            ServerMeta s -> pre s ' '
                            ServerIn   s -> pre s '<'
                            ServerOut  s -> pre s '>'
                            ServerErr  s -> pre s '!'
                    ws = replicate (length hdr) ' '
                    mb_hdr l | hdr == prev_hdr = ws ++ l
                             | otherwise = hdr ++ l
                    ls' = case ls of
                            [] -> []
                            r:rs ->
                                mb_hdr r : map (ws ++) rs
                    logmsg = unlines ls'
                hPutStr stderr logmsg
                hPutStr log_handle logmsg
                go hdr

-- Cribbed from tasty
type Time = Double

getTime :: IO Time
getTime = do
    t <- Clock.getTime Clock.Monotonic
    let ns = realToFrac $ Clock.toNanoSecs t
    return $ ns / 10 ^ (9 :: Int)
