{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- You can set the following VERBOSE environment variable to control
-- the verbosity of the output generated by this module.
module PackageTests.PackageTester
    ( PackageSpec(..)
    , Success(..)
    , Result(..)
    , TestsConfig(..)

    -- * Running cabal commands
    , cabal_configure
    , cabal_build
    , cabal_haddock
    , cabal_test
    , cabal_bench
    , cabal_install
    , unregister
    , compileSetup
    , run

    -- * Test helpers
    , assertConfigureSucceeded
    , assertBuildSucceeded
    , assertBuildFailed
    , assertHaddockSucceeded
    , assertTestSucceeded
    , assertInstallSucceeded
    , assertOutputContains
    , assertOutputDoesNotContain
    ) where

import qualified Control.Exception.Extensible as E
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath
import System.IO (hIsEOF, hGetChar, hClose)
import System.IO.Error (isDoesNotExistError)
import System.Process (runProcess, waitForProcess)
import Test.Tasty.HUnit (Assertion, assertFailure)

import Distribution.Compat.CreatePipe (createPipe)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.Program.Run (getEffectiveEnvironment)
import Distribution.Simple.Utils (printRawCommandAndArgsAndEnv)
import Distribution.ReadE (readEOrFail)
import Distribution.Verbosity (Verbosity, flagToVerbosity, normal)

data PackageSpec = PackageSpec
    { directory  :: FilePath
    , distPref :: Maybe FilePath
    , configOpts :: [String]
    }

data TestsConfig = TestsConfig {
    testsConfigGhcPath     :: FilePath
  , testsConfigGhcPkgPath  :: FilePath
  , testsConfigInPlaceSpec :: PackageSpec
  , testsConfigBuildDir    :: FilePath    -- ^ @--builddir@ argument
  }

data Success = Failure
             | ConfigureSuccess
             | BuildSuccess
             | HaddockSuccess
             | InstallSuccess
             | TestSuccess
             | BenchSuccess
             deriving (Eq, Show)

data Result = Result
    { successful :: Bool
    , success    :: Success
    , outputText :: String
    } deriving Show

nullResult :: Result
nullResult = Result True Failure ""

------------------------------------------------------------------------
-- * Running cabal commands

recordRun :: (String, ExitCode, String) -> Success -> Result -> Result
recordRun (cmd, exitCode, exeOutput) thisSucc res =
    res { successful = successful res && exitCode == ExitSuccess
        , success    = if exitCode == ExitSuccess then thisSucc
                       else success res
        , outputText =
            (if null $ outputText res then "" else outputText res ++ "\n") ++
            cmd ++ "\n" ++ exeOutput
        }

cabal_configure :: IO TestsConfig -> PackageSpec -> IO Result
cabal_configure cfg spec = do
    res <- doCabalConfigure cfg spec
    record spec res
    return res

doCabalConfigure :: IO TestsConfig -> PackageSpec -> IO Result
doCabalConfigure cfg spec = do
    TestsConfig{..} <- cfg
    cleanResult@(_, _, _) <- cabal cfg spec [] ["clean"]
    requireSuccess cleanResult
    res <- cabal cfg spec []
           (["configure", "--user", "-w", testsConfigGhcPath] ++ configOpts spec)
    return $ recordRun res ConfigureSuccess nullResult

doCabalBuild :: IO TestsConfig -> PackageSpec -> IO Result
doCabalBuild cfg spec = do
    configResult <- doCabalConfigure cfg spec
    if successful configResult
        then do
            res <- cabal cfg spec [] ["build", "-v"]
            return $ recordRun res BuildSuccess configResult
        else
            return configResult

cabal_build :: IO TestsConfig -> PackageSpec -> IO Result
cabal_build cfg spec = do
    res <- doCabalBuild cfg spec
    record spec res
    return res

cabal_haddock :: IO TestsConfig -> PackageSpec -> [String] -> IO Result
cabal_haddock cfg spec extraArgs = do
    res <- doCabalHaddock cfg spec extraArgs
    record spec res
    return res

doCabalHaddock :: IO TestsConfig -> PackageSpec -> [String] -> IO Result
doCabalHaddock cfg spec extraArgs = do
    configResult <- doCabalConfigure cfg spec
    if successful configResult
        then do
            res <- cabal cfg spec [] ("haddock" : extraArgs)
            return $ recordRun res HaddockSuccess configResult
        else
            return configResult

unregister :: IO TestsConfig -> String -> IO ()
unregister cfg libraryName = do
    TestsConfig{..} <- cfg
    res@(_, _, output) <- run Nothing testsConfigGhcPkgPath [] ["unregister", "--user", libraryName]
    if "cannot find package" `isInfixOf` output
        then return ()
        else requireSuccess res

-- | Install this library in the user area
cabal_install :: IO TestsConfig -> PackageSpec -> IO Result
cabal_install cfg spec = do
    buildResult <- doCabalBuild cfg spec
    res <- if successful buildResult
        then do
            res <- cabal cfg spec [] ["install"]
            return $ recordRun res InstallSuccess buildResult
        else
            return buildResult
    record spec res
    return res

cabal_test :: IO TestsConfig -> PackageSpec -> [(String, Maybe String)] -> [String] -> IO Result
cabal_test cfg spec envOverrides extraArgs = do
    res <- cabal cfg spec envOverrides ("test" : extraArgs)
    let r = recordRun res TestSuccess nullResult
    record spec r
    return r

cabal_bench :: IO TestsConfig -> PackageSpec -> [String] -> IO Result
cabal_bench cfg spec extraArgs = do
    res <- cabal cfg spec [] ("bench" : extraArgs)
    let r = recordRun res BenchSuccess nullResult
    record spec r
    return r

compileSetup :: IO TestsConfig -> FilePath -> IO ()
compileSetup cfg packageDir = do
    TestsConfig{..} <- cfg
    wd <- getCurrentDirectory
    r <- run (Just $ packageDir) testsConfigGhcPath []
         [ "--make"
-- HPC causes trouble -- see #1012
--       , "-fhpc"
         , "-package-conf " ++ wd </> ".." </> testsConfigBuildDir </> "package.conf.inplace"
         , "Setup.hs"
         ]
    requireSuccess r

-- | Returns the command that was issued, the return code, and the output text.
cabal :: IO TestsConfig -> PackageSpec -> [(String, Maybe String)] -> [String] -> IO (String, ExitCode, String)
cabal cfg spec envOverrides cabalArgs_ = do
    let cabalArgs = case distPref spec of
                       Nothing -> cabalArgs_
                       Just dist -> ("--builddir=" ++ dist) : cabalArgs_
    customSetup <- doesFileExist (directory spec </> "Setup.hs")
    if customSetup
        then do
            compileSetup cfg (directory spec)
            path <- canonicalizePath $ directory spec </> "Setup"
            run (Just $ directory spec) path envOverrides cabalArgs
        else do
            -- Use shared Setup executable (only for Simple build types).
            path <- canonicalizePath "Setup"
            run (Just $ directory spec) path envOverrides cabalArgs

-- | Returns the command that was issued, the return code, and the output text
run :: Maybe FilePath -> String -> [(String, Maybe String)] -> [String] -> IO (String, ExitCode, String)
run cwd path envOverrides args = do
    verbosity <- getVerbosity
    -- path is relative to the current directory; canonicalizePath makes it
    -- absolute, so that runProcess will find it even when changing directory.
    path' <- do pathExists <- doesFileExist path
                canonicalizePath (if pathExists then path else path <.> exeExtension)
    menv <- getEffectiveEnvironment envOverrides

    printRawCommandAndArgsAndEnv verbosity path' args menv
    (readh, writeh) <- createPipe
    pid <- runProcess path' args cwd menv Nothing (Just writeh) (Just writeh)

    -- fork off a thread to start consuming the output
    out <- suckH [] readh
    hClose readh

    -- wait for the program to terminate
    exitcode <- waitForProcess pid
    let fullCmd = unwords (path' : args)
    return ("\"" ++ fullCmd ++ "\" in " ++ fromMaybe "" cwd, exitcode, out)
  where
    suckH output h = do
        eof <- hIsEOF h
        if eof
            then return (reverse output)
            else do
                c <- hGetChar h
                suckH (c:output) h


requireSuccess :: (String, ExitCode, String) -> IO ()
requireSuccess (cmd, exitCode, output) =
    unless (exitCode == ExitSuccess) $
        assertFailure $ "Command " ++ cmd ++ " failed.\n" ++
        "output: " ++ output

record :: PackageSpec -> Result -> IO ()
record spec res = do
    C.writeFile (directory spec </> "test-log.txt") (C.pack $ outputText res)

------------------------------------------------------------------------
-- * Test helpers

assertConfigureSucceeded :: Result -> Assertion
assertConfigureSucceeded result = unless (successful result) $
    assertFailure $
    "expected: \'setup configure\' should succeed\n" ++
    "  output: " ++ outputText result

assertBuildSucceeded :: Result -> Assertion
assertBuildSucceeded result = unless (successful result) $
    assertFailure $
    "expected: \'setup build\' should succeed\n" ++
    "  output: " ++ outputText result

assertBuildFailed :: Result -> Assertion
assertBuildFailed result = when (successful result) $
    assertFailure $
    "expected: \'setup build\' should fail\n" ++
    "  output: " ++ outputText result

assertHaddockSucceeded :: Result -> Assertion
assertHaddockSucceeded result = unless (successful result) $
    assertFailure $
    "expected: \'setup haddock\' should succeed\n" ++
    "  output: " ++ outputText result

assertTestSucceeded :: Result -> Assertion
assertTestSucceeded result = unless (successful result) $
    assertFailure $
    "expected: \'setup test\' should succeed\n" ++
    "  output: " ++ outputText result

assertInstallSucceeded :: Result -> Assertion
assertInstallSucceeded result = unless (successful result) $
    assertFailure $
    "expected: \'setup install\' should succeed\n" ++
    "  output: " ++ outputText result

assertOutputContains :: String -> Result -> Assertion
assertOutputContains needle result =
    unless (needle `isInfixOf` (concatOutput output)) $
    assertFailure $
    " expected: " ++ needle ++ "\n" ++
    " in output: " ++ output ++ ""
  where output = outputText result

assertOutputDoesNotContain :: String -> Result -> Assertion
assertOutputDoesNotContain needle result =
    when (needle `isInfixOf` (concatOutput output)) $
    assertFailure $
    "unexpected: " ++ needle ++
    " in output: " ++ output
  where output = outputText result

-- | Replace line breaks with spaces, correctly handling "\r\n".
concatOutput :: String -> String
concatOutput = unwords . lines . filter ((/=) '\r')

------------------------------------------------------------------------
-- Verbosity

lookupEnv :: String -> IO (Maybe String)
lookupEnv name =
    (fmap Just $ getEnv name)
    `E.catch` \ (e :: IOError) ->
        if isDoesNotExistError e
        then return Nothing
        else E.throw e

-- TODO: Convert to a "-v" flag instead.
getVerbosity :: IO Verbosity
getVerbosity = do
    maybe normal (readEOrFail flagToVerbosity) `fmap` lookupEnv "VERBOSE"
