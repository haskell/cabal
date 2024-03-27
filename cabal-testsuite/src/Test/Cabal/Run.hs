{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NondecreasingIndentation #-}
-- | A module for running commands in a chatty way.
module Test.Cabal.Run (
    run,
    runAction,
    Result(..)
) where

import Distribution.Simple.Program.Run
import Distribution.Verbosity

import Control.Concurrent.Async
import System.Process
import System.IO
import System.Exit
import System.Directory
import System.FilePath

-- | The result of invoking the command line.
data Result = Result
    { resultExitCode    :: ExitCode
    , resultCommand     :: String
    , resultOutput      :: String
    } deriving Show

-- | Run a command, streaming its output to stdout, and return a 'Result'
-- with this information.
run :: Verbosity -> Maybe FilePath -> [(String, Maybe String)] -> FilePath -> [String]
    -> Maybe String -> IO Result
run verbosity mb_cwd env_overrides path0 args input =
    runAction verbosity mb_cwd env_overrides path0 args input (\_ -> return ())

runAction :: Verbosity -> Maybe FilePath -> [(String, Maybe String)] -> FilePath -> [String]
    -> Maybe String -> (ProcessHandle -> IO ()) -> IO Result
runAction _verbosity mb_cwd env_overrides path0 args input action = do
    -- In our test runner, we allow a path to be relative to the
    -- current directory using the same heuristic as shells:
    -- 'foo' refers to an executable in the PATH, but './foo'
    -- and 'foo/bar' refer to relative files.
    --
    -- Unfortunately, we cannot just pass these relative paths directly:
    -- 'runProcess' resolves an executable path not with respect to the
    -- current working directory, but the working directory that the
    -- subprocess will execute in.  Thus, IF we have a relative
    -- path which is not a bare executable name, we have to tack on
    -- the CWD to make it resolve correctly
    cwdir <- getCurrentDirectory
    let path | length (splitPath path0) /= 1 && isRelative path0
             = cwdir </> path0
             | otherwise
             = path0

    mb_env <- getEffectiveEnvironment env_overrides
    putStrLn $ "+ " ++ showCommandForUser path args
    (readh, writeh) <- createPipe
    hSetBuffering readh LineBuffering
    hSetBuffering writeh LineBuffering
    let drain = do
            r <- hGetContents readh
            putStr r -- forces the output
            hClose readh
            return r
    withAsync drain $ \sync -> do

    let prc = (proc path args)
          { cwd = mb_cwd
          , env = mb_env
          , std_in = case input of { Just _ -> CreatePipe; Nothing -> Inherit }
          , std_out = UseHandle writeh
          , std_err = UseHandle writeh
          }

    withCreateProcess prc $ \stdin_h _ _ procHandle -> do

    case input of
      Just x ->
        case stdin_h of
          Just h -> hPutStr h x >> hClose h
          Nothing -> error "No stdin handle when input was specified!"
      Nothing -> return ()

    action procHandle

    -- wait for the program to terminate
    exitcode <- waitForProcess procHandle
    out <- wait sync

    return Result {
            resultExitCode = exitcode,
            resultCommand = showCommandForUser path args,
            resultOutput = out
        }
