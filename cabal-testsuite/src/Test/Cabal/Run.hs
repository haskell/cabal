{-# LANGUAGE NondecreasingIndentation #-}
-- | A module for running commands in a chatty way.
module Test.Cabal.Run (
    run,
    Result(..)
) where

import qualified Distribution.Compat.CreatePipe as Compat
import Distribution.Simple.Program.Run
import Distribution.Verbosity

import Control.Concurrent.Async
import System.Process
import System.IO
import System.Exit
import System.Directory
import System.FilePath
import Data.Foldable (traverse_)

-- | The result of invoking the command line.
data Result = Result
    { resultExitCode    :: ExitCode
    , resultCommand     :: String
    -- | Output sent to any file descriptor.
    , resultOutput      :: String
    -- | Output sent to stdout.
    , resultStdout      :: String
    -- | Output sent to stderr.
    , resultStderr      :: String
    } deriving Show

-- | Run a command, streaming its output to stdout, and return a 'Result'
-- with this information.
run :: Verbosity -> Maybe FilePath -> [(String, Maybe String)] -> FilePath -> [String] -> Maybe String -> IO Result
run _verbosity mb_cwd env_overrides path0 args input = do
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
    (readstdout, writestdout) <- Compat.createPipe
    (readstderr, writestderr) <- Compat.createPipe
    (readall, writeall) <- Compat.createPipe
    traverse_ (`hSetBuffering` LineBuffering) [ stdout, readstdout, writestdout, readstderr, writestderr, readall, writeall ]
    let mkDrain h = do
            r <- hGetContents' h
            hPutStr writeall r
            return r
    withAsync (mkDrain readstdout) $ \syncstdout -> do
    withAsync (mkDrain readstderr) $ \syncstderr -> do

    let prc = (proc path args)
          { cwd = mb_cwd
          , env = mb_env
          , std_in = case input of { Just _ -> CreatePipe; Nothing -> Inherit }
          , std_out = UseHandle writestdout
          , std_err = UseHandle writestderr
          }
    (stdin_h, _, _, procHandle) <- createProcess prc

    case input of
      Just x ->
        case stdin_h of
          Just h -> hPutStr h x >> hClose h
          Nothing -> error "No stdin handle when input was specified!"
      Nothing -> return ()

    -- wait for the program to terminate
    exitcode <- waitForProcess procHandle
    rStdout <- wait syncstdout
    rStderr <- wait syncstderr
    hClose writeall

    rAll <- hGetContents' readall

    return Result {
            resultExitCode = exitcode,
            resultCommand = showCommandForUser path args,
            resultOutput = rAll,
            resultStdout = rStdout,
            resultStderr = rStderr
        }

-- `hGetContents'` is in since base-4.15.0.0 -- which we don't have.
hGetContents' :: Handle -> IO String
hGetContents' h = do
  v <- hGetContents h
  length v `seq` hClose h
  pure v
