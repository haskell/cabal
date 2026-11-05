{-# LANGUAGE CPP #-}

module UnitTests.Distribution.Compat.CopyFile (tests) where

import Test.Tasty

#ifdef mingw32_HOST_OS
tests :: [TestTree]
tests = []
#else
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)

import Distribution.Simple.Utils (installExecutableFile, withTempDirectory)
import Distribution.Verbosity (defaultVerbosityHandles, mkVerbosity, silent)

import System.Directory (createDirectory, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hClose, hPutStr)
import System.Posix.Files (createNamedPipe, ownerModes)
import System.Posix.IO (OpenFileFlags (cloexec), OpenMode (ReadWrite), defaultFileFlags, fdToHandle, openFd)
import System.Process (createProcess, proc, rawSystem, terminateProcess, waitForProcess)

import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase
      "a process started during a copy does not stop the copy from running"
      runnableDespiteConcurrentProcess
  ]

-- | A process started while an executable is being copied must not inherit the
-- file being written: the executable could not be run until that process exits.
--
-- The source is a FIFO, which holds the copy open for as long as we like, so
-- the test does not depend on timing.
runnableDespiteConcurrentProcess :: Assertion
runnableDespiteConcurrentProcess =
  withTempDirectory "." "copyfile-" $ \dir -> do
    let src = dir </> "src"
        binDir = dir </> "bin"
        dest = binDir </> "tool"
    createNamedPipe src ownerModes
    createDirectory binDir
    -- Read-write: opening does not wait for a reader, and the reader sees end
    -- of file only when we close. Close-on-exec: the process below must not
    -- keep the FIFO open, or the copy would not finish until that process exits.
    fifo <- openFd src ReadWrite defaultFileFlags{cloexec = True} >>= fdToHandle
    copied <- newEmptyMVar
    _ <- forkIO $ do
      installExecutableFile (mkVerbosity defaultVerbosityHandles silent) src dest
      putMVar copied ()
    waitForFileIn binDir (100 :: Int)
    bracket
      (createProcess (proc "sleep" ["60"]))
      (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
      $ \_ -> do
        hPutStr fifo "#!/bin/sh\nexit 0\n"
        hClose fifo
        takeMVar copied
        rawSystem dest [] >>= (@?= ExitSuccess)
  where
    waitForFileIn _ 0 = assertFailure "copy did not start"
    waitForFileIn binDir n = do
      files <- listDirectory binDir
      when (null files) $ threadDelay 50000 >> waitForFileIn binDir (n - 1)
#endif
