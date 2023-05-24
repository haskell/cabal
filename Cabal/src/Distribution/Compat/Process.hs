{-# LANGUAGE CPP #-}

module Distribution.Compat.Process
  ( -- * Redefined functions
    proc

    -- * Additions
  , enableProcessJobs
  ) where

import System.Process (CreateProcess)
import qualified System.Process as Process

#if defined(mingw32_HOST_OS) && MIN_VERSION_process(1,6,9)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Win32.Info.Version (dwMajorVersion, dwMinorVersion, getVersionEx)
#endif

-------------------------------------------------------------------------------
-- enableProcessJobs
-------------------------------------------------------------------------------

#if defined(mingw32_HOST_OS) && MIN_VERSION_process(1,6,9)
-- This exception, needed to support Windows 7, could be removed when
-- the lowest GHC version cabal supports is a GHC that doesn’t support
-- Windows 7 any more.
{-# NOINLINE isWindows8OrLater #-}
isWindows8OrLater :: Bool
isWindows8OrLater = unsafePerformIO $ do
  v <- getVersionEx
  pure $ (dwMajorVersion v, dwMinorVersion v) >= (6, 2)
#endif

-- | Enable process jobs to ensure accurate determination of process completion
-- in the presence of @exec(3)@ on Windows.
--
-- Unfortunately the process job support is badly broken in @process@ releases
-- prior to 1.6.9, so we disable it in these versions, despite the fact that
-- this means we may see sporadic build failures without jobs.
--
-- On Windows 7 or before the jobs are disabled due to the fact that
-- processes on these systems can only have one job. This prevents
-- spawned process from assigning jobs to its own children. Suppose
-- process A spawns process B. The B process has a job assigned (call
-- it J1) and when it tries to spawn a new process C the C
-- automatically inherits the job. But at it also tries to assign a
-- new job J2 to C since it doesn’t have access J1. This fails on
-- Windows 7 or before.
enableProcessJobs :: CreateProcess -> CreateProcess
#if defined(mingw32_HOST_OS) && MIN_VERSION_process(1,6,9)
enableProcessJobs cp = cp {Process.use_process_jobs = isWindows8OrLater}
#else
enableProcessJobs cp = cp
#endif

-------------------------------------------------------------------------------
-- process redefinitions
-------------------------------------------------------------------------------

-- | 'System.Process.proc' with process jobs enabled when appropriate,
-- and defaulting 'delegate_ctlc' to 'True'.
proc :: FilePath -> [String] -> CreateProcess
proc path args = enableProcessJobs (Process.proc path args){Process.delegate_ctlc = True}
