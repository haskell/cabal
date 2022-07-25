{-# LANGUAGE CPP #-}
module Distribution.Compat.Process (
    -- * Redefined functions
    createProcess,
    runInteractiveProcess,
    rawSystem,
    -- * Additions
    enableProcessJobs,
    ) where

import System.Exit (ExitCode (..))
import System.IO   (Handle)

import           System.Process (CreateProcess, ProcessHandle, waitForProcess)
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

-- | 'System.Process.createProcess' with process jobs enabled when appropriate.
-- See 'enableProcessJobs'.
createProcess :: CreateProcess
              -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess = Process.createProcess . enableProcessJobs

-- | 'System.Process.rawSystem' with process jobs enabled when appropriate.
-- See 'enableProcessJobs'.
rawSystem :: String -> [String] -> IO ExitCode
rawSystem cmd args = do
  (_,_,_,p) <- createProcess (Process.proc cmd args) { Process.delegate_ctlc = True }
  waitForProcess p

-- | 'System.Process.runInteractiveProcess' with process jobs enabled when
-- appropriate. See 'enableProcessJobs'.
runInteractiveProcess
  :: FilePath                   -- ^ Filename of the executable (see 'RawCommand' for details)
  -> [String]                   -- ^ Arguments to pass to the executable
  -> Maybe FilePath             -- ^ Optional path to the working directory
  -> Maybe [(String,String)]    -- ^ Optional environment (otherwise inherit)
  -> IO (Handle,Handle,Handle,ProcessHandle)
runInteractiveProcess cmd args mb_cwd mb_env = do
  (mb_in, mb_out, mb_err, p) <-
      createProcess (Process.proc cmd args)
              { Process.std_in  = Process.CreatePipe,
                Process.std_out = Process.CreatePipe,
                Process.std_err = Process.CreatePipe,
                Process.env     = mb_env,
                Process.cwd     = mb_cwd }
  return (fromJust mb_in, fromJust mb_out, fromJust mb_err, p)
  where
    fromJust = maybe (error "runInteractiveProcess: fromJust") id
