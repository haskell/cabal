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

import           System.Process (CreateProcess, ProcessHandle)
import qualified System.Process as Process

#if MIN_VERSION_process(1,2,0)
import           System.Process (waitForProcess)
#endif

-------------------------------------------------------------------------------
-- enableProcessJobs
-------------------------------------------------------------------------------

-- | Enable process jobs to ensure accurate determination of process completion
-- in the presence of @exec(3)@ on Windows.
--
-- Unfortunately the process job support is badly broken in @process@ releases
-- prior to 1.6.9, so we disable it in these versions, despite the fact that
-- this means we may see sporatic build failures without jobs.
enableProcessJobs :: CreateProcess -> CreateProcess
#ifdef MIN_VERSION_process
#if MIN_VERSION_process(1,6,9)
enableProcessJobs cp = cp {Process.use_process_jobs = True}
#else
enableProcessJobs cp = cp
#endif
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
#if MIN_VERSION_process(1,2,0)
  (_,_,_,p) <- createProcess (Process.proc cmd args) { Process.delegate_ctlc = True }
  waitForProcess p
#else
  -- With very old 'process', just do its rawSystem
  Process.rawSystem cmd args
#endif

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
