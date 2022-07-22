{-# LANGUAGE CPP #-}
module Distribution.Compat.Process (
    -- * Redefined functions
    proc
    ) where

import           System.Process (CreateProcess)
import qualified System.Process as Process

-------------------------------------------------------------------------------
-- process redefinitions
-------------------------------------------------------------------------------

-- | 'System.Process.proc' defaulting 'delegate_ctlc' to 'True'.
proc :: FilePath -> [String] -> CreateProcess
proc path args = (Process.proc path args) { Process.delegate_ctlc = True }
