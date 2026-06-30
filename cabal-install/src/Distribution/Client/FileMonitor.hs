-- | An abstraction to help with re-running actions when files or other
-- input values they depend on have changed.
module Distribution.Client.FileMonitor
  ( -- * Declaring files to monitor
    module Distribution.Simple.FileMonitor.Types

    -- * Creating and checking sets of monitored files
  , FileMonitor (..)
  , newFileMonitor
  , MonitorChanged (..)
  , MonitorChangedReason (..)
  , checkFileMonitorChanged
  , updateFileMonitor
  , MonitorTimestamp
  , beginUpdateFileMonitor

    -- * Internal
  , MonitorStateFileSet
  , MonitorStateFile
  , MonitorStateGlob
  ) where

import Distribution.Simple.FileMonitor
import Distribution.Simple.FileMonitor.Types
