{-# LANGUAGE CPP #-}

module Distribution.Client.Signal
  ( installTerminationHandler
  , Terminated (..)
  )
where

import qualified Control.Exception as Exception

#ifndef mingw32_HOST_OS
import Control.Concurrent (myThreadId)
import Control.Monad (void)
import qualified System.Posix.Signals as Signals
#endif

-- | Terminated is an asynchronous exception, thrown when
-- SIGTERM is received. It's to 'kill' what 'UserInterrupt'
-- is to Ctrl-C.
data Terminated = Terminated

instance Exception.Exception Terminated where
  toException = Exception.asyncExceptionToException
  fromException = Exception.asyncExceptionFromException

instance Show Terminated where
  show Terminated = "terminated"

-- | Install a signal handler that initiates a controlled shutdown on receiving
-- SIGTERM by throwing an asynchronous exception at the main thread. Must be
-- called from the main thread.
--
-- It is a noop on Windows.
installTerminationHandler :: IO ()

#ifdef mingw32_HOST_OS

installTerminationHandler = return ()

#else

installTerminationHandler = do
  mainThreadId <- myThreadId
  void $ Signals.installHandler
    Signals.sigTERM
    (Signals.CatchOnce $ Exception.throwTo mainThreadId Terminated)
    Nothing

#endif
