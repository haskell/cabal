-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.JobControl
-- Copyright   :  (c) Duncan Coutts 2012
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A job control concurrency abstraction
-----------------------------------------------------------------------------
module Distribution.Client.JobControl (
    JobControl,
    newSerialJobControl,
    newParallelJobControl,
    spawnJob,
    collectJob,

    JobLimit,
    newJobLimit,
    withJobLimit,

    Lock,
    newLock,
    criticalSection
  ) where

import Control.Monad
import Control.Concurrent hiding (QSem, newQSem, waitQSem, signalQSem)
import Control.Exception (SomeException, bracket_, mask, throw, try)
import Distribution.Client.Compat.Semaphore

data JobControl m a = JobControl {
       spawnJob    :: m a -> m (),
       collectJob  :: m a
     }


newSerialJobControl :: IO (JobControl IO a)
newSerialJobControl = do
    queue <- newChan
    return JobControl {
      spawnJob   = spawn queue,
      collectJob = collect queue
    }
  where
    spawn :: Chan (IO a) -> IO a -> IO ()
    spawn = writeChan

    collect :: Chan (IO a) -> IO a
    collect = join . readChan

newParallelJobControl :: IO (JobControl IO a)
newParallelJobControl = do
    resultVar <- newEmptyMVar
    return JobControl {
      spawnJob   = spawn resultVar,
      collectJob = collect resultVar
    }
  where
    spawn :: MVar (Either SomeException a) -> IO a -> IO ()
    spawn resultVar job =
      mask $ \restore ->
        forkIO (do res <- try (restore job)
                   putMVar resultVar res)
         >> return ()

    collect :: MVar (Either SomeException a) -> IO a
    collect resultVar =
      takeMVar resultVar >>= either throw return

data JobLimit = JobLimit QSem

newJobLimit :: Int -> IO JobLimit
newJobLimit n =
  fmap JobLimit (newQSem n)

withJobLimit :: JobLimit -> IO a -> IO a
withJobLimit (JobLimit sem) =
  bracket_ (waitQSem sem) (signalQSem sem)

newtype Lock = Lock (MVar ())

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

criticalSection :: Lock -> IO a -> IO a
criticalSection (Lock lck) act = bracket_ (takeMVar lck) (putMVar lck ()) act
