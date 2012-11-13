{-# LANGUAGE CPP #-}
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
import Control.Concurrent
import Control.Exception

import qualified Distribution.Compat.MSem as MSem

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
#if MIN_VERSION_base(4,3,0)
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
#else
newParallelJobControl = newSerialJobControl
#endif

data JobLimit = JobLimit (MSem.MSem Int)

newJobLimit :: Int -> IO JobLimit
newJobLimit n
  | n < 1     = error "Distribution.Client.JobControl.newJobLimit: n < 1"
  | otherwise = fmap JobLimit (MSem.new n)

withJobLimit :: JobLimit -> IO a -> IO a
withJobLimit (JobLimit sem) =
  bracket_ (MSem.wait sem) (MSem.signal sem)

newtype Lock = Lock (MVar ())

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

criticalSection :: Lock -> IO a -> IO a
criticalSection (Lock lck) act = bracket_ (takeMVar lck) (putMVar lck ()) act
