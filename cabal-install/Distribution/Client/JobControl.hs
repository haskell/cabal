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
import Control.Exception (SomeException, throw)
import Distribution.Client.Compat.Semaphore
import Distribution.Client.Shell ( Shell, liftIO, forkIO' )
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class ( MonadIO(..) )

data JobControl m a = JobControl {
       spawnJob    :: m a -> m (),
       collectJob  :: m a
     }

newSerialJobControl :: MonadIO m => IO (JobControl m a)
newSerialJobControl = do
    queue <- newChan
    return JobControl {
      spawnJob   = liftIO . spawn queue,
      collectJob = collect queue
    }
  where
    spawn :: Chan (m a) -> m a -> IO ()
    spawn = writeChan

    collect :: MonadIO m => Chan (m a) -> m a
    collect = join . liftIO . readChan

newParallelJobControl :: IO (JobControl Shell a)
newParallelJobControl = do
    resultVar <- newEmptyMVar :: IO (MVar (Either SomeException a))
    return JobControl {
      spawnJob   = spawn resultVar,
      collectJob = collect resultVar
    }
  where
    spawn :: MVar (Either SomeException a) -> Shell a -> Shell ()
    spawn resultVar job =
      MC.mask $ \restore ->
        forkIO' (do res <- MC.try (restore job)
                    liftIO (putMVar resultVar res))
        >> return ()

    collect :: MVar (Either SomeException a) -> Shell a
    collect resultVar =
      liftIO (takeMVar resultVar) >>= either throw return

data JobLimit = JobLimit QSem

newJobLimit :: Int -> IO JobLimit
newJobLimit n =
  fmap JobLimit (newQSem n)

withJobLimit :: (MC.MonadMask m, MonadIO m) => JobLimit -> m a -> m a
withJobLimit (JobLimit sem) =
  MC.bracket_ (liftIO (waitQSem sem)) (liftIO (signalQSem sem))

newtype Lock = Lock (MVar ())

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

criticalSection :: (MC.MonadMask m, MonadIO m) => Lock -> m a -> m a
criticalSection (Lock lck) act = MC.bracket_ (liftIO (takeMVar lck)) (liftIO (putMVar lck ())) act
