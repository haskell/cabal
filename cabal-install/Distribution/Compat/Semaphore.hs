{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Distribution.Compat.Semaphore (
    QSem, newQSem, waitQSem, signalQSem
  ) where

import Control.Concurrent hiding (QSem, newQSem, waitQSem, signalQSem)
import Control.Concurrent.STM
import Control.Monad
import Data.Typeable
import Control.Exception

data QSem = QSem !(TVar Int) !(TVar [TVar Bool]) !(TVar [TVar Bool])
  deriving (Eq, Typeable)

newQSem :: Int -> IO QSem
newQSem i = atomically $ do
  q <- newTVar i
  b1 <- newTVar []
  b2 <- newTVar []
  return (QSem q b1 b2)

waitQSem :: QSem -> IO ()
waitQSem (QSem q b1 b2) =
  join $ atomically $ do
     v <- readTVar q
     if v == 0
        then do b <- newTVar False
                ys <- readTVar b2
                writeTVar b2 (b:ys)
                return (wait b)
        else do writeTVar q $! v - 1
                return (return ())

wait t = atomically $ do
  v <- readTVar t
  when (not v) retry

wake t = atomically $ writeTVar t True

signalQSem :: QSem -> IO ()
signalQSem (QSem q b1 b2) = mask_ $ join $ atomically $ do
         -- join, so we don't force the reverse inside the txn
         -- mask_ is needed so we don't lose a wakeup
  v <- readTVar q
  if v /= 0
     then do writeTVar q $! v + 1
             return (return ())
     else do
        xs <- readTVar b1
        case xs of
            [] -> do ys <- readTVar b2
                     case ys of
                       [] -> do writeTVar q 1
                                return (return ())
                       _  -> do let (z:zs) = reverse ys
                                writeTVar b1 zs
                                writeTVar b2 []
                                return (wake z)
            (b:xs') -> do writeTVar b1 xs'
                          return (wake b)
