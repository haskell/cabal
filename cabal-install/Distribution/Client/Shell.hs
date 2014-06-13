-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Shell
-- Copyright   :  (c) 2014 Greg Horn
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Wrapper around IO with concurrency and logging, used for
-- nice running summary messages durring parallel builds.
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Distribution.Client.Shell ( Shell
                                 , InstallProgress(..)
                                 , liftIO -- reexport for convenience
                                 , warn', notice', info', debug', die'
                                 , forkIO'
                                 , runShell
                                 , catchIO', catchExit'
                                 ) where

import Distribution.Verbosity ( Verbosity )
import Distribution.Simple.Utils as Utils
         ( notice, info, warn, debug, die )

import System.Exit
import qualified Control.Exception as Exception
import qualified Control.Concurrent as CC
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch ( MonadCatch, MonadThrow, MonadMask )
import qualified Control.Monad.Catch as MC

data Message = Info String
             | Notice String
             | Warn String
             | Debug String
             | Die String

data InstallProgress = InstallProgress
                       -- lines "printed" to terminal
                       [Message]

-- | This is basically the IO monad where you only print to the terminal
-- through special commands (warn', notice', info', etc). This lets us write
-- a nice ncurses UI.
-- .
-- Shell is an instance of MonadIO, though you better not use that to print
-- to terminal. (Maybe this instance should be hidden).
-- .
-- This monad can be forked with forkIO'.
-- .
-- Shell also carries around the state of the parallel build progress.
newtype Shell a = Shell { unShell :: CC.MVar (InstallProgress, Verbosity) -> IO a }

instance Functor Shell where
  fmap f (Shell sh) = Shell (fmap f . sh)

instance Applicative Shell where
  pure x = Shell $ const (pure x)
  Shell f <*> Shell v = Shell $ \r -> f r <*> v r

instance Monad Shell where
  return x = Shell $ \_ -> return x
  Shell m >>= k  = Shell $ \r -> do
      a <- m r
      unShell (k a) r
  fail msg = Shell $ \_ -> fail msg

instance MonadIO Shell where
  liftIO = Shell . const

instance MonadCatch Shell where
  catch (Shell m) c = Shell $ \r -> m r `MC.catch` \e -> unShell (c e) r

instance MonadThrow Shell where
  throwM e = liftIO (MC.throwM e)

instance MonadMask Shell where
  mask a = Shell $ \e -> MC.mask $ \u -> unShell (a $ q u) e
    where q u (Shell b) = Shell (u . b)
  uninterruptibleMask a =
    Shell $ \e -> MC.uninterruptibleMask $ \u -> unShell (a $ q u) e
      where q u (Shell b) = Shell (u . b)

-- | execute the Shell action in the IO monad
runShell :: Verbosity -> Shell a -> IO a
runShell verbosity (Shell sh) = do
  r <- CC.newMVar (InstallProgress [], verbosity)
  sh r

-- | wrapped version of forkIO in the Shell monad
forkIO' :: Shell () -> Shell CC.ThreadId
forkIO' (Shell f) = Shell $ \r -> CC.forkIO (f r)

modifyState :: (InstallProgress -> InstallProgress) -> Shell ()
modifyState f = Shell $ \r -> CC.modifyMVar_ r (\(x,y) -> return (f x, y))

withVerbosity :: (Verbosity -> IO a) -> Shell a
withVerbosity f = Shell $ \r -> do
  (_,v) <- CC.readMVar r
  f v

-- | Shell wrapper around `Distribution.Simple.Utils(info)`
info' :: String -> Shell ()
info' msg = do
  modifyState (\(InstallProgress msgs) -> InstallProgress (Info msg:msgs))
  withVerbosity (\v -> info v msg)

-- | Shell wrapper around `Distribution.Simple.Utils(notice)`
notice' :: String -> Shell ()
notice' msg = do
  modifyState (\(InstallProgress msgs) -> InstallProgress (Notice msg:msgs))
  withVerbosity (\v -> notice v msg)

-- | Shell wrapper around `Distribution.Simple.Utils(warn)`
warn' :: String -> Shell ()
warn' msg = do
  modifyState (\(InstallProgress msgs) -> InstallProgress (Warn msg:msgs))
  withVerbosity (\v -> warn v msg)

-- | Shell wrapper around `Distribution.Simple.Utils(debug)`
debug' :: String -> Shell ()
debug' msg = do
  modifyState (\(InstallProgress msgs) -> InstallProgress (Debug msg:msgs))
  withVerbosity (\v -> debug v msg)

-- | Shell wrapper around `Distribution.Simple.Utils(die)`
die' :: String -> Shell a
die' msg = do
  modifyState (\(InstallProgress msgs) -> InstallProgress (Die msg:msgs))
  liftIO (die msg)

catchIO' :: Shell a -> (Exception.IOException -> Shell a) -> Shell a
catchIO' = MC.catch

catchExit' :: Shell a -> (ExitCode -> Shell a) -> Shell a
catchExit' = MC.catch
