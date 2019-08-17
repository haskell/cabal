{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE NondecreasingIndentation #-}

-- | Support for linearized logging
--
module Distribution.Client.ProjectLogging
    ( LogHandle -- abstract
    , newLogHandleMap
    , openLogHandle
    , withLogHandle
    , getLogHandle
    , closeLogHandle
    ) where

import           Distribution.Client.ProjectPlanning
import           Distribution.Client.ProjectBuilding.Types
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.Types (GenericReadyPackage(..))
import           Distribution.Package
import           Distribution.Utils.Generic (ordNub)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad
import           Control.Exception (try, throwIO, assert)
import           Control.Concurrent
import           Control.Concurrent.Async
import           System.IO
import           System.IO.Error
import           System.Posix.IO hiding (createPipe)
import           System.Posix.Terminal
import           GHC.IO.Exception

data LogHandleState
    = Buffering
    | Forwarding
    | Closed
      deriving (Eq, Ord, Read, Show)

data LogHandleVar = LogHandleVar
    { lhState  :: LogHandleState
    , lhTTY    :: Handle
    , lhFile   :: Handle
    , lhMaster :: Handle
    , lhSlave  :: Handle
    , lhFilePath :: FilePath
    , lhAsync :: Async ()
    } deriving (Eq)

data LogHandle = LogHandle
    { _lhUnitId :: UnitId
    , lhNext   :: Maybe LogHandle
    , lhFirst  :: Bool
    , lhVar    :: MVar LogHandleVar
    } deriving (Eq)

type LogHandleMap = Map UnitId LogHandle
newLogHandleMap :: BuildStatusMap -> ElaboratedInstallPlan -> IO LogHandleMap
newLogHandleMap pkgsBuildStatus installPlan = fmap Map.fromList $ do
    foldM newLogHandle [] (reverse (uids `zip` (True : repeat False)))
  where
    newLogHandle lhds (uid, first) = do
      mv <- newEmptyMVar
      return $ (uid, LogHandle uid (snd <$> headMay lhds) first mv) : lhds

    headMay [] = Nothing
    headMay (x:_) = Just x

    -- All the units that need building. If we get this wrong we'll hang
    -- forever in passForwarding as the next LogHandle will never be opened
    -- so watch out.
    uids = assert (ordNub uids' == uids') uids'
    uids' =
      [ uid
      | ReadyPackage elab
         <- InstallPlan.executionOrder installPlan
      , let uid = installedUnitId elab
            pkgBuildStatus = Map.findWithDefault (error "newLogHandleMap uid not found") uid pkgsBuildStatus
      , buildStatusRequiresBuild pkgBuildStatus
      ]

openLogHandle :: LogHandle -> FilePath -> Handle -> IO ()
openLogHandle lh@LogHandle{lhVar=mv, lhFirst} logFile ttyhdl = do
    mlhv <- tryTakeMVar mv
    putMVar mv =<< case fmap lhState mlhv of
      Nothing -> do
        filehdl <- openFile logFile ReadWriteMode
        (amux, master, slave) <- newLogMuxThread lh
        return $ LogHandleVar
          { lhState = if lhFirst then Forwarding else Buffering
          , lhTTY  = ttyhdl
          , lhFile = filehdl
          , lhMaster = master
          , lhSlave = slave
          , lhFilePath = logFile
          , lhAsync = amux
          }
      Just Buffering -> do
        error "openLogFile: already buffering!"
      Just Forwarding ->
        error "openLogFile: already forwarding!"
      Just Closed ->
        error "openLogFile: already closed!"

withLogHandle :: LogHandle -> (Handle -> IO a) -> IO a
withLogHandle lh action = action =<< getLogHandle lh

getLogHandle :: LogHandle -> IO Handle
getLogHandle LogHandle{lhVar=mv} = do
    lhv <- readMVar mv
    when (lhState lhv == Closed) $
      error "withLogHandle: already closed!"
    return (lhSlave lhv)

closeLogHandle :: LogHandle -> IO ()
closeLogHandle lh@LogHandle{lhVar=mv} = do
    amux <- withMVar mv $ \lhv -> do
       when (lhState lhv == Closed) $
         error "closeLogHandle: already closed!"
       hClose $ lhSlave lhv -- signal mux thread to exit
       return $ lhAsync lhv
    wait amux

    -- now we can close the rest without breaking the mux thread
    modifyMVar_ mv $ \lhv -> do
      hClose $ lhFile lhv
      when (lhState lhv == Forwarding) $
        void $ forkIO $ passForwarding lh
      return lhv { lhState = Closed }

  where
    drain whdl rhdl = do
      ebuf <- BS.hGetSome rhdl 4096
      case ebuf of
        buf | BS.null buf -> return ()
        buf -> do
          BS.hPut whdl buf
          drain whdl rhdl

    passForwarding LogHandle{lhNext=Nothing} =
      return ()
    passForwarding LogHandle{lhNext=Just nlh} = do
      let nmv = lhVar nlh
      modifyMVar_ nmv $ \nlhv -> do -- waits until openend
        case lhState nlhv of
          Forwarding ->
            error $ "passForwarding: next handle already forwarding!?"
          Closed -> do
            LBS.hPut (lhTTY nlhv) =<< LBS.readFile (lhFilePath nlhv)
            passForwarding nlh
            return nlhv
          Buffering -> do
            hSeek (lhFile nlhv) AbsoluteSeek 0
            drain (lhTTY nlhv) (lhFile nlhv)
            hSeek (lhFile nlhv) SeekFromEnd 0
            return $ nlhv { lhState = Forwarding }

newLogMuxThread :: LogHandle -> IO (Async (), Handle, Handle)
newLogMuxThread LogHandle{lhVar=mv} = do
--    (rhdl, whdl) <- createPipe -- pipes are nice and portable but buffer
--    the output like crazy. There really is no way around ptys for smooth
--    output -- at least that I can find.
    (master, slave) <- openPseudoTerminal
    rhdl <- fdToHandle master
    whdl <- fdToHandle slave
    amux <- async $ loop rhdl
    return (amux, rhdl, whdl)
  where
    loop rhdl = do
      ebuf <- try $ BS.hGetSome rhdl 512
      case ebuf of
        -- pty master will throw EIO on slave close (at least on linux)
        Left err | ioeGetErrorType err == HardwareFault -> do
          hClose rhdl

        -- BS.hGetSome will usually signal EOF of a pipe via zero buffer
        -- size. For pty masters this doesn't happen, see below. "Some
        -- platforms" might still do it this way (ugh).
        Right buf | BS.null buf -> do
          hClose rhdl

        Left err -> throwIO err
        Right buf -> do
          LogHandleVar{lhState, lhTTY, lhFile} <- readMVar mv
          case lhState of
            Forwarding -> do
              BS.hPut lhTTY buf
              BS.hPut lhFile buf
            Buffering  ->
              BS.hPut lhFile buf
            Closed ->
              error "newLogMuxThread.loop: handle closed prematurely"
          loop rhdl
