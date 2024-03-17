{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module HooksLib where

import Control.Monad.Reader

import V01.Lib as L1
import V02.Lib as L2
import "lib01" Lib as L

import System.IO (stderr)
import System.Process
import Control.Exception as C
import Data.Binary
import Control.Concurrent
import GHC.IO.Handle (BufferMode(..), hClose, hSetBuffering)
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Control.Exception as C
import Data.ByteString.Lazy (hPutStr, hGetContents)
import qualified Data.ByteString.Char8 as C8
import Control.DeepSeq
import GHC.IO.Exception as GHC
import Foreign.C.Error

data SupportedVersion = V01 | V02

--import Lib as LX

data HooksEnv = HooksEnv { hooksExe :: FilePath,  hooksVersion :: SupportedVersion }

type HooksM a = ReaderT HooksEnv IO a

version :: FilePath -> IO (Maybe SupportedVersion)
version fp = do
  (ver :: String) <- readHooksExe_ fp "version" ()
  print ver
  return $ case C8.split '.' (C8.pack ver) of
    ["0","1","0","0"] -> Just V01
    ["0","2","0","0"] -> Just V02
    _ -> Nothing

withHooksExe :: FilePath -> HooksM a -> IO a
withHooksExe prog action = do
  ver <- version prog
  case ver of
    Just sver -> runReaderT action (HooksEnv prog sver)
    Nothing -> error "Hooks executable is unsupported version"

convertA1 :: L.A -> L1.A
convertA1 (L.A {}) = L1.A
convertA1 (L.B) = L1.B

convertA2 :: L.A -> L2.A
convertA2 (L.A x) = (L2.A x)
convertA2 L.B = L2.B

revertA1 :: L1.A -> L.A
revertA1 L1.A = L.A 0
revertA1 L1.B = L.B

revertA2 :: L2.A -> L.A
revertA2 (L2.A x) = L.A x
revertA2 L2.B = L.B


hooks_show :: L.A -> HooksM String
hooks_show a = do
  ver <- asks hooksVersion
  case ver of
    V01 -> readHooksExe "show" (convertA1 a)
    V02 -> readHooksExe "show" (convertA2 a)

hooks_inc :: L.A -> HooksM L.A
hooks_inc a = do
  ver <- asks hooksVersion
  case ver of
    V01 -> revertA1 <$> (readHooksExe "inc" (convertA1 a))
    V02 -> revertA2 <$> (readHooksExe "inc" (convertA2 a))




-- Library funcs


readHooksExe :: (Binary a, Binary b) => String -> a -> HooksM b
readHooksExe hook args = do
  exe <- asks hooksExe
  liftIO $ readHooksExe_ exe hook args

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

readHooksExe_ :: (Binary a, Binary b) => FilePath -> String -> a -> IO b
readHooksExe_ exe hook args =
  let stdin_ = encode args
      cp = (proc exe [hook]) { std_in = CreatePipe
                             , std_out = CreatePipe }
  in withCreateProcess cp $ \(Just inh) (Just outh) Nothing ph -> do
      -- fork off a thread to start consuming the output
        hSetBuffering inh NoBuffering
        hSetBuffering outh NoBuffering
        output  <- hGetContents outh
        withForkWait (C.evaluate $ rnf output) $ \waitOut -> do

          hPutStr inh stdin_
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          hClose outh

        -- wait on the process
        _ex <- waitForProcess ph
        return (decode output)



-- | Ignore SIGPIPE in a subcomputation.
ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \case
  GHC.IOError{GHC.ioe_type = GHC.ResourceVanished, GHC.ioe_errno = Just ioe}
    | Errno ioe == ePIPE -> return ()
  e -> throwIO e
