{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Distribution.Compat.CreatePipe (createPipe, tee) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, when)
import System.IO (Handle, hClose, hGetContents, hPutStr)

-- The mingw32_HOST_OS CPP macro is GHC-specific
#if mingw32_HOST_OS
import Control.Exception (onException)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff)
import GHC.IO.FD (mkFD)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.Handle.FD (mkHandleFromFD)
import System.IO (IOMode(ReadMode, WriteMode))
#else
import System.Posix.IO (fdToHandle)
import qualified System.Posix.IO as Posix
#endif

createPipe :: IO (Handle, Handle)
-- The mingw32_HOST_OS CPP macro is GHC-specific
#if mingw32_HOST_OS
createPipe = do
    (readfd, writefd) <- allocaArray 2 $ \ pfds -> do
        throwErrnoIfMinus1_ "_pipe" $ c__pipe pfds 2 ({- _O_BINARY -} 32768)
        readfd <- peek pfds
        writefd <- peekElemOff pfds 1
        return (readfd, writefd)
    (do readh <- fdToHandle readfd ReadMode
        writeh <- fdToHandle writefd WriteMode
        return (readh, writeh)) `onException` (close readfd >> close writefd)
  where
    fdToHandle :: CInt -> IOMode -> IO Handle
    fdToHandle fd mode = do
        (fd', deviceType) <- mkFD fd mode (Just (Stream, 0, 0)) False False
        mkHandleFromFD fd' deviceType "" mode False Nothing

    close :: CInt -> IO ()
    close = throwErrnoIfMinus1_ "_close" . c__close

foreign import ccall "io.h _pipe" c__pipe ::
    Ptr CInt -> CUInt -> CInt -> IO CInt

foreign import ccall "io.h _close" c__close ::
    CInt -> IO CInt
#else
createPipe = do
    (readfd, writefd) <- Posix.createPipe
    readh <- fdToHandle readfd
    writeh <- fdToHandle writefd
    return (readh, writeh)
#endif

-- | Copy the contents of the input handle to the output handles, like
-- the Unix command. The input handle is processed in another thread until
-- EOF is reached; 'tee' returns immediately. The 'Bool' with each output
-- handle indicates if it should be closed when EOF is reached.
-- Synchronization can be achieved by blocking on an output handle.
tee :: Handle -- ^ input
    -> [(Handle, Bool)] -- ^ output, close?
    -> IO ()
tee inH outHs = do
    -- 'hGetContents' might cause text decoding errors on binary streams that
    -- are not text. It might be better to read into a buffer with 'hGetBuf'
    -- that does no text decoding, but that seems to block all threads on
    -- Windows. This is much simpler.
    str <- hGetContents inH
    forM_ outHs $ \(h, close) -> forkIO $ do
        hPutStr h str
        when close $ hClose h
