{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Distribution.Compat.CreatePipe (createPipe) where

import System.IO (Handle)

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
import System.Posix.IO (fdToHandle)
import qualified System.Posix.IO as Posix
#else
# include <io.h>        /* for _pipe */
# include <fcntl.h>     /* for _O_BINARY */
import Control.Exception (onException)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(CInt), CUInt(CUInt))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff)
import GHC.IO.FD (mkFD)
import GHC.IO.Device (close)
import GHC.IO.Handle.FD (mkHandleFromFD)
import System.IO (IOMode(ReadMode, WriteMode))
#endif

createPipe :: IO (Handle, Handle)
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
createPipe = do
    (readfd, writefd) <- Posix.createPipe
    readh <- fdToHandle readfd
    writeh <- fdToHandle writefd
    return (readh, writeh)
#else
createPipe = do
   (readfd, writefd) <- allocaArray 2 $ \ pfds -> do
      throwErrnoIfMinus1_ "_pipe" $ c__pipe pfds 2 (#const _O_BINARY)
      readfd <- peek pfds
      writefd <- peekElemOff pfds 1
      return (readfd, writefd)
   (readFD, readDeviceType) <- mkFD readfd ReadMode Nothing False False
   readh <- mkHandleFromFD readFD readDeviceType "" ReadMode False Nothing
       `onException` close readFD
   (writeFD, writeDeviceType) <- mkFD writefd WriteMode Nothing False False
   writeh <- mkHandleFromFD writeFD writeDeviceType "" WriteMode False Nothing
       `onException` close writeFD
   return (readh, writeh)

foreign import ccall "io.h _pipe" c__pipe ::
    Ptr CInt -> CUInt -> CInt -> IO CInt
#endif
