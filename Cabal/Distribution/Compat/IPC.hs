{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.IPC
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--


module Distribution.Compat.IPC (
  Semaphore(semaphoreName)

  -- * Low-level API.
  , createSemaphore, deleteSemaphore
  , openSemaphore, closeSemaphore
  , waitSemaphore, tryWaitSemaphore, releaseSemaphore

  -- * High-level API.
  ,withNewSemaphore,  withSemaphore
  ,withWaitSemaphore, withWaitGreedySemaphore )
       where

import Control.Exception    (bracket, bracket_)

#ifdef mingw32_HOST_OS

import Data.Bits            ((.|.))
import Data.Maybe           (fromMaybe)
import Foreign.Ptr          (Ptr)
import System.Win32.File    (LPSECURITY_ATTRIBUTES, closeHandle)
import System.Win32.Process (iNFINITE)
import System.Win32.Types   (BOOL, DWORD, HANDLE, LONG, LPCTSTR,
                             errorWin, failIf, failIfFalse_, failIfNull,
                             nullPtr, withTString)

data Semaphore = Semaphore {
  semaphoreName   :: FilePath,
  semaphoreHandle :: HANDLE
  }

foreign import stdcall "windows.h CreateSemaphoreW"
    c_CreateSemaphore :: LPSECURITY_ATTRIBUTES -> LONG -> LONG -> LPCTSTR
                         -> IO HANDLE

-- Can't use the name createSemaphore because it collides with our usage.
w32CreateSemaphore :: Maybe LPSECURITY_ATTRIBUTES -> LONG -> LONG -> String
                   -> IO HANDLE
w32CreateSemaphore mbLpSemaphoreAttributes lInitialCount lMaximumCount name =
    withTString name $ \c_name -> failIfNull "CreateSemaphore" $
        c_CreateSemaphore (fromMaybe nullPtr mbLpSemaphoreAttributes)
                          lInitialCount lMaximumCount c_name

foreign import stdcall "windows.h OpenSemaphoreW"
    c_OpenSemaphore :: DWORD -> BOOL -> LPCTSTR -> IO HANDLE

sYNCHRONIZE :: DWORD
sYNCHRONIZE = 0x00100000

sEMAPHORE_MODIFY_STATE :: DWORD
sEMAPHORE_MODIFY_STATE = 0x0002

w32OpenSemaphore :: String -> IO HANDLE
w32OpenSemaphore name =
    withTString name $ \c_name -> failIfNull "OpenSemaphore" $
        c_OpenSemaphore (sEMAPHORE_MODIFY_STATE .|. sYNCHRONIZE) False c_name

type LPLONG = Ptr LONG

foreign import stdcall "windows.h ReleaseSemaphore"
    c_ReleaseSemaphore :: HANDLE -> LONG -> LPLONG -> IO BOOL

w32ReleaseSemaphore :: HANDLE -> LONG -> IO ()
w32ReleaseSemaphore hSemaphore lReleaseCount =
    failIfFalse_ "ReleaseSemaphore" $
        c_ReleaseSemaphore hSemaphore lReleaseCount nullPtr

foreign import stdcall "windows.h WaitForSingleObject"
    c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD

wAIT_OBJECT_0 :: DWORD
wAIT_OBJECT_0 = 0x00000000

wAIT_FAILED :: DWORD
wAIT_FAILED = 0xFFFFFFFF

w32WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD
w32WaitForSingleObject handle dwMilliseconds =
    failIf (== wAIT_FAILED) "WaitForSingleObject" $
    c_WaitForSingleObject handle dwMilliseconds

foreign import stdcall "windows.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO DWORD

getCurrentProcessId :: IO DWORD
getCurrentProcessId = c_GetCurrentProcessId

-- | Create a new semaphore with a given initial value.
createSemaphore :: Int -> IO Semaphore
createSemaphore val = do
  pid <- getCurrentProcessId
  let name = "cabal-sem-" ++ (show pid)
  handle <- w32CreateSemaphore Nothing (toEnum val) (toEnum val) name
  return (Semaphore name handle)

-- | Delete a given semaphore.
deleteSemaphore :: Semaphore -> IO ()
deleteSemaphore =
    -- On Windows, a semaphore gets deleted automatically when all open handles
    -- to it are closed.
    closeHandle . semaphoreHandle

-- | Open an existing semaphore.
openSemaphore :: FilePath -> IO Semaphore
openSemaphore semName = do handle <- w32OpenSemaphore semName
                           return (Semaphore semName handle)

-- | Close an open semaphore handle. This does not delete the semaphore.
closeSemaphore :: Semaphore -> IO ()
closeSemaphore = closeHandle . semaphoreHandle

-- | Wait for a semaphore to become available, blocking if needed.
waitSemaphore :: Semaphore -> IO ()
waitSemaphore = wait . semaphoreHandle
  where
    wait handle = do dwResult <- w32WaitForSingleObject handle iNFINITE
                     if dwResult == wAIT_OBJECT_0
                       then return ()
                       else errorWin "WaitForSingleObject"

-- | Non-blocking variant of 'waitSemaphore'.
tryWaitSemaphore :: Semaphore -> IO Bool
tryWaitSemaphore = tryWait . semaphoreHandle
  where
    tryWait handle = do dwResult <- w32WaitForSingleObject handle 0
                        if dwResult == wAIT_OBJECT_0
                          then return True
                          else return False

-- | Increment the semaphore counter's value (previously decremented by
-- 'waitSemaphore' or 'tryWaitSemaphore').
releaseSemaphore :: Semaphore -> IO ()
releaseSemaphore = release . semaphoreHandle
  where
    release handle = do w32ReleaseSemaphore handle 1
                        closeHandle handle

#else

import System.Posix.Process   (getProcessID)
import System.Posix.Semaphore (OpenSemFlags(..),
                               semOpen, semUnlink, semPost, semWait, semTryWait)

import qualified System.Posix.Semaphore as Sem (Semaphore)

data Semaphore = Semaphore {
  semaphoreName   :: FilePath,
  semaphoreHandle :: Sem.Semaphore
  }

-- | Create a new semaphore with a given initial value.
createSemaphore :: Int -> IO Semaphore
createSemaphore val = do
  pid      <- getProcessID
  let name  = "/cabal-sem-" ++ (show pid)
      flags = OpenSemFlags { semCreate = True, semExclusive = True }
      perms = 0o755
  sem <- semOpen name flags perms val
  return (Semaphore name sem)

-- | Delete a given semaphore.
deleteSemaphore :: Semaphore -> IO ()
deleteSemaphore = semUnlink . semaphoreName

-- | Open an existing semaphore.
openSemaphore :: FilePath -> IO Semaphore
openSemaphore semName = do
  let flags = OpenSemFlags { semCreate = False, semExclusive = False}
      ignored0 = 0o755
      ignored1 = 0
  handle <- semOpen semName flags ignored0 ignored1
  return (Semaphore semName handle)

-- | Close an open semaphore handle. This does not delete the semaphore.
closeSemaphore :: Semaphore -> IO ()
closeSemaphore _ = return ()

-- | Wait for a semaphore to become available, blocking if needed.
waitSemaphore :: Semaphore -> IO ()
waitSemaphore = semWait . semaphoreHandle

-- | Non-blocking variant of 'waitSemaphore'.
tryWaitSemaphore :: Semaphore -> IO Bool
tryWaitSemaphore = semTryWait . semaphoreHandle

-- | Increment the semaphore counter's value (previously decremented by
-- 'waitSemaphore' or 'tryWaitSemaphore').
releaseSemaphore :: Semaphore -> IO ()
releaseSemaphore = semPost . semaphoreHandle

#endif

-- High-level API.

-- | Create a new semaphore, run the provided action, delete the semaphore.
withNewSemaphore :: Int -> (Semaphore -> IO a) -> IO a
withNewSemaphore n = bracket (createSemaphore n) (deleteSemaphore)

-- | Open a handle to an existing semaphore, run the provided action, close the
-- handle.
withSemaphore :: FilePath -> (Semaphore -> IO a) -> IO a
withSemaphore semName = bracket (openSemaphore semName) (closeSemaphore)

-- | Acquire the semaphore, run the provided action, release the semaphore.
withWaitSemaphore :: Semaphore -> IO a -> IO a
withWaitSemaphore sem = bracket_ (waitSemaphore sem) (releaseSemaphore sem)

-- | Acquire the maximum possible value from the semaphore (always at least 1),
-- run the provided action, release the semaphore.
withWaitGreedySemaphore :: Semaphore -> (Int -> IO a) -> IO a
withWaitGreedySemaphore sem = bracket (acquire) (release)
  where
    acquire = waitSemaphore sem >> go 1
      where
        go n = do b <- tryWaitSemaphore sem
                  if b then go (n+1) else return n

#ifdef mingw32_HOST_OS
    release n = w32ReleaseSemaphore (semaphoreHandle sem) (toEnum n)
#else
    release n | n <= 0    = return ()
              | otherwise = releaseSemaphore sem >> release (n-1)
#endif
