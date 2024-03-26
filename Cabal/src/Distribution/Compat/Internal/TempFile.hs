{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Internal.TempFile
  ( openTempFile
  , openBinaryTempFile
  , openNewBinaryFile
  , createTempDirectory
  ) where

import Distribution.Compat.Exception

import System.FilePath ((</>))

import System.IO (Handle, openBinaryTempFile, openTempFile)
#if defined(__IO_MANAGER_WINIO__)
import System.IO              (openBinaryTempFileWithDefaultPermissions)
#else
import Control.Exception      (onException)
import Data.Bits              ((.|.))
import Foreign.C              (CInt, eEXIST, getErrno, errnoToIOError)
import GHC.IO.Handle.FD       (fdToHandle)
import System.Posix.Internals (c_open, c_close, o_EXCL, o_BINARY, withFilePath,
                               o_CREAT, o_RDWR, o_NONBLOCK, o_NOCTTY)
#endif

import System.IO.Error (isAlreadyExistsError)
import System.Posix.Internals (c_getpid)

#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
import System.Directory       ( createDirectory )
#else
import qualified System.Posix
#endif

-- ------------------------------------------------------------

-- * temporary files

-- ------------------------------------------------------------

-- This is here for Haskell implementations that do not come with
-- System.IO.openTempFile. This includes nhc-1.20, hugs-2006.9.
-- TODO: This file should probably be removed.

-- This is a copy/paste of the openBinaryTempFile definition, but
-- it uses 666 rather than 600 for the permissions. Newer versions
-- of base have a new function with this behavior which we use on
-- Windows when the new IO manager is used.
openNewBinaryFile :: FilePath -> String -> IO (FilePath, Handle)
openNewBinaryFile dir template = do

-- This method can't be used under WINIO. Also the current implementation has
-- thread safety issues depending on which GHC is used.  On newer GHC's let's
-- use the built in one.
#if defined(__IO_MANAGER_WINIO__)
  openBinaryTempFileWithDefaultPermissions dir template
#else
  pid <- c_getpid
  findTempName pid
  where
    -- We split off the last extension, so we can use .foo.ext files
    -- for temporary files (hidden on Unix OSes). Unfortunately we're
    -- below file path in the hierarchy here.
    (prefix,suffix) =
       case break (== '.') $ reverse template of
         -- First case: template contains no '.'s. Just re-reverse it.
         (rev_suffix, "")       -> (reverse rev_suffix, "")
         -- Second case: template contains at least one '.'. Strip the
         -- dot from the prefix and prepend it to the suffix (if we don't
         -- do this, the unique number will get added after the '.' and
         -- thus be part of the extension, which is wrong.)
         (rev_suffix, '.':rest) -> (reverse rest, '.':reverse rev_suffix)
         -- Otherwise, something is wrong, because (break (== '.')) should
         -- always return a pair with either the empty string or a string
         -- beginning with '.' as the second component.
         _                      -> error "bug in System.IO.openTempFile"

    oflags = rw_flags .|. o_EXCL .|. o_BINARY

    findTempName x = do
      fd <- withFilePath filepath $ \ f ->
              c_open f oflags 0o666
      if fd < 0
       then do
         errno <- getErrno
         if errno == eEXIST
           then findTempName (x+1)
           else ioError (errnoToIOError "openNewBinaryFile" errno Nothing (Just dir))
       else do
         -- TODO: We want to tell fdToHandle what the file path is,
         -- as any exceptions etc will only be able to report the
         -- FD currently
         h <- fdToHandle fd `onException` c_close fd
         return (filepath, h)
      where
        filename        = prefix ++ show x ++ suffix
        filepath        = dir `combine` filename

        -- FIXME: bits copied from System.FilePath
        combine a b
                  | null b = a
                  | null a = b
                  | last a == pathSeparator = a ++ b
                  | otherwise = a ++ [pathSeparator] ++ b

-- FIXME: Copied from GHC.Handle
std_flags, output_flags, rw_flags :: CInt
std_flags    = o_NONBLOCK   .|. o_NOCTTY
output_flags = std_flags    .|. o_CREAT
rw_flags     = output_flags .|. o_RDWR

-- FIXME: Should use System.FilePath library
pathSeparator :: Char
#ifdef mingw32_HOST_OS
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif
-- /* __IO_MANAGER_WINIO__ */
#endif

createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let relpath = template ++ "-" ++ show x
          dirpath = dir </> relpath
      r <- tryIO $ mkPrivateDir dirpath
      case r of
        Right _ -> return relpath
        Left e
          | isAlreadyExistsError e -> findTempName (x + 1)
          | otherwise -> ioError e

mkPrivateDir :: String -> IO ()
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
mkPrivateDir s = createDirectory s
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif
