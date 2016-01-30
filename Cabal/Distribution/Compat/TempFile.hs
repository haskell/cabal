{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Distribution.Compat.TempFile (
  openTempFile,
  openBinaryTempFile,
  openNewBinaryFile,
  createTempDirectory,
  ) where


import System.FilePath        ((</>))
import Foreign.C              (eEXIST)

import System.IO              (Handle, openTempFile, openBinaryTempFile)
import Data.Bits              ((.|.))
import System.Posix.Internals (c_open, c_close, o_CREAT, o_EXCL, o_RDWR,
                               o_BINARY, o_NONBLOCK, o_NOCTTY)
import System.IO.Error        (isAlreadyExistsError)
import System.Posix.Internals (withFilePath)
import Foreign.C              (CInt)
import GHC.IO.Handle.FD       (fdToHandle)
import Distribution.Compat.Exception (tryIO)
import Control.Exception      (onException)
import Foreign.C              (getErrno, errnoToIOError)

import System.Posix.Internals (c_getpid)

#ifdef mingw32_HOST_OS
import System.Directory       ( createDirectory )
#else
import qualified System.Posix
#endif

-- ------------------------------------------------------------
-- * temporary files
-- ------------------------------------------------------------

-- This is here for Haskell implementations that do not come with
-- System.IO.openTempFile. This includes nhc-1.20, hugs-2006.9.
-- TODO: Not sure about jhc

-- This is a copy/paste of the openBinaryTempFile definition, but
-- if uses 666 rather than 600 for the permissions. The base library
-- needs to be changed to make this better.
openNewBinaryFile :: FilePath -> String -> IO (FilePath, Handle)
openNewBinaryFile dir template = do
  pid <- c_getpid
  findTempName pid
  where
    -- We split off the last extension, so we can use .foo.ext files
    -- for temporary files (hidden on Unix OSes). Unfortunately we're
    -- below filepath in the hierarchy here.
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
         -- TODO: We want to tell fdToHandle what the filepath is,
         -- as any exceptions etc will only be able to report the
         -- fd currently
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

-- FIXME: Should use filepath library
pathSeparator :: Char
#ifdef mingw32_HOST_OS
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif

-- FIXME: Copied from GHC.Handle
std_flags, output_flags, rw_flags :: CInt
std_flags    = o_NONBLOCK   .|. o_NOCTTY
output_flags = std_flags    .|. o_CREAT
rw_flags     = output_flags .|. o_RDWR

createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let dirpath = dir </> template ++ "-" ++ show x
      r <- tryIO $ mkPrivateDir dirpath
      case r of
        Right _ -> return dirpath
        Left  e | isAlreadyExistsError e -> findTempName (x+1)
                | otherwise              -> ioError e

mkPrivateDir :: String -> IO ()
#ifdef mingw32_HOST_OS
mkPrivateDir s = createDirectory s
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif
