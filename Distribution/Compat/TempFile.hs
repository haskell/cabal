{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-- #hide
module Distribution.Compat.TempFile (openTempFile) where

#if __NHC__ || __HUGS__
import System.IO              (openFile, Handle, IOMode(ReadWriteMode))
import System.Directory       (doesFileExist)
import System.FilePath        ((</>), (<.>), splitExtension)
#if __NHC__
import System.Posix.Types (CPid(..))
foreign import ccall unsafe "getpid" c_getpid :: IO CPid
#else
import System.Posix.Internals (c_getpid)
#endif
#else
import System.IO (openTempFile)
#endif

-- ------------------------------------------------------------
-- * temporary files
-- ------------------------------------------------------------

-- This is here for Haskell implementations that do not come with
-- System.IO.openTempFile. This includes nhc-1.20, hugs-2006.9.
-- TODO: Not sure about jhc

#if __NHC__ || __HUGS__
-- use a temporary filename that doesn't already exist.
-- NB. *not* secure (we don't atomically lock the tmp file we get)
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile tmp_dir template
  = do x <- getProcessID
       findTempName x
  where
    (templateBase, templateExt) = splitExtension template
    findTempName :: Int -> IO (FilePath, Handle)
    findTempName x
      = do let path = tmp_dir </> (templateBase ++ show x) <.> templateExt
           b  <- doesFileExist path
           if b then findTempName (x+1)
                else do hnd <- openFile path ReadWriteMode
                        return (path, hnd)

    getProcessID :: IO Int
    getProcessID = fmap fromIntegral c_getpid
#endif
