{-# LANGUAGE CPP #-}

module Distribution.Compat.Directory (listDirectory, makeAbsolute) where

import System.Directory as Dir
#if !MIN_VERSION_directory(1,2,2)
import System.FilePath as Path
#endif

#if !MIN_VERSION_directory(1,2,5)

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> Dir.getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

#endif

#if !MIN_VERSION_directory(1,2,2)

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p | Path.isAbsolute p = return p
               | otherwise         = do
    cwd <- Dir.getCurrentDirectory
    return $ cwd </> p

#endif
