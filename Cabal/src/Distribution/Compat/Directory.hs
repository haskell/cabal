{-# LANGUAGE CPP #-}

module Distribution.Compat.Directory
  ( listDirectory
  , makeAbsolute
  , doesPathExist
  ) where

#if MIN_VERSION_directory(1,2,7)
import System.Directory as Dir hiding (doesPathExist)
import System.Directory (doesPathExist)
#else
import System.Directory as Dir
#endif
#if !MIN_VERSION_directory(1,2,2)
import System.FilePath as Path
#endif

#if !MIN_VERSION_directory(1,2,5)

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f `fmap` Dir.getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

#endif

#if !MIN_VERSION_directory(1,2,2)

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p | Path.isAbsolute p = return p
               | otherwise         = do
    cwd <- Dir.getCurrentDirectory
    return $ cwd </> p

#endif

#if !MIN_VERSION_directory(1,2,7)

doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    -- not using Applicative, as this way we can do less IO
    e <- doesDirectoryExist path
    if e
    then return True
    else doesFileExist path

#endif
