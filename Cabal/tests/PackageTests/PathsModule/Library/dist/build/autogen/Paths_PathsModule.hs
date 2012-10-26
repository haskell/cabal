module Paths_PathsModule (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(Version, versionBranch, versionTags))
import System.Environment (getEnv)
import Prelude ((++), FilePath, IO, return)

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/tibbe/.cabal/bin"
libdir     = "/Users/tibbe/.cabal/lib/PathsModule-0.1/ghc-7.4.1"
datadir    = "/Users/tibbe/.cabal/share/PathsModule-0.1"
libexecdir = "/Users/tibbe/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "PathsModule_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PathsModule_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PathsModule_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PathsModule_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
