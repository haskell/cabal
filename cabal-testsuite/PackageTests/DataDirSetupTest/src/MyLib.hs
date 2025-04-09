module MyLib (getDataFileName) where

import qualified Paths_datadir_test as Paths

getDataFileName :: FilePath -> IO FilePath
getDataFileName = Paths.getDataFileName
