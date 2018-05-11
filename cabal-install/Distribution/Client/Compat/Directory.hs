{-# LANGUAGE CPP #-}
module Distribution.Client.Compat.Directory (setModificationTime) where

#if MIN_VERSION_directory(1,2,3)
import System.Directory (setModificationTime)
#else

import Data.Time.Clock (UTCTime)

setModificationTime :: FilePath -> UTCTime -> IO ()
setModificationTime _fp _t = return ()

#endif
