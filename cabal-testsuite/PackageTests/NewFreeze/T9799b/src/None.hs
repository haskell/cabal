{-# LANGUAGE TemplateHaskell #-}
module None where

import MyLib
import Language.Haskell.TH
import System.IO

$(do
    runIO $ do
        putStrLn $ "Building: " ++ renamedVers
        hFlush stdout
    [d| x = () |]
 )
