{-# LANGUAGE TemplateHaskell #-}
module None where

import MyLib
import Language.Haskell.TH

$(do
    runIO $ putStrLn $ "Building: " ++ renamedVers
    [d| x = () |]
 )
