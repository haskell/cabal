{-# LANGUAGE TemplateHaskell #-}
module OK where

import Data.List
import System.Process
import Language.Haskell.TH

$(do
    out <- runIO $ readProcess "mybuilder" [] ""
    if "0.2.0.0" `isInfixOf` out then
       [d| x = () |]
    else
      error ("Expecting Version 0.2.0.0, but got: " ++ out)
 )
